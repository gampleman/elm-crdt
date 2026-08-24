# 09 — Fugue ordering (kill same-position interleaving)

**Status:** ✅ done — `Crdt.Rga` now orders by Fugue (`parent` + `Side`), threaded
through `OpLog`/`Json`/`OpJson`/`OpDoc`. `Crdt.OpDoc.applyTextDiff` anchors runs by
the Fugue rule (`fuguePlacement`/`descendsFrom`). Tested by `tests/FugueTests.elm`
(10 — headline no-interleaving property + RGA-would-interleave contrast + `side=Left`
ordering units) and the existing RGA/convergence suites (241 total / 0 fail).
Prereq for rich text (`design-docs/10`, planned).
**Roadmap item:** #3 (pulled ahead of rich text — see "Why now" below).
**Depends on / touches:** `Crdt.Rga` (element model + ordering), `Crdt.Json` &
`Crdt.OpJson` (wire), `Crdt.OpDoc` (`applyTextDiff` insert anchoring), `Crdt.MoveList`
(shares `Rga` for its cells — must stay correct).

## Problem

Our sequences (lists **and** text) order by **RGA**: each element stores a single
left anchor `origin : Maybe OpId`, and `Rga.toElementsInOrder` walks the insertion
forest from the head, ordering siblings that share an origin by id **descending**.

RGA converges, but it has a well-known merge-*quality* flaw: **concurrent insertions
at the same position interleave character-by-character.** If Alice types `abc` and
Bob types `xyz` at the same cursor spot (both anchored after the same element),
RGA can converge to `axybcz`-style interleavings — each replica's run is broken up
by id-tiebreak at every character. Correctness holds (everyone agrees on the
garble); readability does not.

This is purely a text problem in practice (lists rarely get concurrent same-anchor
inserts, and when they do, item-granular interleaving is far less jarring than
character-granular). But `Rga` backs both, so we fix it once at the ordering layer.

### Why now, before rich text

Rich text adds a **marks/annotations layer keyed by character `OpId`** (Peritext
style) plus its own wire encoding, layered *on top of* the character sequence. The
marks layer is agnostic to the ordering algorithm — a mark referencing char ids
survives an ordering swap untouched. But the **character element model and its wire
format are not** agnostic: Fugue needs more per-element data than RGA (below), which
is a `Crdt.Rga.Element` change **and** a `Crdt.Json`/`Crdt.OpJson` migration.

If we shipped rich text first, we'd migrate the text element wire format **twice** —
once for marks, once for Fugue — the second time underneath a marks layer. Doing
Fugue first stabilizes the substrate once, and lets rich-text mark-range tests
exercise the *final* contiguous-run behavior rather than RGA interleaving artifacts
we're about to delete. Fugue is also small, self-contained, and independently
fuzz-testable, so it's a clean reviewable diff on its own.

## Why RGA interleaves and Fugue doesn't (the one idea)

RGA gives each element **one** anchor (its left origin) and resolves same-anchor
siblings by a global id tiebreak. Two concurrent runs share the same left origin,
so *every* character of both runs is a same-anchor sibling of the other run's
characters — the id tiebreak shuffles them together.

**Fugue** (Weidner, Toomim, et al., "The Art of the Fugue", 2023) gives each element
a **left origin _and_ a right origin** — the two elements it was inserted *between*
at creation time — and orders by a rule that keeps a run **contiguous**: an element
inserted between L and R attaches to whichever of L/R is "closer" in the tree, and
concurrent runs that were inserted between the same L/R go entirely-left or
entirely-right of each other as a block (decided once, by id, for the whole run),
instead of interleaving per character.

Equivalently (the formulation we'll implement): each element records **one** parent
anchor plus a **side** bit (`Left`/`Right`) saying whether it hangs to the left or
right of that parent. This is the "Fugue as a tree" encoding, and it's the minimal
delta from what we already store:

- RGA element: `{ id, origin : Maybe OpId, content, deleted }` — origin = left anchor.
- Fugue element: `{ id, parent : Maybe OpId, side : Side, content, deleted }`
  where `Side = Left | Right`.

`side = Right` with `parent = L` means "immediately right of L" (this is exactly
today's `origin = Just L`). `side = Left` with `parent = R` means "immediately left
of R" — the case RGA can't express, and the reason concurrent runs stay contiguous.

## The change, layer by layer

### 1. `Crdt.Rga.Element` (data model)

```elm
type Side = Left | Right

type alias Element c =
    { id : OpId
    , parent : Maybe OpId   -- was `origin`
    , side : Side           -- NEW
    , content : c
    , deleted : Bool
    }
```

**Decided:** rename `origin → parent` (the semantics genuinely change — it is no
longer "the element I come after" — and it matches Fugue's vocabulary). This is the
larger diff, touching every `Rga.element`/`.origin` reference across `Node`,
`MoveList`, `OpDoc`, `Json`, `OpJson`, but it keeps the code honest.

Keeping the *type* name `Rga` (it's still a replicated growable array; Fugue is the
ordering discipline) avoids churn on the module name across `Node`, `MoveList`,
`OpDoc`, and the docs. We can rename to `Crdt.Seq` later if desired — out of scope
here.

### 2. Ordering (`toElementsInOrder`) — the heart

Replace the origin-forest walk with the Fugue traversal. The tree is: every element
is a child of its `parent`, on its `side`. Left-children of a node come before the
node; right-children come after. Within one (parent, side) group, **concurrent
siblings order by id descending** (same tiebreak rule as today — this is what makes
distinct concurrent runs pick a total block order deterministically).

Recursive shape (to be implemented with an explicit stack, per our stack-safety
rule — see the RGA 20k regression in `tests/RgaTests.elm`):

```
render(node):
    for each left-child L of node, by id desc:   render(L)
    emit node
    for each right-child R of node, by id desc:  render(R)
```

Roots = elements whose `parent` is `Nothing` (head) or points at a missing id
(dangling — never drop, per today's robustness rule). The cycle-sweep tail (pick
lowest unvisited id as an extra root until all elements appear) stays exactly as it
is — it's an adversarial-input guard, orthogonal to Fugue.

**Determinism / convergence:** order is still a pure function of the element set
`{id, parent, side}`. Same set → same tree → same traversal on every replica,
independent of merge order. This is the invariant the fuzz laws already assert; it
must keep holding.

### 3. Insert anchoring (`Crdt.OpDoc.applyTextDiff` and list appends)

Today an insert captures only `startOrigin` (the visible element **before** the
gap) and chains new chars left-to-right, each `after` the previous. Fugue needs the
gap's **right** neighbor too, and picks parent+side by the Fugue rule:

- Let `L` = visible element left of the gap (`Nothing` at head),
  `R` = visible element right of the gap (`Nothing` at tail).
- New element attaches as a child of **whichever of L, R is deeper in the tree**
  (the standard Fugue "closer anchor" rule); side is `Right` of L (if we attach to
  L) or `Left` of R (if we attach to R). At the head, `parent = Nothing, side =
  Right`; at the tail, attach `Right` of L as today.
- A **run** of inserted chars still chains: char₂ attaches `Right` of char₁, etc.
  The first char is the only one that consults L/R. This is what keeps a whole
  typed run together and, crucially, keeps a *concurrent* run on one side of it.

`InsertElem` op grows a field (see wire) so the right anchor / side is transmitted.
Materialization (`OpLog.insertElem` → `Rga.put`) just stores the element; ordering
does the rest, so the fold stays O(1) per op.

### 4. Merge (`Rga.mergeElement` / `mergeOrigin`)

Same shape as today: union by id; tombstone OR; content merged recursively. The
`mergeOrigin` deterministic-conflict resolver becomes a `mergeAnchor` over
`(parent, side)` — in normal use an id is minted once so both copies agree; the
resolver only exists to keep merge commutative on corrupt/disagreeing input, so a
fixed rule (larger parent id wins; `Right` beats `Left` on tie) suffices.

### 5. Wire format (`Crdt.Json`, `Crdt.OpJson`) — **breaking**

RGA element JSON today: `{ id, o: origin|null, c, d }`. Fugue:
`{ id, p: parent|null, s: "L"|"R", c, d }`. `InsertElem` op today carries
`after : origin`; it becomes `parent + side`.

This is a **breaking wire change**. **Decided: clean break.** Bump the format
version tag; old snapshots don't load. Justified — the library is pre-1.0, there is
no persisted production data, and the demo regenerates its state. No legacy decode
branch (we can add the trivial `{o} → parent+Right` mapping later if a
back-compat need ever appears, since old data *is* valid all-`Right` Fugue).

## What does NOT change

- Public API (`Crdt.OpDoc`, `Crdt.Schema`, `Crdt.Ref`, `Crdt.Cursor`) — untouched.
- Stable cursors — they anchor to element `OpId`s and count live elements in order
  (`liveCountThrough`); the *order* changes but the mechanism doesn't.
- `Crdt.MoveList` — its cells are an `Rga OpId`; concurrent same-anchor cell inserts
  are rare and item-granular, but it rides the same fixed ordering for free.
- Tombstones, GC/`compact`, delta sync, undo/redo — all operate on element sets and
  ids, not on the ordering rule.
- Incremental cache (`OpDoc.cached`) — still valid; ordering is still a pure
  function of the set, so a local op's effect is unchanged.

## Test plan

Core (`tests/RgaTests.elm`, extended) and a focused `tests/FugueTests.elm`:

1. **The headline property — no interleaving.** Two replicas insert a multi-char run
   at the same anchor from a shared base; merge both orders. Assert the result is
   one run entirely before the other (`aaabbb` or `bbbaaa`), **never** interleaved,
   and identical in both merge orders. This is the test that fails on RGA today and
   must pass on Fugue. Fuzz over run lengths and anchor positions.
2. **Convergence laws still hold** — commutativity / associativity / idempotence of
   `merge` over a depth-bounded `Fuzzer Element`, same harness as RGA today.
3. **Ordering is merge-order-independent** — random merge interleavings of the same
   element set give an identical `toList` (existing RGA property, must survive).
4. **Backward semantic equivalence** — an all-`side=Right` element set orders
   identically to the old RGA on the same `{id, parent}` (proves Fugue is a strict
   generalization; also validates the option-B legacy decode if we take it).
5. **Stack safety** — the 20k-append origin-chain regression, retargeted to the
   Fugue traversal (explicit stack, no overflow).
6. **Cursor stability across a concurrent same-anchor insert** — a caret survives a
   peer's concurrent run insertion at the caret's anchor and lands at the right
   offset (property over random edit interleavings), now that the run stays
   contiguous.
7. **End-to-end through `OpDoc`** — `applyTextDiff` on two peers typing at the same
   offset converges to contiguous runs; delta-synced and full-state-synced paths
   agree.

## Risks / open questions

- **Traversal cost.** Fugue's tree walk is the same asymptotics as the RGA forest
  walk (O(N) with an explicit stack). Re-benchmark against `benchmarks/` to confirm
  no regression on the read path; the edit-path append fast-path (`lastAppend`) still
  applies (append = `Right` of last visible, cache unchanged).
- **The "closer anchor" rule** is the one subtle part; I'll implement it directly
  from the Fugue paper's tree formulation and pin it with test #1 (the property that
  *defines* correct behavior) rather than trust the prose. If the between-L-and-R
  parent choice is gotten wrong, #1 fails loudly.
- **Wire break** — resolved: clean break, format-version bump, no legacy decode.
- **Scope discipline** — this doc is *only* the ordering swap. Marks/rich text is
  `design-docs/10`, built on top, next.

## Decisions (resolved)

1. **Wire compat:** ✅ **clean break** — bump the format version, old snapshots don't
   load, no legacy decode branch. Safe given no persisted data.
2. **Element field rename** `origin → parent`: ✅ **rename** — the semantics change
   and it matches Fugue vocabulary; accept the wider diff.
