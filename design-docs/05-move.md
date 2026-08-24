# 05 — Moving list elements (reorder)

**Status:** ✅ done — `Crdt.MoveList` + `Schema.movableList` + `OpDoc.listMove`, wired through `Node`/`Json`/`OpLog`/`OpJson`. Tested by `tests/MoveListTests.elm` (12, core) and `tests/MoveTests.elm` (9, public API). Demo: native HTML5 drag-and-drop reorder of todos.
**Roadmap item:** #2 (the half that wasn't stable cursors)
**Depends on:** `Crdt.Rga`, `Crdt.OpLog`, `Crdt.OpDoc`, the demo.

## Problem

Reorder a list item — drag a todo to a new position — and have it converge under
concurrent edits, **preserving the item's identity** (its nested text/fields, and
any cursor anchored to it, survive the move).

## Why the obvious approach is wrong (the earlier revert)

A first attempt made `move` **re-point an element's RGA `origin`** (the id it's
ordered after). This is fundamentally broken, not merely buggy:

A list built by appending is a *chain* in the insertion graph:

    a.origin = Nothing,  b.origin = a,  c.origin = b      (order a, b, c)

To move `a` after `c`, you'd set `a.origin = c`. But `b` still has `b.origin = a`,
so now: `a → c → b → a` — a **cycle**. Order in an RGA is derived by walking the
origin graph from the head; a cycle is unreachable from the head. The cycle-safe
walk we added (`toElementsInOrder`) prevents *data loss* (it sweeps up unreachable
elements by id) but the resulting order is an id-tiebreak fallback — so the move
**silently doesn't reorder**. And this happens on *every* move of a non-tail
element, because the moved element always still has children pointing at it.

**Lesson:** order must come from a scheme where a move changes only the moved
element, never a reference others depend on. The insertion graph (`origin`) is the
wrong substrate for moves — it must stay an append-only record of *where things
were inserted*, used for the stable tiebreak, not for live order.

## Two correct designs

### Option A — fractional sort keys (chosen direction; see decision below)

Give each sequence element an **LWW sort key** alongside its existing fields:

```elm
type alias Element c =
    { id : OpId
    , origin : Maybe OpId       -- unchanged: insertion anchor (tiebreak only)
    , content : c
    , deleted : Bool
    , sortKey : ( Frac, OpId )  -- NEW: fractional index + the stamp that set it (LWW)
    }
```

- **Order** = sort by `sortKey`'s `Frac`, ties broken by `elemId` (total + deterministic).
- A fresh insert gets a `Frac` derived from its neighbors at insert time (or just
  its `OpId`-position; the RGA order at insert is a fine seed).
- **Move** = assign a new `Frac` strictly between the destination neighbors'
  keys, stamped with a fresh `OpId`. Because a `Frac` is a **value, not a
  reference**, no cycle is possible — you only ever change the moved element.
- **Merge** = LWW on `sortKey` by stamp (exactly like the register rule, and it
  drops into the existing `mergeElement` which already merges per-element fields
  such as `deleted`/`origin`). Concurrent moves of the *same* item → the
  higher-stamped key wins. Concurrent moves of *different* items → both keep
  their keys, order is well-defined.

`Frac` = a dense order-key. Simplest sound encoding: a `List Int` "path"
(fractional/LSEQ-style) with a tie-break, or rational `(num, den)`. Generating a
key *between* two keys is the one piece of real logic; a `List Int` boundary
allocator is the standard, no-unbounded-growth choice for a demo's needs.

**Known wart (documented, not a bug):** two items dragged into the *exact same
gap* concurrently get keys that may collide on the `Frac`; the `elemId` tiebreak
still gives a deterministic, convergent order — just not necessarily the visual
order either user intended. This is the accepted trade for all fractional-index
schemes (Figma, Linear, etc. live with it).

**Blast radius:** `Crdt.Rga` (Element field + ordering + a `move` + a `Frac`
module), `Crdt.Node`/`Crdt.OpLog` (a `MoveElem` action setting the key),
`Crdt.OpJson` (serialize the key), `Crdt.OpDoc` (`listMove`), demo. Contained:
text and the insertion graph are untouched; it's an additive per-element LWW
register plus a sort in `toElementsInOrder`.

### Option B — move-cell + value-home LWW (Loro/Kleppmann MovableList)

Keep the RGA strictly append-only (never mutate an element). Model a sequence as
an RGA of **cells**, each carrying a `valueId`; a separate LWW **home** register
per `valueId` says which cell is its current location. A move *inserts a new cell*
at the destination with the same `valueId` and sets `home[valueId] = newCell`.
`materialize` emits a cell only if it is its value's current home; other cells are
skipped (like tombstones).

- Textbook-correct, closest to Loro's `MovableList`; no fractional-key collisions.
- **More invasive:** it changes *what a `Seq` element is* (cell-id vs value-id),
  so `Rga`, `Node`, `OpLog`, `OpJson`, `Schema` (list seeding/decoding), cursors
  (anchor to value-id, not cell-id), and GC (old cells need compaction) all shift.

## Decision — **Option B (move-cells), built correctly off the bat**

We build the **move-cell + value-identity** design, *not* fractional keys. The
guiding principle: this is a primitive other things (cursors, GC, future
abstractions) build on, so it must be genuinely correct — Option A's same-gap
collision wart would become load-bearing debt that's hard to undo later.

**Refinement that simplifies Option B** (cleaner than the original sketch): we do
**not** need a separate LWW `home` register per value. A value's live position is
simply **the max-`OpId` cell that carries its valueId**. Every insert/move appends
a cell tagged with the valueId; the cell set is itself the LWW (max `OpId` =
last-writer-wins by `(counter, replica)`). This removes a whole component.

### Model

A new `Crdt.MoveList` (generic `MoveList c`, instantiated `MoveList Node` in
`Crdt.Node`, mirroring how `Crdt.Rga` is used), and a new `Node` variant
`Mov (MoveList Node)` — **additive**: `Seq`/`list`/`Crdt.Text`/the state-based
modules are untouched, so the 108 existing tests stay green. A new
`Schema.movableList` combinator opts in; the demo's todos use it.

```elm
type MoveList c =
    MoveList
        { cells   : Rga OpId        -- positional anchors; each cell's CONTENT is a valueId.
                                     --   insert and move each append one cell. cells are
                                     --   never re-pointed (append-only ⇒ no cycles).
        , values  : Dict String c   -- valueId → the item's content (stable across moves)
        , deleted : Set String      -- deleted valueIds (grow-only; delete-wins)
        }
```

- **order / `toList`**: walk `cells` in RGA order; a value is emitted at the
  position of its **max-`OpId` cell** (its current home), once, if present and not
  deleted. Non-max cells (superseded by a move) are skipped, like tombstones.
- **insert**: valueId = the op's id; append cell{id=valueId, content=valueId,
  after}; `values[valueId] = seed`.
- **move** v after cell `c`: append cell{id=moveOpId, content=v, after=c}. No home
  update — the new cell has the highest `OpId`, so it wins. The element keeps its
  valueId, so its content and any cursor anchored to it are preserved.
- **delete** v: `deleted` += v.
- **merge**: `cells` = `Rga.merge`, `values` = `Dict.merge` with recursive
  `Node.merge`, `deleted` = `Set.union`. Each component is a semilattice ⇒ the
  whole is, and "max cell per value" is a deterministic read-time function ⇒
  convergent. No new LWW-register merge to reason about.

### Identity & addressing

The **valueId is the stable identity**. `Target`'s `IntoElem OpId` now means "into
the value with this id" for a `Mov` node (it already means "by id" for `Seq`).
Cursors anchor to valueId — so a cursor into a moved item's text Just Works, and
visible-index↔valueId resolution uses the materialized order.

### Why not Option A

Fractional keys are contained but carry the same-gap concurrent-move collision
(a convergent-but-maybe-wrong order). For a building-block primitive that's a
defect we'd inherit everywhere. Move-cells have no such wart: concurrent moves of
the same item resolve cleanly by max-`OpId`, of different items both apply.

### Cost, accepted

Additive but broad: a new variant in the closed `Node` union means adding a branch
to every exhaustive match (`merge`, `maxCounter`, `restore`/`reStamp`, accessors,
`Json`, `OpLog.applyOp`/`materialize`/navigation, `Schema`). That breadth is the
price of a closed union — and it's what keeps merge total and fuzz-testable.
Built bottom-up: `Crdt.MoveList` standalone + unit-tested **first** (the
correctness core), then wired up.

## API

```elm
-- Crdt.Rga
move : OpId -> OpId -> Frac -> Rga c -> Rga c   -- stamp, elemId, new key (LWW)
-- (toElementsInOrder now sorts by sortKey then id)

-- Crdt.OpLog
type Action = … | MoveElem { container : Target, elem : OpId, key : Frac }

-- Crdt.OpDoc
listMove : Path -> Int -> Int -> OpDoc a -> Result Error (OpDoc a)
--          path    from   to
--   resolves `from`→elemId and computes a Frac between the visible neighbours
--   at `to`, then emits a MoveElem.
```

`listMove` computes the destination key from the *current visible order*'s
neighbours (the elements that will sit just before/after the moved item at `to`),
so it reads naturally as "drop item `from` at index `to`".

## Demo: drag-and-drop reorder

Chosen UX: **native HTML5 drag-and-drop** on todo rows.

- Each todo `li` gets `draggable=true` and handlers: `dragstart` records the
  source index, `dragover` (preventDefault to allow drop), `drop` reads the target
  index and fires `ReorderTodo from to` → `OpDoc.listMove todosPath from to`.
- Decode indices off the events; store the dragged index in the model
  (`draggingTodo : Maybe Int`) between `dragstart` and `drop`.
- Visual: a drop-target highlight on `dragover`; the existing per-todo presence
  highlight and remote carets keep working (they key off element identity, which
  the move preserves — worth showing: drag a todo a peer is editing, their caret
  follows it).
- Honest caveat: HTML5 DnD event wiring in Elm is fiddly (the `dragover`
  preventDefault, `dataTransfer` quirks). If it fights us, fall back to ↑/↓
  buttons (`listMove i (i±1)`) — same library call, trivial wiring. The library
  feature is independent of which UX we ship.

## Testing (`tests/MoveTests.elm`)

The library half is fully testable headless:

1. **Basic reorder:** move index 0 → end; `read` shows the new order; round-trips.
2. **Identity preserved:** edit a todo's text, then move it; the text moves with
   it (this is the whole point — Option A keeps `id`/`content`).
3. **Move to head / middle / end** boundary cases.
4. **Concurrent move of the same item → LWW converges** (both merge orders equal).
5. **Concurrent move of different items → both honoured**, order deterministic.
6. **Move + concurrent insert** converge.
7. **No cycles / no loss:** the failure mode that killed the origin approach —
   after a move, every element still appears exactly once, in the intended order.
8. **A cursor anchored to a moved element still resolves** to that element.
9. **JSON round-trip** of a `MoveElem`.
10. **`Frac` between**: fuzz that `between a b` yields a key strictly ordered
    between `a` and `b` for any `a < b` (the one bit of real arithmetic).

## Risks

- **`Frac` allocator correctness** is the load-bearing new logic — fuzz the
  `between` invariant, and confirm keys stay bounded over many sequential moves
  (a `List Int` boundary allocator grows the path on adversarial interleavings;
  fine for the demo, note it).
- **Convergence with the new LWW field** — must extend the existing `Node`/RGA
  merge-law fuzz tests to include `sortKey`, so the semilattice properties still
  hold with the field present.
- **GC interaction:** a `MoveElem` is just another op below/above a cut; since the
  key lives on the element (which survives in `base`), compaction is unaffected.
  Add one test (move, then gc, then read).
- **Cursors:** unaffected in principle (they anchor to element id, and order is
  derived) — but test #8 confirms it rather than assuming.
