# 12 — Referential stability + diff subscription

**Status:** in progress. **Roadmap item:** #1 of the remaining list (was "referential
stability + event / diff subscription").

Two capabilities that turn out to be one mechanism:

1. **Referential stability** so `Html.Lazy` (and any `==`-guarded memoization) actually
   pays off — after a remote change, subtrees that did not logically change should keep
   their **reference identity**, so `lazy`-wrapped views don't re-render.
2. **Diff subscription** — a "what changed" value after a merge/ingest, to drive
   effects, animation, and provenance-aware UI, carrying whether each change was
   **local** or from a specific remote replica.

They are coupled because they rest on the same fact: **the ops that were applied _are_
the diff, and applying only those ops (instead of re-materializing) is what preserves
identity.**

## The problem today

`OpDoc` maintains a materialized `cached : Node` and reads by decoding it. Local edits
(`commit`) fold new ops straight onto `cached` via `OpLog.applyOps`, which reallocates
only the **spine** from the root to each touched node and structurally shares every
untouched sibling subtree. Good — local edits are already identity-preserving off the
edit path.

But **`merge` and `decodeInto` re-materialize the whole tree from `base`**
(`OpLog.materialize base store` — a full causal-order fold over *every* op). The
comment on `merge` says it plainly: "a merge can interleave ops causally anywhere, so
re-materialize from base." The result is a brand-new `Node` sharing **no** references
with the previous `cached`, even for containers no incoming op touched. So after any
remote message every `lazy` view re-renders, and there is no diff.

## Why incremental merge is correct

Every `OpLog.Action` is a **commutative, idempotent** function on the `Node`, with all
ordering/conflict resolution **deferred to read time**:

- **Registers / dict presence** — LWW by stamp; `setRegLww` only overwrites when the
  incoming stamp is greater, else no-ops. Applying a stale op is a no-op.
- **Sequences / text (RGA / Fugue)** — `insertElem` stores the element in a `Dict` keyed
  by id; `deleteElem` sets a tombstone. Order is re-derived in `toElementsInOrder`,
  which even keeps an element whose `parent` hasn't arrived as a root (never dropped).
- **Counters** — deltas accrete, keyed by op id (a shared key carries the same delta).
- **Tree moves / movable-list moves** — moves accrete into a set/list keyed by op id;
  `Tree.resolve` / `MoveList` re-fold the whole set at read (sorted by move-op id,
  cycle-skipping). The `Tree` module's own docstring notes it relies on OpLog re-folding.
- **Marks** — accrete into a set keyed by op id.

So the materialized `Node` is a **pure function of the op _set_**, and

    applyOps cached newOps  ==  materialize base (local.store ∪ incoming.store)

as long as `newOps` (the ops in the merged store not already in `cached`) are applied
in a **causal order**.

### The one hazard: cross-container causal order

`applyOp` addresses its target with `updateAt container …`. If an op that edits *inside*
a dict key is applied **before** the `SetKeyPresence` op that creates that key, `updateAt`
fabricates the intermediate node with a default (a bare `Map`), and the later
`SetKeyPresence` — being LWW and finding the key already present — will not replace it with
its typed seed. The container then materializes as the wrong node type. (This is exactly
the "schema read error: expected rich text" bug we hit when a marker op with a
front-sorting id landed before its container existed.)

A full `materialize` avoids this because `causalOrder` topologically sorts so every op
follows its `deps`. Incremental merge must do the same: apply `newOps` in a causal order
(each op after the ops it depends on, whether those are already in `cached` or earlier in
`newOps`). We reuse `causalOrder` over the merged store and keep only the new ops in that
order — `causalOrder` already treats deps that are absent-from-the-store (i.e. already
folded into `cached`) as satisfied, so the ordering is valid relative to `cached`.

### The identity win

Applying `newOps` onto `cached` reallocates only the spines those ops touch; every
container no incoming op addressed keeps its exact previous reference. So after a remote
merge, `lazy (viewTodos …) doc.todos` sees the *same reference* when the remote edit was
to `settings`, and skips re-rendering. This is the whole point.

## Design

### Part A — incremental merge (foundation)

Replace the full re-materialize in `merge` and in `decodeInto`'s `rebuild` with an
incremental apply of the new ops in causal order:

    cached' = OpLog.applyOps cached (newOpsInCausalOrder mergedStore cached)

Guarded by the existing `cacheConsistent` invariant
(`cached == materialize base store`), which becomes the correctness oracle — extended
with tests over the tricky cross-container / out-of-causal-order cases, tree/move
merges, and snapshot ingest. The GC/snapshot path (`decodeInto` adopting a new `base`)
still re-materializes, since a new base is a different starting point — that is rare
(catch-up only) and correctness-critical, so it stays conservative.

### Part B — the diff value (Ref-queryable, **no public `Path`**)

The applied ops *are* the raw diff, but the public API must **not** expose `Path`: the
v2 API deliberately replaced the stringly-typed public `Path` with typed `Ref` optics,
and a diff that handed back `Path`-carrying changes would reintroduce it. It also can't
be a `List Change` where each change carries *its own* typed location — a heterogeneous
list of differently-typed `Ref`s is the existential/GADT shape Elm can't express (that
is *why* `Ref` is a write-only optic the app holds, not something the library returns).

So the diff is an **opaque value you query with the typed refs you already have**:

    type Diff                       -- opaque; internally: changed locations + origins
    type Origin = Local | Remote ReplicaId

    -- did the spot at `ref` (or anything under it) change, and by whom?
    Diff.touched : Ref root kind a -> Diff -> Maybe Origin

    -- every origin that contributed a change (was there any *remote* change? whose?)
    Diff.origins : Diff -> List Origin

    mergeWithDiff  : OpDoc a -> OpDoc a -> ( OpDoc a, Diff )
    decodeWithDiff : Value  -> OpDoc a -> Result Error ( OpDoc a, Diff )

`Path` stays entirely internal: a `Ref` already compiles down to one, `Diff` stores the
changed locations, and `touched` compares `ref`'s path against them behind the module
boundary — never surfacing a `Path`. `Origin` comes from each applied op's id `replica`
(`ReplicaId` is already public). Coalescing: a run of character inserts/deletes on one
text container collapses so a 100-char paste is one changed location, not 100.

This is more idiomatic than a change list: for a conditional effect you ask "did
`board.refs.title` change?" (`Diff.touched`), and to highlight a peer's edits you fold
your own known refs against the diff. Finer granularity ("*which* list element is new")
is handled app-side by diffing the now-referentially-stable decoded values (cheap once
Part A lands) — the library needn't carry per-element locations. The plain
`merge`/`decodeInto` stay (diff is opt-in, zero-cost when unused). Returned as a
**value** from `update`, not an imperative subscription — TEA's `update` *is* the
subscription point.

### Part C — diff-driven selective re-read (what actually enables `Html.Lazy`)

An early plan here was to memoize `read` (cache the last `( Node, a )` so an unchanged
doc returns the same `a`). Building the demo verification showed that is the **wrong**
lever: Elm's `Html.Lazy` fast-paths on **reference** equality of the view's arguments,
and `read` produces a fresh whole-document `a` whenever *any* field changes — so a
whole-doc memo never fires `lazy` for an unchanged *sub*-tree once anything else moved.

The mechanism that actually pays off (and the reason B and this are coupled): the **app
holds decoded slices** and, on each change, uses the **diff** to re-`read` only the
slices that were touched (`Ref.touched slice.ref`), leaving untouched slices at their
previous reference. Then `Html.Lazy` over a slice fast-paths whenever the change was
elsewhere. Part A supplies the identity-stable `Node` underneath; Part B supplies the
"what changed"; the app composes them. The demo does exactly this for its todos slice
(`refreshSlices` + `Html.Lazy.lazy viewTodoSummary model.todosSlice`), and an e2e test
(`demo/e2e/referential-stability.spec.js`) asserts, via a `Debug.log` render marker, that
an unrelated edit — local or from a peer — does **not** re-render the summary while a
todos edit does.

A plain internal `read` memo (single-slot `( Node, a )`) remains a possible O(1) win for
the "doc unchanged since last render" case, but it is a minor perf refinement, not the
enabler, so it is not part of this work.

## Build order

1. **Part A** — incremental merge + `newOpsInCausalOrder`; keep `cacheConsistent`, add
   adversarial tests (cross-container order, tree/move merges, snapshot ingest,
   idempotent re-merge). This is the load-bearing correctness change.
2. **Referential-stability test** — a merge to container X leaves container Y's decoded
   sub-value (or the `Node`) referentially equal; a Playwright/`Html.Lazy` smoke check
   in the demo is optional follow-up.
3. **Part B** — `mergeWithDiff` / `decodeWithDiff` + `Diff`/`Change`/`Origin`, coalescing.
4. **Part C** — `read` memoization.

## Risks / open questions

- **Causal-sort cost.** Incremental apply still runs `causalOrder` over the merged store
  (O(n²)-ish today), so the *sort* cost is unchanged; the win is skipping re-apply of old
  ops **and** preserving identity. If the sort dominates, a later optimization is to sort
  only `newOps` against a cached frontier. Not a blocker for this item.
- **Reference equality in Elm.** Elm exposes only structural `==`; there is no
  `identity` primitive. Structural sharing still gives `Html.Lazy` its win because
  `lazy`'s check short-circuits on reference equality *before* falling back to
  structural — an untouched shared subtree passes the fast path. Verify empirically.
- **Diff granularity.** First cut coalesces per (container, kind). Finer provenance
  (which character, which field) is possible from the ops but out of scope for v1.
- **Snapshot/base adoption** stays a full re-materialize (correctness over identity on a
  rare catch-up path).
