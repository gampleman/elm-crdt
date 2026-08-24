# 02 — Op-log core (collaborative history, delta sync, GC)

**Status:** planned — supersedes the standalone delta-sync plan
**Roadmap item:** #1 (reframed)
**Enables:** collaborative branchable history, time-travel/checkout, fork, delta
sync, and GC — all from one core.

## Why this, and why now

The original plan ([`01-delta-sync.md`](01-delta-sync.md)) kept us state-based and
shipped throwaway *delta-state* fragments. That gets bandwidth, but it does **not**
get history: a state CRDT has no record of *how* it reached its state, so
collaborative, branchable time-travel is impossible. Our current `Crdt.History`
proves the point — it fakes time-travel by storing whole-`Node` snapshots, which
is memory-heavy, commit-granular, local-only, and unbranchable.

The decision: **history is a first-class requirement**, so we move the source of
truth from *state* to an *operation log*. The realization that makes this cheap
rather than a rewrite:

> A retained, ordered, addressable log of deltas **is** an op log. Delta sync
> stops being separate work and becomes "ship a range of the log." Building
> delta-state first would mean building the version-vector/frontier machinery
> twice.

So we pivot to the op-log now, and delta sync + history + GC all fall out of it.

## The model

```elm
type alias OpId = ( Int, ReplicaId )         -- (lamport counter, replica) — unchanged

type alias Op =
    { id : OpId
    , deps : Frontier          -- the op ids this op causally follows (its parents in the DAG)
    , action : Action          -- what it does (below)
    }

type alias Frontier = List OpId   -- the causal "tips"; a minimal cut of the DAG

type Action
    = SetReg Target Prim                      -- LWW register write
    | SetKeyPresence Target Bool                 -- map key present/absent (presence cell)
    | InsertElem { container : Target, id : OpId, after : Maybe OpId, value : Seed }
    | DeleteElem { container : Target, elem : OpId }
    | MoveElem  { container : Target, elem : OpId, after : Maybe OpId }   -- enables MovableList (roadmap #2)

type alias Target = List PathSeg   -- where in the tree; PathSeg already exists in Crdt.Path
```

A document becomes the log plus a cached materialization:

```elm
type alias Doc =
    { ops : OpStore             -- the DAG of ops, keyed by OpId (set-union merge)
    , frontier : Frontier       -- current tips
    , version : Version         -- Dict ReplicaId Int — max counter seen per replica
    , state : Node              -- MATERIALIZED read model (cache; see performance)
    , ctx : Ctx                 -- minting, unchanged
    }
```

**State is now derived:** `state = materialize ops`. The `Node` tree we already
have stops being the source of truth and becomes the fold result that
`Crdt.Schema` reads from. The schema/codec layer is **unchanged**.

## What materialize does (where the CRDT logic moves to)

`materialize : OpStore -> Node` is a deterministic, local fold over ops in causal
order. The conflict-resolution logic we already wrote **moves here, intact**:

- `SetReg` / `SetKeyPresence` → last-writer-wins by `OpId` (today's `mergeRegister` /
  presence-cell rule).
- `InsertElem` / `DeleteElem` → today's RGA: ids, origins, deterministic sibling
  ordering, permanent tombstones during a materialization.
- `MoveElem` → the move op the current model can't express.

The big simplification: **`merge` becomes set-union of op stores keyed by `OpId`**
— trivially commutative, associative, idempotent (re-adding an op you have is a
no-op). The semilattice burden leaves `merge` (structural, fiddly) and lands on
`materialize` (a pure local fold, far easier to test). The existing
commutativity/associativity/idempotence fuzz tests get *easier* to satisfy.

## How each feature falls out

| Feature | Mechanism |
| --- | --- |
| **Delta sync** | `OpLog.opsAfter : Frontier -> OpStore -> List Op` — the ops *not* among the causal ancestors of the peer's frontier. Receiver unions them in. Defined by the DAG, **not** by comparing Lamport counters (a version vector is unsafe here — our `Id.observe` clock is gappy, so "counter ≤ vv[replica]" could skip an op). |
| **Time-travel / checkout** | `materialize` restricted to ops causally ≤ a target `Frontier`. Any point, not just commits. |
| **Collaborative history** | The DAG is shared: any two peers holding op X agree on its causal position. "The version before Bob's edit" is well-defined across peers. |
| **Fork / branches** | A `Frontier` *is* a branch handle; materialize from it, append ops with it as `deps`. |
| **Undo/redo** | Emit an inverse op (or a `deps`-based "skip" marker) — real ops that sync, not a local stack. |
| **GC / shallow snapshot** | Replace ops causally below a frontier *everyone* has with one snapshot op; drop their tombstones safely (the causal-stability argument state-based can't make). |

## Materialization performance — the one genuinely hard part

Naive `materialize` is O(all ops) per read, which is unacceptable on every
keystroke. The standard answer (Loro's `DocState` + shallow snapshot, Automerge's
similar split):

- **Cache the materialized `Node`** in `Doc.state` (shown above).
- **Incremental apply:** appending a local op or merging a small delta updates the
  cached `state` directly without a full re-fold; only a `checkout` to an
  arbitrary historical frontier pays a re-materialization.
- **Snapshot + tail:** keep a materialized snapshot at some frontier plus the ops
  after it; re-fold only the tail. GC advances the snapshot.

This is the part to prototype first and benchmark — it's where the op-log earns
or loses its keep. Everything else is mechanical.

## Migration from the current state-based core

Reassuringly incremental — most modules survive:

| Module | Fate |
| --- | --- |
| `Crdt.Schema` (codec) | **Unchanged** — reads a materialized `Node`. |
| `Crdt.Node` | Stays as the materialized **read model**; its `merge` is retired in favor of `materialize`'s fold (which reuses the same LWW/RGA rules). |
| `Crdt.Rga` | Ordering + sibling rule reused inside `materialize`. |
| `Crdt.Edit` | Edits now **emit `Op`s** instead of mutating `Node`. Crucially, edits **already resolve visible indices to stable `OpId`s** (`Rga.idAtVisibleIndex` / `originForVisibleIndex`, `Crdt.Text`), so producing position-independent ops is half-done. |
| `Crdt.Path` | `PathSeg` reused verbatim as op `Target`. |
| `Crdt.Id` | `OpId` / `Ctx` / `observe` reused; add `Frontier`. |
| `Crdt.Json` | Re-targeted: encode/decode **ops**, not the state tree. |
| `Crdt.History` | **Reimplemented** on the DAG (checkpoints become named frontiers; checkout/fork become real). The public API can largely stay. |
| `Crdt.Presence` | Untouched — ephemeral, stays off the log. |

The wire format changes (we ship ops, not state). We have no compat obligation,
so it's a clean break.

## Risks / open questions

- **Materialization cost** — addressed above; must be prototyped + benchmarked
  before committing the whole API. This is the make-or-break.
- **Op granularity for text** — one op per character is a lot of ops. May want
  run-length insert ops; affects the DAG size and GC.
- **`deps`/frontier bookkeeping** — getting causal `deps` right is the new
  correctness surface (replaces structural-merge correctness). Needs its own fuzz
  tests: materialize is order-independent over any causal-order linearization.
- **Undo semantics in a collaborative DAG** — inverse-op vs. skip-marker is a real
  design sub-question (Loro's UndoManager is non-trivial); defer the polish.
- **MovableList (#2)** now folds in naturally via `MoveElem` — worth doing as part
  of the core rather than bolting on later.

## Phase 0 spike — DONE (retired)

The spike proved the model end-to-end (folding ops into a `Node`, read through the
real `Crdt.Schema`). It has since been **retired** — superseded by the real Phase 1
module `src/Crdt/OpLog.elm` — so there is no throwaway code to maintain. Its
findings are recorded below.

**Two findings that change the Phase 1 design:**

1. **`materialize` must fold over the schema's *empty structure*, not a bare
   map.** The `record` decoder requires every field present, but ops only create
   fields they touch, so a bare-map fold fails with `MissingField`. Production
   shape: `materialize schema ops = List.foldl applyOp (Schema.emptyNode schema)`
   — i.e. materialize needs the schema (or a stored empty/snapshot) as its base,
   it is not schema-agnostic. (Alternative: make the decoder tolerate absent
   fields — but seeding from `emptyNode` is cleaner and reuses existing code.)

2. **Container-insert ops must carry their sub-schema's empty subtree (`Seed`).**
   Inserting a list element seeded with a bare map fails the same way one level
   down (`MissingField "label"`). Our existing `Seed` type (`Schema.with` /
   `emptyNode`) already produces exactly this, so `InsertElem` carries a `Seed`.

Both confirm the migration is sound and that `Crdt.Schema`/`Seed`/`Crdt.Path` slot
in as planned.

## Phase 1 — DONE

Real op-log core in `src/Crdt/OpLog.elm`, tested in `tests/OpLogTests.elm`
(16 tests, all green; `elm-review` clean):

- **`Op { id, deps : Frontier, action }`** with an `OpStore` (a `Dict` keyed by
  `OpId` string). `merge = Dict.union` — set-union by id, so the join-semilattice
  laws are trivially satisfied (tested: commutative / associative / idempotent on
  the read).
- **`causalOrder`** is a Kahn topological sort honouring `deps`, ties broken by
  `OpId` for determinism; partial stores (deps pointing at not-yet-received ops)
  still materialize. **`materialize base store`** folds in that order;
  **`applyOps`** folds an explicit order.
- **Causal order genuinely matters** (not just a capability): a `DeleteElem`
  folded before its target's `InsertElem` is a silent no-op. Tested via the
  delete-after-insert case, and order-independence is fuzzed over 100 random
  store-insertion permutations of a causally-structured op set.
  *(No longer a no-op — such an op is now held back and retried, so a delivery that
  isn't causally closed loses nothing. See [`15-pending-ops.md`](15-pending-ops.md).)*
- **`checkout frontier`** materializes only the causal ancestors of a frontier —
  real time-travel, verified to show pre-delete state.
- **`SetReg`** (LWW primitive, resolved by stamp *not* fold order — tested),
  **`SetKeyPresence`** (dict key present/absent, LWW, with creation seed — closes the
  spike's omission), **`InsertElem`/`DeleteElem`** (RGA reuse).
  *(The creation seed is now the value's canonical empty skeleton rather than its value, and
  only a creating `SetKeyPresence` installs one — a map key is the one slot two ops can create,
  so a seed carrying the value made the result depend on arrival order. See
  [`15-pending-ops.md`](15-pending-ops.md).)*
- Cross-replica convergence: two peers' concurrent stores read equal in either
  merge order.

**Public surface (Phase 1 tail — DONE).** `src/Crdt/OpDoc.elm` wraps an
`OpStore` + schema + clock into a public document whose API mirrors
`Crdt.Edit`: `init`, `read`, `merge`, and path-addressed `setText` / `setBool` /
`setInt` / `setString` / `listAppend` / `listRemove` / `setKey` / `removeKey`.
Edits **resolve a visible-index `Path` against the current materialized state into
a stable id-based `Target`**, then emit ops — so the index-addressed public API
produces position-independent ops. `setText` emits the minimal char insert/delete
diff. Tested in `tests/DocTests.elm` (14 tests: the state-based
`ConvergenceTests` scenarios ported to the op model — concurrent title/list/dict
edits converge both merge orders, LWW, error paths).

Remaining tail (one `todo`): a JSON wire format for an `OpStore` so ops can be
shipped (today `OpDoc.allOps` exposes them un-serialized).

## Phase 4 — collaborative history (DONE: time-travel)

The feature that drove the whole pivot. `Crdt.OpDoc` now exposes `Version` (an
opaque causal frontier), `version : OpDoc a -> Version`, and
`readAt : Version -> OpDoc a -> Result _ a`. Because a `Version` is a point in the
shared op DAG (not a local snapshot), it is **collaborative and branchable**:

- `tests/DocTests.elm` proves a `Version` captured before a `merge` excludes
  *both* the peer's concurrent ops and later local edits, while the live document
  reflects everything — the property the snapshot-stack `Crdt.History` could never
  give.
- A `Version` doubles as a branch handle (checkout + keep editing the live doc).

All of what this note once listed as open is now shipped: named checkpoints over a
`Version` (`Doc.checkpoint`), an explicit fork/branch API with branch comparison
(`Doc.fork` / `forkAt` / `divergence` — a branch is a re-keyed `Doc` off a `Version`, a
merge-back is a plain `merge`, `divergence` is the cross-store op set-difference), and
undo/redo as syncing inverse ops (`Doc.undo` / `redo`, replacing `Crdt.History`'s local
whole-root stacks).

### Spike findings (still relevant)

_(Historical: the Phase-1 gaps were once tracked as `Test.todo` in a
`tests/OpLogPhase1Gaps.elm` gap module. That file has since been removed — every gap it
listed is now either implemented and covered by a real test or recorded in the
[ROADMAP](../ROADMAP.md), and a permanent backlog of failing `todo`s is the wrong tool
for CI.)_

## Phased plan

0. ~~**Spike:** `Op`/`Action` + `materialize` over a flat op list reproducing
   today's `read`.~~ **Done — see above.**
1. ~~**DAG + frontiers:** real `deps`, causal-order materialize, `merge` =
   op-union; checkout; SetKeyPresence/SetReg.~~ **Done — `src/Crdt/OpLog.elm`.**
   Remaining tail: wire into the public `Crdt` module + port `ConvergenceTests`.
2. **Incremental materialization.** ~~Cache the materialized `Node`; fold local
   ops onto it (O(1) hot path), re-materialize on merge.~~ **Done —
   `Crdt.OpDoc` (`cached`), invariant `cachedState == freshState` tested;
   `benchmarks/` confirms the read-path gate (fresh O(N) vs cached far flatter,
   speedup widens with N → ~12× at N=200).** The benchmark also surfaced + fixed a
   **stack overflow** in `Rga.toElementsInOrder` (iterative DFS now; 20k-chain
   regression test). **Open tail:** the *edit* path is still O(N) per op
   (`Rga.lastVisibleId`/`idAtVisibleIndex` call `toElementsInOrder`), so N appends
   are O(N²) — needs an RGA order-index cache. And merge still re-materializes the
   whole store (snapshot+tail would bound it).
   Go/no-go on performance here.
3. ~~**Delta sync** + rewire the demo to ship op ranges.~~ **Done —
   `OpLog.opsAfter` / `OpDoc.encodeSince` (frontier-based, not version-vector);
   demo broadcasts per-edit deltas with a hello/full-state handshake on connect.**
4. ~~**Collaborative history** on the DAG: checkout, fork, named frontiers;
   replace `Crdt.History`'s snapshot stacks. Fix the demo's history UI.~~
   **Done — `OpDoc.version`/`readAt`/`versionAt` (per-op scrubber), named
   `Checkpoint`s, `restoreTo` (syncing diff-based restore), and local
   op-inverting `undo`/`redo` (`tests/RestoreTests.elm`, `tests/UndoTests.elm`).
   Demo has a history scrubber, restore button, and undo/redo.** (State-based
   `Crdt.History` remains for the `Crdt` flavor.)
5. ~~**MoveElem / MovableList**, then **GC / shallow snapshot**.~~ **Both done —
   `Crdt.MoveList` + `S.movableList` + `OpDoc.listMove` ([`05-move.md`](05-move.md));
   `OpDoc.gc` + `OpLog.compact` + snapshot-transfer wire ([`04-gc.md`](04-gc.md)).**

The op-log core is now **feature-complete**; remaining work is additional
containers (Tree, rich text) and capabilities (diff subscription, binary
encoding) — see [`../ROADMAP.md`](../ROADMAP.md).

## The wire format is now pinned, not just round-tripped

`Crdt.OpJson` had no test module of its own for a long time: it was covered
*transitively*, by the sync tests going through `Doc.encode`/`decodeInto`. That covers
plenty, but it is structurally blind to the property that matters most about a wire
format, because it encodes and decodes with the **same build**. Rename a field key, swap
two positional components, drop one from a branch — every round-trip test still passes,
and every real peer breaks.

`tests/OpJsonTests.elm` closes that with three claims:

- **Lossless** over an arbitrary op (`Helpers.fuzzOp`, which covers every `Action`
  constructor — a missing one there is an unfuzzed wire branch), plus one hand-written
  fixture per kind with every optional component present.
- **Stable**: the literal bytes of those fixtures are pinned as a string. That test exists
  precisely to fail on a change that is compatible with itself — the kind a green build
  would otherwise wave through, since encoder and decoder move together. **Pre-1.0 the
  right response to a failure is usually to re-pin it**: the format is not frozen until
  `1.0.0` ships, so breaking it is cheap and no deployed peer is owed anything. The pin's
  job before then is to make the change *deliberate and visible in the diff* rather than
  incidental — a format change should be something you decided, not something you noticed
  later. After 1.0.0 the same test becomes a compatibility guard, and the answer to a
  failure changes to "revert, or version the format on purpose".
- **Total on hostile input**: 25 named malformed payloads must each decode to `Err`, an
  unknown action kind is **rejected rather than skipped** (dropping an op keeps the
  document readable while making it permanently, silently wrong — it is gone from this
  replica's store, so it is never retried and never re-sent; failing the delta leaves the
  peer's `version` un-advanced so it re-offers it), and a delta containing one corrupt op
  leaves the receiving document byte-for-byte as it was — decode fully, then apply, so a
  failed sync is retryable rather than half-merged.

One deliberate **tolerance** is pinned rather than fixed: a `"pos": []` on a tree move
decodes to the midpoint, because `Frac` has no invalid value and rejecting the op would
strand the node. Empty digit lists therefore do not round-trip.

## Memory / size — measured, not guessed (`benchmarks/run-mem.js`)

Before optimizing footprint we measured (`node --expose-gc run-mem.js`, `--optimize`
build). Per-workload at n=400 elements — encoded bytes and retained Elm heap:

| workload       | ops  | encoded | bytes/op | heap/doc | heap/op |
| -------------- | ---- | ------- | -------- | -------- | ------- |
| text (chars)   | 392  | 79 KB   | **202**  | 246 KB   | ~630    |
| dict (keys)    | 400  | 57 KB   | 142      | 296 KB   | ~740    |
| list (records) | 400  | 308 KB  | 770      | 1.16 MB  | ~3000   |
| tree (nodes)   | 400  | 306 KB  | 764      | 2.03 MB  | ~5200   |
| demo (mixed)   | 3247 | 1.9 MB  | 585      | 7.36 MB  | ~2280   |

Findings (dev vs `--optimize` differed <3%, re-runs <1% — stable enough to rank):

1. **Text is the *cheapest* structure per op, not the hog.** A character costs ~200
   encoded / ~630 heap bytes, so **run-length text is low-leverage** — the intuitive
   first optimization is the wrong one. Measuring saved building the hard thing.
2. **Nested-record containers dominate per element** — a list todo (~3 KB heap) or
   tree node (~5.2 KB) carries a whole `Map` subtree (its own text RGA + presence
   stamps) plus move/cell metadata; 5–25× a char.
3. **The common cost across *all* structures is per-op `OpId` metadata, not content.**
   A char holds ~1 byte of text but costs 200 — the rest is OpIds. Every `OpId` is
   `OpId Int (ReplicaId String)`, and the replica string is re-embedded in every id,
   stamp, dep, and every `"c@replica"` `Dict` key. That is what `bytes/op` measures,
   and it is uniform across containers.

**Where the per-op metadata cost actually bites — and what we tried.** A
*locally-built* doc mints all its OpIds from one `Ctx`, so they already share one
`ReplicaId` reference; the replica cost is a *decoded*-doc phenomenon — `opIdDecoder`
allocates a fresh `ReplicaId` per op, so a received/merged doc (the one you hold in a
collaborative app) is heavier: text +67%, demo +20% over the built doc.

We **attempted** a wire replica-table (encode each replica once, reference by index;
decode via a shared `Array ReplicaId`) to fix both wire and heap. Result: it cut
encoded size **~14%** (a real bandwidth win, fully convergence-tested) but **did not
reduce heap** — the intended `ReplicaId` sharing didn't survive into the retained
`Dict`/`Node` structure. Since heap (not bandwidth) was the goal, and the table
threaded through ~30 codec functions for a wire-only gain, it was **reverted**.

**Conclusion:** run-length text is deprioritized (text is the cheapest per op); the
replica-table wire win is available if bandwidth ever becomes the bottleneck (it's a
clean, tested change to reapply); `gc`/`compact` remains the growth bound; and
footprint work is otherwise parked until it's a demonstrated pain point. Caveat:
`heapUsed` deltas include `Dict`/V8 bookkeeping and assume linear retention — good
for *ranking* structures, not exact per-doc MB.

> `01-delta-sync.md` is retained for context but is **superseded** by this doc:
> its version-vector design lives on here as step 3, and its "stamp deletions"
> insight is moot because deletions are now first-class `DeleteElem` ops.
