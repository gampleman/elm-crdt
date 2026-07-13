# benchmarks

Performance + footprint harness for the op-log (`docs/02-oplog.md`).

## Memory / size benchmark (`run-mem.js`)

Measures the footprint of built documents so optimization work is evidence-driven
(the "measure first" step). Two kinds of number per workload + size:

- **structural proxies** (deterministic): op count and encoded byte size, from Elm
  via the public API (`OpDoc.opCount`, `String.length` of the encoded JSON). These
  are attributable to a *structure* — compare the per-container workloads.
- an absolute **heap** figure: the Elm worker retains N built docs in its model, so
  their live heap sits in the shared V8 process and `process.memoryUsage().heapUsed`
  sees it. GC-noisy, so force GC (`--expose-gc`) and average.

```sh
cd benchmarks
../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
node --expose-gc run-mem.js
# env knobs: SIZES=100,400  WORKLOADS=demo,text,list,dict,tree  HEAP_COPIES=20
```

Workloads: `demo` (realistic mixed doc), `text`, `list`, `dict`, `tree` (each
per-container, for attribution). The `roundtrip` command flag retains a
decoded-from-wire copy (fresh `ReplicaId` per op) instead of a locally-built one
(shared `ReplicaId` reference), to measure the received-doc overhead. **Always build
with `--optimize`** — dev mode adds debug wrappers that skew heap. See
`docs/02-oplog.md` "Memory / size" for the findings.

Headline: text is the *cheapest* structure per op; per-op `OpId` metadata dominates.
The received-doc heap carries a replica string per op (text +67%, demo +20% over the
built doc). We **tried** a wire replica-table to intern it: it cut encoded size ~14%
(a real bandwidth win) but did **not** reduce heap — the `Array.get`-shared
`ReplicaId` sharing didn't survive into the retained structure — so it was reverted
as not worth the codec complexity for a wire-only gain. Footprint work is parked
until it's a demonstrated bottleneck.

Caveat: `heapUsed` deltas include `Dict`/V8 bookkeeping and assume linear retention —
trust the *ranking* between structures, not the absolute per-doc MB.

## Wire-size benchmark (`run-wire.js` + `run-packed.js`)

Decides whether a custom binary wire format is worth building, against the real
baseline of **gzipped JSON** (`node`'s built-in `zlib`). `Headless` `mode:"wire"`
hands back the full-doc encoding + a one-edit delta as JSON strings; the runners gzip
them.

```sh
node run-wire.js     # raw JSON vs gzip(JSON), full docs + deltas
node run-packed.js   # + a prototype columnar/delta/varint/interned packed format, gzipped
```

**Finding (see `docs/02` and ROADMAP):** gzip(JSON) compresses full docs to 6–9%
(11–16×); the packed prototype beats gzip(JSON) by only 0–18% on full docs and ~0% on
small deltas — **a custom format is not worth it** at these sizes. Two takeaways that
are: use **raw DEFLATE** (not gzip framing) for tiny deltas (free ~5%), and the real
wire bloat is an **algorithmic** bug, not a format one — sequence-insert ops carry the
entire causal frontier as `deps` (979 deps/op measured), which belongs in the
optimization pass, not a codec.

## Merge-timing benchmark (`run-merge.js`)

Measures how long `OpDoc.merge` takes to integrate a small remote delta into a size-`n`
document — the every-incoming-message path in a live app. `Headless` `mode:"merge"` runs
`ITERS` identical merges internally and returns a forced checksum; the runner times the
batch.

```sh
node run-merge.js    # env: SIZES, WORKLOADS, ITERS
```

**Finding (see `docs/12`):** this is the before/after gate for **incremental merge**.
Before, `merge` re-materialized the whole tree from base on every merge, so cost scaled
with the *document* (superlinear: ~13–19 ms at n=400, ~78–120 ms at n=1000). After
applying only the added ops (sorted among themselves, not the whole store), cost scales
with the *delta*: **text n=1000 dropped 77.8 → 2.1 ms (37×), dict 82.5 → 6.1 ms (14×),
list/tree ~4×.** The residual list/tree cost is their read-time move-set re-fold, not the
merge. The same change preserves referential identity of untouched subtrees, which is
what lets the demo's `Html.Lazy` views skip re-rendering.

## Read-path benchmark (the Phase 2 go/no-go gate)

> **Note:** `run.js` is the original read-path gate and expects `OpDoc.cachedState`/
> `freshState` on `Headless`. `src/Headless.elm` has since been repurposed for the
> memory benchmark above; restore the read-path worker (git history) before running
> `run.js` again. Kept here as a record of the Phase 2 result.

Compares reading an op-document via the maintained **cache** (`OpDoc.read` /
`cachedState`) against a full **re-materialization** of the same op store
(`OpDoc.freshState`). Runs headless under Node.

```sh
cd benchmarks
../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
node run.js
```

`run.js` builds a document once per size, then times 100 reads each way and
subtracts the shared build cost, so the columns isolate the read loop.

### Result (the gate — passed)

Read cost, build subtracted:

| N   | cached-reads | fresh-reads | speedup |
| --- | ------------ | ----------- | ------- |
| 50  | ~6 ms        | ~18 ms      | ~3×     |
| 200 | ~25 ms       | ~288 ms     | ~12×    |

`fresh-reads` grows ~linearly in N (a full fold per read); `cached-reads` grows
far slower, and the gap **widens with N**. This is the O(1)-vs-O(N) per-read
divergence the cache exists to create. **Go.**

## Bugs this benchmark surfaced (now fixed + regression-tested)

1. **Stack overflow in `Rga.toElementsInOrder`.** A list built by appending forms
   a linear origin-chain of depth N; the ordering walk recursed N-deep and blew
   the stack around a few thousand elements. Rewritten as an iterative
   explicit-stack DFS. Regression test: `RgaTests` orders a 20k-element chain.

2. **Super-linear build (FIXED for appends).** Each `listAppend` called
   `Rga.lastVisibleId` (O(N)), so building N items was O(N²). Fixed with a
   single-slot **append fast-path** in `OpDoc` (`lastAppend`): a run of appends to
   one list reuses the last id instead of re-ordering, cleared on any other edit
   or merge (equality-safe — it lives in `OpDoc`, never in `Node`/`Rga`). Build at
   N=200 dropped ~21ms → ~7ms; read speedup now ~24× at N=400. The intra-edit
   O(D·N) in text *replace* was also fixed (`Rga.visibleIds` orders once).
   Append-order correctness is tested in `OpDocTests`.

   Still open (lower priority): random-index insert/remove (not appends) still
   call `toElementsInOrder`; a general sub-linear visible-index lookup needs a
   persistent order index. Tracked in `tests/OpLogPhase1Gaps.elm`.

## Browser benchmark

`src/Main.elm` is an `elm-explorations/benchmark` suite (statistical sampling,
needs a browser):

```sh
../node_modules/.bin/elm make src/Main.elm --output benchmark.js
# open an HTML page that loads benchmark.js and calls Elm.Main.init()
```
