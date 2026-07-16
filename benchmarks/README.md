# benchmarks

## How we compare (elm-crdt vs Automerge vs Loro vs no-CRDT)

Same operations, same Node process, median ms. Automerge 2.x / Loro 1.x are **Rust →
WASM**; elm-crdt is **Elm → JS**, so a constant-factor gap to Loro (the fastest) is
expected — the point is we're within a small factor and **algorithmically on par**. The
**plain Elm** column is a no-CRDT `Board` built through the identical harness (immutable
Elm data — the honest "cost of adding CRDTs to my Elm app" floor, *not* plain JS's mutable
arrays, which are ~35× cheaper still). Full detail + method in
[`results/COMPARISON.md`](results/COMPARISON.md); regenerate with `run-bench.js` +
`run-compare.js`.

**List build** — append N `{text, done}` items (ms):

| n    | plain Elm | Loro (WASM) | **elm-crdt** | Automerge (WASM) |
| ---- | --------: | ----------: | -----------: | ---------------: |
| 100  |     0.05  |        0.76 |     **1.03** |             6.05 |
| 400  |     0.06  |        2.00 |     **3.52** |             14.4 |
| 1000 |     0.14  |        4.22 |     **7.98** |             35.6 |

→ ~**1.9× Loro**, ~**4.5× faster than Automerge**; linear. ~**57×** a plain Elm list.

**Text build** — insert N chars, one op/char (ms):

| n    | plain Elm | Loro (WASM) | **elm-crdt** | Automerge (WASM) |
| ---- | --------: | ----------: | -----------: | ---------------: |
| 100  |     0.02  |        0.16 |     **0.18** |             1.38 |
| 400  |     0.05  |        0.53 |     **0.54** |             6.11 |
| 1000 |     0.13  |        1.16 |     **1.29** |             20.1 |

→ **~parity with Loro**, ~**15× faster than Automerge**. ~**10×** a plain Elm string build.

**Read** (materialize whole value, ms) — not fully apples-to-apples: Automerge returns a
lazy proxy (defers the cost), so only Loro's `.toJSON()` is a fair peer:

| n=1000 | Automerge (lazy) | Loro (`.toJSON`) | **elm-crdt** |
| ------ | ---------------: | ---------------: | -----------: |
| list   |               ~0 |             0.72 |      **4.6** |

→ ~**6× Loro** — our relative weak spot (full typed re-materialization vs a JSON walk); the
natural next target if a workload is read-heavy.

**Cost of collaboration** — elm-crdt vs the same edits on a plain immutable Elm `Board`
(no `Doc`): list build **0.14 → 7.98 ms ≈ 57×**, text **0.13 → 1.29 ms ≈ 10×** at n=1000.
Sounds large, but that's 7.6 µs/op (list) / 1.3 µs/char (text) — comfortably sub-frame —
and it buys convergence, per-op identity, history/undo, and delta sync.

---

Performance + footprint harness for the op-log (`docs/02-oplog.md`). All runners drive the
compiled `src/Headless.elm` worker over a port. Use the npm scripts — each **rebuilds
`headless.js` `--optimize` first**, so you never measure a stale or dev-mode build:

```sh
cd benchmarks
npm install            # once — pulls Automerge + Loro for `compare`
npm run bench          # build + read latency
npm run all            # bench + merge + mem
```

| script          | runner                | measures                                                        |
| --------------- | --------------------- | --------------------------------------------------------------- |
| `npm run bench` | `run-bench.js`        | build + read **latency** (ms/op) — the interactive path         |
| `npm run merge` | `run-merge.js`        | full-doc `merge a b` time (unions two whole op stores — O(n))   |
| `npm run delta` | `run-delta.js`        | `decodeInto` a one-edit delta — the demo's per-message path (O(delta)) |
| `npm run mem`   | `run-mem.js`          | structural proxies (ops, bytes) + retained heap (`--expose-gc`) |
| `npm run wire`  | `run-wire.js`         | wire size: raw JSON vs gzip(JSON)                               |
| `npm run packed`| `run-packed.js`       | + a prototype packed binary format                              |
| `npm run compare`| `run-compare.js`     | **external comparison** vs Automerge + Loro                     |
| `npm run all`   | bench + merge + mem   | the core suite                                                  |
| `npm run build` | —                     | just (re)compile `headless.js --optimize`                       |

All take `SIZES`, `WORKLOADS` (and where relevant `ITERS`, `REPEAT`, `HEAP_COPIES`) env
knobs, e.g. `SIZES=100,1000 WORKLOADS=list,tree npm run bench`. The plain-Elm no-CRDT
baseline is `run-bench.js`'s `plainlist` / `plaintext` workloads.

Note for `run-delta.js`: each measurement builds the size-`n` doc **once** and then times
`ITERS` decodes against it, so the one-time build is amortized across `ITERS`. For an
expensive-build workload (`demo`) at large `n`, use a high `ITERS` (e.g. 2000+) or the
per-decode figure will be inflated by residual build cost, not decode cost.

Results snapshots live in `results/`: `BASELINE.md` (the optimization before/after) and
`COMPARISON.md` (the vs-Automerge/Loro/no-CRDT tables above, with method).

## Memory / size benchmark (`run-mem.js`)

Measures the footprint of built documents so optimization work is evidence-driven
(the "measure first" step). Two kinds of number per workload + size:

- **structural proxies** (deterministic): op count and encoded byte size, from Elm
  via the public API (`OpDoc.opCount`, `String.length` of the encoded JSON). These
  are attributable to a _structure_ — compare the per-container workloads.
- an absolute **heap** figure: the Elm worker retains N built docs in its model, so
  their live heap sits in the shared V8 process and `process.memoryUsage().heapUsed`
  sees it. GC-noisy, so force GC (`--expose-gc`) and average.

```sh
npm run mem   # env knobs: SIZES=100,400  WORKLOADS=demo,text,list,dict,tree  HEAP_COPIES=20
```

Workloads: `demo` (realistic mixed doc), `text`, `list`, `dict`, `tree` (each
per-container, for attribution). The `roundtrip` command flag retains a
decoded-from-wire copy (fresh `ReplicaId` per op) instead of a locally-built one
(shared `ReplicaId` reference), to measure the received-doc overhead. **Always build
with `--optimize`** — dev mode adds debug wrappers that skew heap. See
`docs/02-oplog.md` "Memory / size" for the findings.

Headline: text is the _cheapest_ structure per op; per-op `OpId` metadata dominates.
The received-doc heap carries a replica string per op (text +67%, demo +20% over the
built doc). We **tried** a wire replica-table to intern it: it cut encoded size ~14%
(a real bandwidth win) but did **not** reduce heap — the `Array.get`-shared
`ReplicaId` sharing didn't survive into the retained structure — so it was reverted
as not worth the codec complexity for a wire-only gain. Footprint work is parked
until it's a demonstrated bottleneck.

Caveat: `heapUsed` deltas include `Dict`/V8 bookkeeping and assume linear retention —
trust the _ranking_ between structures, not the absolute per-doc MB.

## Wire-size benchmark (`run-wire.js` + `run-packed.js`)

Decides whether a custom binary wire format is worth building, against the real
baseline of **gzipped JSON** (`node`'s built-in `zlib`). `Headless` `mode:"wire"`
hands back the full-doc encoding + a one-edit delta as JSON strings; the runners gzip
them.

```sh
npm run wire     # raw JSON vs gzip(JSON), full docs + deltas
npm run packed   # + a prototype columnar/delta/varint/interned packed format, gzipped
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
npm run merge    # env: SIZES, WORKLOADS, ITERS
```

**Finding (see `docs/12`):** this is the before/after gate for **incremental merge**.
Before, `merge` re-materialized the whole tree from base on every merge, so cost scaled
with the _document_ (superlinear: ~13–19 ms at n=400, ~78–120 ms at n=1000). After
applying only the added ops (sorted among themselves, not the whole store), cost scales
with the _delta_: **text n=1000 dropped 77.8 → 2.1 ms (37×), dict 82.5 → 6.1 ms (14×),
list/tree ~4×.** The residual list/tree cost is their read-time move-set re-fold, not the
merge. The same change preserves referential identity of untouched subtrees, which is
what lets the demo's `Html.Lazy` views skip re-rendering.

## Read-path (the Phase 2 cache gate — historical)

Read latency is now measured by `npm run bench` (the `read (ms/read)` column). The
original Phase 2 gate — comparing the maintained **cache** against a full
**re-materialization** of the op store — passed: cached reads were O(1)-amortized while
fresh reads grew ~linearly in N (~3× at N=50, ~12× at N=200, widening with N), which is
the divergence the cache exists to create. The standalone `run.js` that produced those
numbers expected `cachedState`/`freshState` helpers since removed from `Headless`; kept
in git history for reference.

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
   O(D·N) in text _replace_ was also fixed (`Rga.visibleIds` orders once).
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
