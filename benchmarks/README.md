# benchmarks

Performance harness for the op-log (`docs/02-oplog.md`, Phase 2).

## Read-path benchmark (the Phase 2 go/no-go gate)

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
