# elm-crdt vs Automerge vs Loro vs no-CRDT — 2026-07-15

Same operations, same Node process (v24), median ms. elm-crdt via `run-bench.js`
(compiled Elm `--optimize`); Automerge 2.x (`next` API) + Loro 1.x via `run-compare.js`.

**The no-CRDT baseline is plain Elm** — the same edits on a plain immutable `Board` record
(no `Doc`), run through the identical Headless harness — because that's the honest "what
does adding CRDTs to *my Elm app* cost". Plain Elm's immutable `List`/`String` are ~35×
slower than plain JS's mutable arrays, so plain JS would flatter us unfairly; we use plain
Elm.

Automerge & Loro are **Rust compiled to WASM**; elm-crdt is **Elm compiled to JS**. So a
constant-factor gap to Loro is expected and fine — the question is *how large*, and
whether we're algorithmically competitive (same O()).

## list build — append N `{text, done}` items (ms)

| n    | plain Elm | Loro (WASM) | elm-crdt | Automerge (WASM) |
|------|----------:|------------:|---------:|-----------------:|
| 100  | 0.05      | 0.76        | 1.03     | 6.05             |
| 400  | 0.06      | 2.00        | 3.52     | 14.4             |
| 1000 | 0.14      | 4.22        | 7.98     | 35.6             |

All three CRDTs scale **linearly**. elm-crdt is ~**1.9× Loro** and ~**4.5× faster than
Automerge** (within 2× of the fastest Rust/WASM lib, from Elm), and ~**57×** a plain Elm
list build.

## text build — insert N chars, one op/char (ms)

| n    | plain Elm | Loro (WASM) | elm-crdt | Automerge (WASM) |
|------|----------:|------------:|---------:|-----------------:|
| 100  | 0.02      | 0.16        | 0.18     | 1.38             |
| 400  | 0.05      | 0.53        | 0.54     | 6.11             |
| 1000 | 0.13      | 1.16        | 1.29     | 20.1             |

Text is our strongest: **on par with Loro** (~1.05–1.1×), **~15× faster than Automerge**,
~**10×** a plain Elm string build. The Fugue append fast-path (earlier work) pays off —
per-char build is effectively as fast as the WASM lib.

## Cost of collaboration vs no CRDT (plain Elm)

The same 1000 edits on a plain immutable Elm `Board` vs through elm-crdt, same harness:

| n=1000 | plain Elm | elm-crdt | overhead | per-op |
|--------|----------:|---------:|---------:|-------:|
| list build | 0.14 | 7.98 | ~**57×** | 8.0 µs/op |
| text build | 0.13 | 1.29 | ~**10×** | 1.3 µs/char |

Large as a ratio, but sub-frame in absolute terms — and it buys convergence, per-op
identity, history/undo, and delta sync. Only matters at very large N or tight batch loops.

## read (materialize whole value) — NOT apples-to-apples

| n=1000 | plain | Automerge | Loro (.toJSON) | elm-crdt |
|--------|------:|----------:|---------------:|---------:|
| list read | ~0 | ~0 (lazy proxy) | 0.72 | 4.6 |

Automerge returns a **lazy proxy** (near-zero until you touch fields), so its "read" isn't
comparable — it defers the cost. Loro's `.toJSON()` fully materializes, like our `read`;
against that fair baseline we're ~**6× Loro**. Our read builds a fully-typed Elm value
every call (and is cached O(1)-amortized within a doc version). This is the widest gap and
the natural next optimization target if read-heavy workloads matter.

## Takeaways

- **Algorithmically competitive**: linear build like both WASM libs; no asymptotic
  disadvantage after the O(N²) sweep.
- **Build**: within ~2× of Loro (the fastest), several× faster than Automerge — very good
  for Elm-vs-Rust/WASM.
- **Text**: essentially at parity with Loro.
- **Read/materialize** is our relative weak spot (~6× Loro) — full typed re-materialization
  vs a JSON walk. Candidate for future work if profiling a real app shows read dominating.
