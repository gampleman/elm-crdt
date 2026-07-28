# Roadmap

Forward-looking plan for `gampleman/elm-crdt`. For what the library _already does_, see
the [README](README.md) and the design notes in [`docs/`](docs/).

## Status

**Feature-complete for typed JSON-like collaborative documents.** The document is an
**operation log** with a materialized read model (`Crdt.Doc`); writes are **type-safe**
through schema-emitted `Ref`s in the `Crdt` module. The container set is complete — map,
list, movable list, plain text, rich text (Peritext marks + block structure), counter,
movable tree — with sequence ordering by **Fugue** (concurrent same-position inserts stay
contiguous). At parity with [Loro](https://loro.dev) on containers, delta sync, frontiers,
time-travel, restore, undo/redo, GC, cursors, and explicit fork/branch (`Doc.fork` /
`forkAt` / `divergence`); **ahead** on type-safety (sum/custom types, compile-checked
writes). Schema evolution (read-time tolerance) and user-defined
CRDTs (`opSet`) shipped. Performance: an O(N²)-elimination sweep landed (build/read now
linear); delta ingest is O(delta); **run-length text ops** store a typed run as one op
(exploded to per-char elements at read), cutting bulk-text store/heap/wire and merge cost
by 1–2 orders of magnitude with no read-path change; external benchmarks put us within
~2× of Loro (WASM) on build, faster than Automerge — see
[`benchmarks/results/`](benchmarks/results/).

**Testing** is the pure-Elm suite (convergence/law/property) plus a Playwright
multi-client browser suite (`demo/e2e/`) — real app + relay + N browser replicas,
including two peers editing the same rich-text block concurrently.

## Remaining work

None of these block correctness — on valid input the library already converges (the
Lamport clock yields a valid causal linearization). They're capabilities deliberately left
open after weighing cost against benefit:

- **True O(log n) visible-index access.** A single arbitrary-position edit is O(n) (one
  Fugue-order walk); a *batch* of n is O(n²). The per-op redundancy is already gone (random
  `move` resolves the order once — `MoveList.resolveOrder`, ~2.2× on the `churn` benchmark),
  so what remains is the asymptotic tail. Sub-linear access would need a **balanced
  order-statistics tree** on the convergence-critical Fugue-ordering path — a from-scratch
  structure (Elm's stdlib has none; the Fugue tree degenerates to a length-n right-spine for
  sequential text). Deferred as high-risk for "the tail": single interactive edits are
  already fine, and the O(n²) only shows in bulk programmatic churn.
- **Fully-decentralized GC (regime 3).** Regime-2 stable-frontier GC ships (`Doc.stableFrontier`,
  see below) — safe multi-replica compaction given the connected peers' versions. Regime 3
  drops the assumption of a shared coordinator/relay: a distributed version-vector agreement
  protocol so a fully peer-to-peer swarm can pick a safe cut. A separate project.

Recently shipped (this cycle; details in the linked docs/benchmarks):

- **Constant-factor visible-index speedup** — random `move` resolves the order once (`MoveList.resolveOrder`).
- **Delta ingest is O(delta), not O(n)** — clock catch-up is `max(ctx)` (O(1)) and added-op
  discovery scans the delta, not the store (`OpLog.addedFromCandidates` / `opsMaxCounter`);
  new `npm run delta` benchmark. (Full-doc `merge a b` stays O(n) — it unions two whole
  stores by nature; the demo's real path is delta decode, now flat.)
- **Explicit fork/branch** — `Doc.fork` / `forkAt` / `divergence` (re-keyed branch off a `Version`,
  merge-back = plain `merge`); demo "branch" workflow.
- **Tombstone compaction (GC phase 4)** — a full `compact` rebuilds `Seq`/`Txt`/`Rich` RGAs
  from their live elements, physically dropping dead tombstones (~17× on a heavily-deleted list).
- **Run-length text ops** — `InsertText` stores a typed run as one op, exploded to per-char
  elements at read; big bulk-text store/heap/wire/merge wins, read path unchanged (`text` vs
  `typing` benchmarks make the interactive-vs-bulk distinction explicit).
- **Stable-frontier GC (regime 2)** — `Doc.stableFrontier` + `encodeVersion`/`decodeVersion`;
  demo "compact shared history" across live peers, straggler caught up by snapshot.

## Maybe

- **Event / diff at finer grain.** `mergeWithDiff` + `Crdt.touched`/`origins` already give
  a ref-queryable diff; a richer typed change stream (per-element deltas for animation /
  provenance UI) could build on it.
- **Cambria-style lenses** (migrations Layer 4): declared, versioned lenses for structural
  moves (hoist/split) that read-time tolerance can't express.

## Decisions recorded

- **Binary wire format — investigated, not pursued.** gzip(JSON) already compresses full
  docs 11–16×; a prototype columnar/varint/interned packed format beat it by only 0–18% on
  full docs and ~0% on deltas (`benchmarks/run-wire.js` / `run-packed.js`). Not worth a
  hand-rolled codec at these sizes; the `Crdt.OpJson` seam keeps it a drop-in if bandwidth
  ever becomes a demonstrated bottleneck. Cheap win noted: raw DEFLATE (no gzip framing)
  for tiny deltas saves ~5%.

## Non-goals

- **Multi-language bindings, WASM, columnar arena/memory pooling** — Loro is a Rust engine;
  this is a pure-Elm library by choice.
- **Wire compatibility with Loro** — out of scope from day one.
- **Opening the `Node` union to user-defined variants** or **custom sequence ordering** —
  impossible/unsafe in pure Elm; `opSet` covers the map/set/register family instead.

---

> Design notes live in [`docs/`](docs/): op-log ([`02`](docs/02-oplog.md)),
> stable cursors ([`03`](docs/03-stable-cursors.md)), GC ([`04`](docs/04-gc.md)),
> moves ([`05`](docs/05-move.md)), sum types ([`06`](docs/06-sum-types.md)),
> optics ([`07`](docs/07-optics.md)), tree ([`08`](docs/08-tree.md)),
> Fugue ([`09`](docs/09-fugue.md)), rich text ([`10`](docs/10-rich-text.md)),
> block structure ([`11`](docs/11-block-structure.md)), referential stability + diff
> ([`12`](docs/12-referential-stability-and-diff.md)), migrations
> ([`13`](docs/13-migrations.md)), extensibility ([`14`](docs/14-extensibility.md)).
> [`01-delta-sync.md`](docs/01-delta-sync.md) is the **superseded** state-based delta plan.
