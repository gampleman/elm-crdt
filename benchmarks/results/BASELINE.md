# Optimization baseline — 2026-07-15 (commit 4f680d0, Elm --optimize, Node)

Measure-first snapshot before the optimization sweep. Times are wall-clock in Node;
`ms/read` subtracts the shared build cost.

## Latency

| workload | n    | build (ms) | read (ms/read) |
|----------|------|-----------:|---------------:|
| demo     | 100  |      65.4  |         5.20   |
| demo     | 400  |    1014.3  |        57.82   |
| text     | 100  |       0.09 |         0.12   |
| text     | 400  |       0.35 |         0.58   |
| text     | 1000 |       1.14 |         1.65   |
| list     | 100  |       8.88 |         0.41   |
| list     | 400  |     174.9  |         1.95   |
| dict     | 100  |       1.49 |         0.057  |
| dict     | 400  |      29.3  |         0.23   |
| dict     | 1000 |     203.2  |         0.64   |
| tree     | 100  |       7.59 |         3.82   |
| tree     | 400  |     155.8  |        62.13   |

## Merge (ms/merge, small remote delta into size-n doc)

| workload | n=100 | n=400 |
|----------|------:|------:|
| demo     | 1.92  | 22.56 |
| text     | 0.055 | 0.305 |
| list     | 0.184 | 2.065 |
| dict     | 0.072 | 0.562 |
| tree     | 0.152 | 1.934 |

## Wire (n=100): text full 19546 raw / 1649 gzip (11.9x); delta 1047 raw / 226 gzip.

## Scaling verdict (build cost as n grows)

- text: ~LINEAR and cheap. Not a target.
- dict: SUPERLINEAR — build 1.5 → 29 → 203 ms (100/400/1000). O(N^2)-ish.
- list: SUPERLINEAR — 8.9 → 175 ms (100→400 = 4x size, 20x time). O(N^2).
- tree: SUPERLINEAR in BOTH build (7.6 → 156 ms) and read (3.8 → 62 ms/read). Worst.
- demo: bundles the above; dominated by list+tree+dict.

Hotspot ranking for the sweep: (1) tree read, (2) tree/list/dict build (shared
`toElementsInOrder` / move-set re-fold O(N) per op), (3) demo as the aggregate.

---

## After fixes 1–4 (toForest single-pass, endPos single-resolve, frontier cache, isOrdered O(1))

| workload | build 100 | build 400 | build 1000 | read/ read 400 |
|----------|----------:|----------:|-----------:|------:|
| text | 0.24 | 0.51 | 1.36 | 0.56 |
| list | 0.92 | 3.28 | 7.98 | 1.80 |  (was 8.9/175/940 — ~118x @1000, now LINEAR)
| dict | 0.12 | 0.50 | 1.25 | 0.22 |  (was 1.5/29/203 — ~162x @1000, now LINEAR)
| tree | 2.84 | 50.2 | 438  | 2.74 |  (read was 62ms→2.7 = 22x; build still O(N^2))

Wins so far:
- **tree READ: 62 → 2.7 ms/read @400 (22x)** — `toForest` resolves + buckets once, walks O(N).
- **list build: 940 → 8 ms @1000 (~118x), now LINEAR** — `isOrdered` was materializing the
  whole element order per append (O(N²)); now an O(1) constructor test.
- **dict build: 203 → 1.25 ms @1000 (~162x), now LINEAR** — shared `OpLog.frontier` rescan
  per op (O(N)); now an incrementally-maintained cached frontier.
- **tree build**: halved (`endPos` resolves once not twice) but STILL O(N²) — the remaining
  target: `endPos`→`resolve` per addChild.

---

## FINAL — all fixes (adds tree addChild fast-path)

Build latency, ms (baseline → final), and scaling:

| workload | n=400 baseline | n=400 final | n=1000 final | scaling now |
|----------|---------------:|------------:|-------------:|-------------|
| text | 0.35 | 0.51 | 1.54 | linear |
| list | 174.9 | 3.3 | 9.2 | **linear (was O(N²), ~53x @400)** |
| dict | 29.3 | 0.50 | 1.55 | **linear (was O(N²), ~59x @400)** |
| tree | 155.8 | 3.5 | 12.5 | **linear (was O(N²), ~45x @400)** |
| demo | 1014 | 54.7 | — | **~18x @400, near-linear** |

Read latency, ms/read (baseline → final):

| workload | n=400 base | n=400 final |
|----------|-----------:|------------:|
| tree | 62.1 | 2.6 | **(24x)** |
| demo | 57.8 | 11.7 | (5x) |
| list/dict/text | ~2/0.2/0.6 | ~unchanged (already linear) |

Merge, ms/merge @400 (baseline → final): demo 22.6→13.1, list 2.07→0.50, tree 1.93→0.49.

## The four fixes (all measured, all keep 321 tests green)

1. **`Tree.toForest` single-pass** — was re-`resolve`ing the whole move-set at every
   node (O(N² log N)); now resolves + buckets children once, walks O(N). Tree read 24x.
2. **`endPos` single resolve** + **`Tree.lastChildPos`** — halved tree-build resolve count.
3. **Cached frontier** (`Doc.frontier` + `OpLog.advanceFrontier`) — `OpLog.frontier`
   rescanned the whole store per edit (O(N) → O(N²) build); now maintained incrementally.
   Fixed dict entirely; helped all.
4. **`isOrdered` O(1)** — was materializing the whole element order per `listAppend` just
   to check "is this a sequence"; now a constructor test. Fixed list entirely (~118x @1000).
5. **tree `addChild` fast-path** (`Doc.lastTreeChild`) — a run of appends under one parent
   skips `endPos`'s `resolve`; tree build 45x, now linear.

Everything is now **O(N) / O(N log N)** to build and read. No remaining O(N²) in the
core container paths.

---

## Random-index churn (arbitrary-position `move`)

The `churn` workload builds a movable list of `n`, then does `n` pseudo-random `move`s —
each `move` resolves the item id at `from`, the destination anchor, and a home-cell map.
A single `move` is O(n) (one Fugue-order walk); a batch of `n` is O(n²). This is the
last O(n²) surface (`WORKLOADS=churn npm run bench`).

Constant-factor fix — resolve the list order **once** per op (`MoveList.resolveOrder`)
instead of walking it 2–3× (index-of-`from` + destination anchor + home cells):

| n   | before (ms) | after (ms) |
|-----|------------:|-----------:|
| 100 |        49.7 |       22.8 |
| 200 |       213.8 |       97.5 |
| 400 |       966.4 |      431.5 |
| 800 |      4303   |     1928   |

~2.2× across the board; still O(n²) (quadruples per doubling). True O(log n) would need a
balanced order-statistics tree on the convergence-critical Fugue-ordering path — deferred
(see ROADMAP): single interactive edits are already O(n) and fine; only bulk programmatic
churn hits the quadratic.

---

## Delta ingest — the demo's per-message path (`npm run delta`)

`decodeInto` a one-edit `encodeSince` delta into a size-`n` doc. The incremental merge
(design-docs/12) already folded only the added ops onto the cache, so the *fold* was delta-bound
— but two pieces of bookkeeping still scanned the whole doc on every merge/decode:

1. **Clock catch-up** — `Node.maxCounter cached` walked the entire materialized tree to
   advance the Lamport clock (the top named hotspot in the merge profile).
2. **Added-ops discovery** — `addedOpsInOrder` folded over the whole *merged store* to
   find which ops were new.

Both are now delta-bound. Clock: `merge` takes `max(local, incoming)` ctx counter (O(1) —
each `Doc` keeps its `Ctx` ≥ its own stamps, seeds included); the decode path catches up
over just the incoming ops' stamps (`OpLog.opsMaxCounter`, incl. seeds). Added ops: the
decode path filters the incoming batch against the prior store (`addedFromCandidates`,
O(delta)) instead of scanning the merged store.

Delta-decode latency, ms (measured with build cost amortized — high `ITERS`):

| workload | n=100 | n=500 | n=2000 | scaling |
|----------|------:|------:|-------:|---------|
| list | 0.012 | 0.015 | 0.030 | **flat (was ~0.08→0.24→1.15, O(n))** |
| text | 0.018 | 0.019 | 0.030 | **flat** |
| tree | 0.011 | 0.017 | 0.052 | **flat** |
| demo | 0.032 | ~0.07 | ~0.07 | **flat** (converges as build amortizes) |

Note: full-document `merge a b` is still O(n) — it unions two whole op stores
(`Dict.union`) and rediscovers the added ops from the merged store, both intrinsic to
merging two *complete* docs. The clock fix still roughly halved it (list @1000: 3.11→1.50
ms). The demo's real transport is delta decode (above), which is now flat.

---

## Run-length text ops (`WORKLOADS=text`)

Text used to store **one `InsertElem` op per character** (each a single-char `Reg` seed in
the Fugue RGA). A new `InsertText` op stores a whole typed run as `{ start, text, parent,
side }` and `applyOp` **explodes** it into the identical per-char right-spine at
apply-time: char `i` gets the derived id `start+i`, so a concurrent insert anchoring
mid-run references an id the explosion already created — no concurrent-split logic, `==`
convergence preserved, and the materialized value / cursors / marks are byte-identical.

The store shrinks (fewer, smaller ops) while the cache is unchanged — so CPU and memory
BOTH improve; no trade. Bulk-text workload (a doc's text typed in one shot), before → after:

| metric @ n=2000 | before (per-char) | after (run-length) | change |
|-----------------|------------------:|-------------------:|--------|
| build CPU (ms) | 2.74 | 0.91 | **3.0× faster** |
| read CPU (ms) | 3.52 | 3.50 | unchanged (cache identical) |
| delta-decode (ms) | 0.038 | 0.021 | 1.8× faster |
| full merge (ms) | 4.26 | 0.032 | **~130× faster** |
| ops stored | 1960 | 2 | ~1000× fewer |
| raw wire (bytes) | 406 099 | 2 248 | ~180× smaller |
| live heap (bytes) | 1 348 096 | 758 654 | **1.78× smaller** |

### Where the win does and doesn't land (don't oversell it)

The dramatic numbers above are the **bulk** case — a whole value set at once
(load / paste / `set` a string / programmatic edit / decode), where one `set` diffs to one
contiguous insert → **one** run-length op. **Interactive char-by-char typing** is
different: each keystroke is its own `set` with a 1-char diff → a 1-char run → **still one
op per character**. Run-length changes nothing about op *count* there; it only makes each
op a little smaller (an `itxt` carries `start`+`text` vs an `ins`'s `elemId`+parent+`Reg`
seed node), and read latency is unchanged in every case (the materialized per-char cache is
identical).

The `text` (bulk) vs `typing` (keystroke-by-keystroke) workloads make this explicit
(`WORKLOADS=text,typing`), n=1000, same ~980 characters:

| workload | ops stored | raw bytes | b/op |
|----------|-----------:|----------:|-----:|
| `text` (whole-value set) | **2** | 1 268 | run-length: one op for the whole run |
| `typing` (per keystroke) | **979** | 143 078 | one op per char — run-length gives no count reduction |

So: paste / load / bulk / programmatic text → order-of-magnitude store/heap/wire/merge wins;
interactive typing → op count unchanged, ops marginally smaller, nothing slower. (Build-CPU
for `typing` isn't tabulated: the harness re-diffs the growing string on every simulated
keystroke, so that timing measures the O(n²) *harness loop*, not the library's op path.)

---

## Causal-order linearization (`OpLog.causalOrder`) — history scrubbing

`causalOrder` linearizes the op store for `materialize` / `checkout` / `versionAt`. It was a
Kahn topological sort whose per-step "find all ready ops" did a `Dict.filter` + a
dependent-decrement `Dict.map` over the whole store — **O(n²)**. `versionAt` and
`historyLength` each call it, so history scrubbing (which calls `versionAt` per drag tick)
crawled once the log passed a few hundred ops.

Replaced with a single ascending-`OpId` sort — **O(n log n)**. This is a valid causal order
by construction: an op's `deps` are the frontier it was minted against, and `Id.nextId`
takes a counter strictly greater than everything observed (deps included), so every dep has
a smaller `OpId` than the op. Ascending id order therefore places every op after its deps —
exactly a causal order — and equals the old sort's output (which also emitted lowest-ready-id
each step, and fell back to "remaining by id" for the Lamport-impossible cycle case).
Convergence/law/fuzz suites stay green.

(The demo compounded this: `scrubPosition` inverted version→step by calling `versionAt`
across every step each render — O(n) × the sort. Fixed separately by storing the scrub step
in the model, making it O(1); the library fix above is what makes each `versionAt` cheap.)

---

## Pending-ops gate (`OpLog.canApply`) — cost of the precheck

The pending-ops precheck (`design-docs/15-pending-ops.md`) adds a test per op in the fold:
can this op's target actually be reached? A/B on the same tree, gate ON vs the call site
neutered to `if True ||`, `npm run scrub` (ITERS=50, build subtracted):

| workload | n | ms/readAt, gate ON | gate neutered |
|----------|--:|-------------------:|--------------:|
| `deletes` (half the log is `DeleteElem`) | 1000 | **5.16** | 5.76 |
| `deletes` | 400 | 1.95 | 2.12 |
| `demo` | 1000 | 29.19 | 30.22 |
| `list` | 1000 | 3.42 | 3.31 |

**No measurable cost**, including in the worst case the gate can have (`deletes` on the
re-fold path — deletes are gated, and `readAt` re-folds the whole log rather than reading the
cache). Differences land on both sides of zero, i.e. noise. `merge`, `delta` and `bench`
likewise show no change, which is expected: `canApply` skips the target walk entirely unless
the action is gated (`DeleteElem`/`MoveElem`/`AddMark`) or the target contains an `IntoElem`
step, so registers, text and inserts never pay for it, and local commits don't go through it
at all.

Two things this run measured incidentally, both pre-existing:

- **`deletes` build is O(n²)** — 9 ms at n=100, 150 ms at 400, **1050 ms at 1000**. That's
  `remove` resolving a visible index by walking the RGA, the same tail `churn` shows for
  `move`; it is a *build* cost, not a fold cost (which is why `run-scrub.js` subtracts a
  build).
- **`readAt` on a delete-heavy log is superlinear** — 0.42 / 1.95 / 5.16 ms at n = 100 / 400
  / 1000 (~2.6× per 2.5× in n, and n here counts inserts while the log also carries n/2
  deletes). Fine at demo scale, worth a look if scrubbing a delete-heavy document ever
  matters.

---

## Typed sequence content (design-docs/16) — measured

`Txt` became `Rga String` and `Rich` became `Rga RichElem`, so a character no longer rides
inside a whole LWW register with a stamp of its own, and a block marker is no longer a magic
`PInt` in one. Element ids, anchors and tombstones are untouched — only the **content**.

**Wire.** Only the `Node` *state* encoding changed; the op encoding did not (`itxt` already
carried a plain string). The state encoding travels in exactly one payload — a compacted
**snapshot**, sent to a peer behind our compaction boundary — which the harness did not
measure at all. `run-wire.js` now has a snapshot column, and the before/after was computed
element-by-element on real snapshot data:

| payload | per element | all text elements | whole snapshot |
|---|---:|---:|---:|
| `text` n=2000 (1959 chars, one rich file) | 120 → 72 B | 231 830 → 138 878 | 234 336 → 141 384 (**−40%**) |
| `demo` n=400 (11 327 chars over 841 rich files) | 120 → 72 B | 1 358 038 → 757 720 | 1 583 188 → 982 870 (**−38%**) |
| `typing` n=1000 (979 chars, plain text) | 122 → 67 B | 121 422 → 67 238 | 122 881 → 68 697 (**−44%**) |

Plain `Txt` gains most (`"c":"a"` becomes just `"a"`). The saving is proportional to how much
of a document is text, so a text-free document sees nothing.

**Heap.** `text` n=2000 live heap **758 654 → 597 164 B (−21%)**, same harness
(`WORKLOADS=text SIZES=2000 npm run mem`), comparable to the run-length row above. One
`Register` record plus one `OpId` per character, gone.

**Latency.** `text` build @400 0.51 → 0.20 ms (less to allocate per character); read, merge,
and every other workload unchanged within run-to-run variance:

| workload | build 100 / 400 / 1000 (ms) | read @400 (ms) | merge @400 (ms) |
|---|---|---:|---:|
| demo | 14.0 / 55.8 / 142.9 | 11.41 | 0.711 |
| text | 0.05 / 0.21 / 0.54 | 0.56 | 0.005 |
| list | 0.92 / 3.73 / 9.31 | 1.73 | 0.150 |
| dict | 0.23 / 0.89 / 2.20 | 0.22 | 0.121 |
| tree | 0.86 / 3.65 / 10.95 | 2.65 | 0.153 |

All still linear in `n`. Caveat on the two before-numbers quoted above (heap 758 654 and the
`text` build 0.51): they come from the earlier rows in this file rather than a same-session
A/B, so machine and Node version may differ — the wire figures are exact, being computed from
the same bytes.
