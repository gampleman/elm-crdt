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
