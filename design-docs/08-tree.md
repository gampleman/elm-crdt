# 08 — Tree / MovableTree

**Status:** ✅ **built (ordered MovableTree).** `Crdt.Tree` (move-set + cycle-skip
fold + fractional sibling order via `Crdt.Frac`), wired through `Node`/`Json`/
`OpLog`/`OpJson`/`OpDoc`, `S.tree` schema, and `Crdt.Ref` (`addChild`/`moveInto`/
`moveBefore`/`moveAfter`/`removeNode` + per-node payload refs). Tested:
`tests/FracTests.elm` (8, fuzzed), `tests/TreeCoreTests.elm` (10, incl. the 2- and
3-cycle tests), `tests/TreeTests.elm` (8, public API incl. concurrent-cycle sync),
`tests/TreePropertyTests.elm` (14, structural invariants — see below).
The scope fork below was resolved **ordered** (fractional index).
**Roadmap item:** #1 of the remaining gaps (missing containers).
**Depends on:** `Crdt.Node`, `Crdt.OpLog`, `Crdt.Schema`, `Crdt.Ref`. Mirrors the
`Crdt.MoveList` buildout (new module + `Node` variant + action + schema + refs).

## Problem

Hierarchical, movable data: outlines, file trees, threaded comments, nested
layers. Each node has a **parent** and a **payload**; you can add a node under a
parent, **move** a node to a new parent, and delete a node. Concurrent edits must
converge — and the hard part is unique to trees:

> **Concurrent moves can form a cycle.** Replica A moves node X under Y; replica B
> concurrently moves Y under X. Neither move is individually wrong, but applied
> together they'd make X and Y each other's ancestor — no longer a tree.

Per-node last-writer-wins on the parent pointer does **not** save you: both moves
can win (they touch different nodes), and the result is a cycle. This is the
canonical hard problem in tree CRDTs (Kleppmann et al., 2020, *A highly-available
move operation for replicated trees*).

## The key realization: it's `MoveList`, generalized

`Crdt.MoveList` already solved the linear version of exactly this. Recall its shape
(`design-docs/05-move.md`): store an **append-only** set of position records, merge by
**union** (a trivial semilattice), and derive the visible order as a **pure
function of the record set** — never by mutating state in place. Cycle-safety there
came from ordering being derived, not imperatively applied.

A tree uses the identical strategy:

- **Store an append-only set of `move` records.** A move is
  `{ moveOp : OpId, child : NodeId, newParent : NodeId }`. Every move ever made is
  kept (like MoveList keeps every position *cell*). Merge = set union — a plain
  semilattice, so convergence of the *storage* is free.
- **Derive the tree as a pure function of that set**, by folding the moves **sorted
  by `moveOp` (ascending, `(counter, replica)`)**, applying each, and **skipping
  any move that would create a cycle** (i.e. whose `newParent` is `child` itself or
  a current descendant of `child`).

Why this converges: every replica holds the same move-set after merge, sorts it the
same way, and therefore makes the *same* skip decisions → identical trees. This is
Kleppmann's algorithm, and it lands naturally here because **`Crdt.OpLog` already
folds ops in a deterministic `causalOrder`, and `merge` re-materializes from base** —
so we never need Kleppmann's tricky incremental undo/redo of already-applied moves.
We just re-fold.

**Why fold-with-skip beats per-node-LWW-parent:** with plain LWW parents, a
concurrent X→Y / Y→X makes the X-Y group *unreachable from the root* — it vanishes
from the rendered tree (recoverable, but surprising). Fold-with-skip instead keeps
the losing move's node at its **previous** parent (the last move that didn't form a
cycle), because we replay the whole history. Nothing vanishes. We can afford this
precisely because we keep the move history.

## Node representation

A new `Node` variant, mirroring `Mov`:

```elm
type Node = … | Tree TreeNode
type alias TreeNode = Crdt.Tree Node
```

`Crdt.Tree c` holds:

- `moves : Dict String Move` — the append-only move set, keyed by `moveOp` string
  (so union dedups identical ops). `Move = { child : OpId, parent : Maybe OpId }`
  (`Nothing` parent = a root). A node's *creation* is its first move.
- `payload : Dict String c` — nodeId → content (a `Node` subtree), stable across
  moves, exactly like MoveList's `valueOf`.
- `tombstones : Set String` — deleted nodeIds (grow-only, delete-wins).

Merge (a semilattice, like MoveList): `Dict.union` the moves, recursive-merge the
payloads, `Set.union` the tombstones. Read: fold moves sorted by `moveOp` with
cycle-skip, drop tombstoned nodes, expose the parent→children structure.

This is a genuine semilattice on storage, so it works for **both** the op-log
`materialize` fold *and* the internal state-based `Node.merge` — no flavor split
(the same property that let `MoveList` serve both).

## Actions & wiring (mirrors `MoveElem`)

- `OpLog.Action` gains `TreeMove { container, child, parent }` (parent `Maybe OpId`).
  Creating a node = a `TreeMove` with a fresh `child` id under some parent.
- `Crdt.Json` / `Crdt.OpJson`: encode/decode the move set + payloads + tombstones
  (it's Dicts + a Set — same shapes `MoveList` already serializes).
- `Crdt.Schema.tree : Crdt ek a -> Crdt (TreeK ek a) (Tree a)` — a new kind marker
  `TreeK`, and a typed `Tree a` read value (see the read-shape fork below).
- `Crdt.Ref`: `addChild parentRef` / `moveNode` / `removeNode`, and element refs to
  descend into a node's payload. `moveNode` is the tree analogue of `Ref.move`.

**No change to `Crdt.OpLog` core, `merge`, or `causalOrder`** — like every container
before it, this is a new leaf on the existing machinery.

## THE FORK: ordered siblings, or not?

This is the one decision that materially changes scope, so it's called out on its
own.

**Unordered children** — a parent's children are a *set*, rendered in a
deterministic order (by `NodeId`). Move = set the parent pointer. This is the full
design above and is comparatively small: one move-set, cycle-skip fold, done. Good
for file trees, threaded comments, layer hierarchies where sibling order is
incidental or derived from a field (e.g. sort by name).

**Ordered children (true MovableTree)** — siblings have an explicit, editable order
(drag node 3 above node 1 *within the same parent*), like Loro's tree with its
fractional-index positions. This needs a **per-parent ordering** on top of the
parent pointers — essentially a `MoveList` of children at every node, or a
fractional sort key per node that survives re-parenting. Roughly doubles the
implementation (position allocation, its own concurrent-collision story) and the
read/ref surface (`moveNodeBefore`/`after`, not just `moveNode`).

Loro provides ordered. But for the common tree use cases (files, comments,
outlines-sorted-by-content) unordered-with-a-sort-field is often enough, and it's a
clean, shippable increment that the ordered version can extend later (add sibling
positions without changing the parent/cycle machinery).

## Read shape (depends on the fork, but roughly)

```elm
type alias Tree a =
    { roots : List (TreeItem a) }

type alias TreeItem a =
    { id : NodeId, value : a, children : List (TreeItem a) }
```

Decoded recursively; `children` ordered by NodeId (unordered variant) or by sibling
position (ordered variant). Tombstoned nodes and their now-orphaned subtrees are
omitted (a deleted node's children become roots, or are also dropped — a sub-fork,
default: re-parent survivors to the deleted node's parent, matching filesystem
"delete only this node" vs. "delete subtree"; I'd default to **delete subtree** as
the least surprising, with the recursive tombstone applied at read).

## Test plan (`tests/TreeTests.elm`, mirroring MoveTests/MoveListTests)

1. Core (`Crdt.Tree` in isolation, `Int`/`String` payloads): add/move/delete,
   read structure, payload survives a move (identity).
2. **The cycle test** — concurrent X→Y and Y→X converge to the *same* tree both
   merge orders, and exactly one move is skipped (the higher-`moveOp` one), the
   other node stays put. This is the headline correctness property.
3. Concurrent moves of *different* nodes both apply.
4. Delete-wins over a concurrent move; subtree handling.
5. Merge laws (commutative/idempotent on the read) + JSON round-trip.
6. Public API via `Ref` + `OpDoc`: build a tree, move a node, read it back;
   nested payload edit follows a moved node.
7. Deep tree doesn't stack-overflow the read fold (cf. the RGA 20k regression).

## Undo/redo (delete → undo → redo → undo)

Tree deletes make undo subtle for two reasons, both handled in `Crdt.OpDoc`:

1. **A deleted subtree must revive as one atomic step.** Undoing a delete re-creates
   the node *and its whole subtree*. If that were N separate re-create ops, its own
   inverse (a redo) would be N deletes rather than the one delete it came from, and
   the undo/redo counts would drift out of sync. So the delete inverts to a single
   `ReGraft` reverse-action, and `reviveNode` walks the subtree beneath it — one
   undo step in, one delete out. Nodes are revived at their *original* sibling
   position (`Tree.siblingPos` from the pre-delete tree), not appended, so order is
   preserved.
2. **Tombstones are permanent, so a revive mints fresh ids.** The revived subtree is
   a *copy*: same structure and payloads, new `OpId`s. That breaks a later inverse
   that still names the *original* id — e.g. `delete X`, `undo` (revives `X'`),
   then undoing the op that first *created* `X` would target the dead `X`, orphaning
   `X'`. `OpDoc` fixes this with an **id-remap table** (`originalId → revivedId`,
   Loro's UndoManager trick): every revive records the mapping, and every subsequent
   inverse op resolves its ids through it (transitively). This is not tree-specific —
   list/text delete-undo revives a fresh element the same way and remaps identically.

Covered by `tests/TreeTests.elm` (delete→undo restores the subtree in place;
delete→undo→redo re-deletes) and by `tests/UndoPropertyTests.elm`, which fuzzes
random edit sequences over a tree + movable-list + text and asserts full unwind,
undo/redo round-trip, and ping-pong stability.

## Structural invariants, fuzzed at the op level (`tests/TreePropertyTests.elm`)

The tests above compare documents to *each other* — two replicas converge, a payload
survives a move, the read is unchanged by compaction. That is the wrong oracle for the
claims this module makes on its own: two replicas that both drop a node, or both loop
forever, agree perfectly. So there is now a property module against
`Crdt.Tree.Internal` directly, split by what kind of input justifies which claim:

- **An arbitrary move-set** (`Helpers.fuzzTreeValue` — move set, payload table and
  tombstones generated independently, ids from a pool of three, so cycles, dangling
  parents and payload-less moves are all routine). This shape comes off the wire, so the
  read only has to be **total**: `toForest` terminates and yields each node once, every
  item is live and has a payload, and — separately — every node's `parentOf` chain
  terminates *within a bounded number of steps*.
- **A scripted tree** — a fuzzed list of concrete `move`/`moveOnly`/`delete` calls, which
  is exactly what folding tree ops does (`OpLog.treeMove` is a three-line wrapper). Only
  here can the semantic claims be stated: created ⇒ readable (so no move ever removes a
  node from the read), the forest is a function of the op **set** and not its order,
  deleting takes exactly one subtree, and `maxCounter` covers every id.

Four things worth recording, all found by writing the properties rather than by a failure:

1. **Three read paths re-implement the same admission rule** (live, payload present, no
   tombstoned ancestor): `toForest`'s one-pass child index, `childIdsOf`, and
   `lastChildPos`'s single fold. They are pinned against each other, and it turned out
   they have to be pinned over *both* input shapes — inverting `lastChildPos`'s max to a
   min passes the arbitrary fuzzer (which almost never lands two payload-bearing siblings
   under one live parent) and fails only on the scripted one. A sibling *group* is the
   only place a comparison can be backwards.
2. **`childIdsOf`'s ancestor climb is reachable only through a stale id** — and that is a
   normal occurrence, not an edge case: a `Ref` holds a node's `OpId` and a peer can
   delete that node before the ref is used. Without the climb, `childrenOf` reports the
   children of a deleted node and an index resolves inside a subtree the read has already
   dropped. `toForest` gets the rule for free (it never descends through a dead node), so
   the two are equivalent only while the climb is there. Pinned as "a deleted node's
   subtree is inert to every read path". (`get` is deliberately narrower — own tombstone
   only — so a nested edit already in flight into a doomed subtree still resolves.)
3. **`resolve`'s sort is not redundant with the `Dict`'s key order.** The move set is keyed
   by `Id.opIdToString`, so key order and `compareOpId` order agree for counters 1–9 and
   then diverge (`"10@m"` sorts before `"2@m"`). Removing the sort therefore passes every
   small fixture — including the 2- and 3-cycle tests — and changes the resolved tree only
   once a document has ten ops. There is now a fixture that straddles that boundary
   deliberately.
4. **A cycle is unreachable from a root**, so `toForest` cannot hang on one even if
   `resolve` stopped skipping cycle-forming moves (every member's parent is another
   member, so no root walks into it). The paths that *would* hang climb upward:
   `wouldCycle` carries a defensive `seen` set, `ancestorTombstoned` carries nothing and
   rests entirely on `resolve` having done its job. Hence the bounded-climb property over
   every node any move names, readable or not — a hang is a worse failure than a red test,
   and it would arrive in a peer's message.

## Risks

- **Read cost:** the fold is O(moves) and cycle-check per move is O(depth); fine for
  interactive trees, and GC-able (compact the move-set to one move-per-live-node
  below a stable frontier, like list/RGA compaction). Note it in the module docs.
- **Recursion depth** on read for deep trees — use an explicit stack, as `Rga`'s
  ordering sweep already does.
