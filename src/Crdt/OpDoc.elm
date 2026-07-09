module Crdt.OpDoc exposing
    ( OpDoc, Error(..)
    , init, read, merge
    , setText, setBool, setInt, setString, increment
    , listAppend, listRemove, listMove
    , setKey, removeKey
    , opCount, cacheConsistent
    , cursorAt, cursorOffset, cursorRange
    , Version, version, readAt
    , historyLength, versionAt, restoreTo
    , recordEdit, undo, redo, canUndo, canRedo
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
    , gc
    , encode, encodeSince, decodeInto
    , seedNodeAt, subValue
    , treeAddChild, treeMoveInto, treeMoveBefore, treeMoveAfter, treeRemove
    )

{-| An op-log-backed document: the public surface over `Crdt.OpLog`.

Unlike the state-based `Crdt`/`Crdt.Edit` (where the `Node` tree is the source of
truth), an `OpDoc` _is_ an op store. Edits don't mutate state — they resolve a
visible-index `Path` against the **current materialized state**, emit ops, and
append them to the log. `read` materializes the log through the schema; `merge`
is op-store union.

This proves the op-log is usable end to end through a real public API. It mirrors
`Crdt.Edit`'s signatures (path + caller-supplied `Seed` for inserts), so the demo
can migrate with minimal churn. It does not yet replace the state-based `Crdt` —
both coexist during the migration (see `docs/02-oplog.md`).

@docs OpDoc, Error
@docs init, read, merge
@docs setText, setBool, setInt, setString, increment
@docs listAppend, listRemove, listMove
@docs setKey, removeKey
@docs opCount, cacheConsistent
@docs cursorAt, cursorOffset, cursorRange
@docs Version, version, readAt
@docs historyLength, versionAt, restoreTo
@docs recordEdit, undo, redo, canUndo, canRedo
@docs Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
@docs gc
@docs encode, encodeSince, decodeInto
@docs seedNodeAt, subValue
@docs treeAddChild, treeMoveInto, treeMoveBefore, treeMoveAfter, treeRemove

-}

import Array
import Crdt.Cursor as Cursor exposing (Cursor)
import Crdt.Frac exposing (Frac)
import Crdt.Id as Id exposing (Ctx, OpId, ReplicaId)
import Crdt.Internal as I exposing (Seed)
import Crdt.Json as Json
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.OpJson as OpJson
import Crdt.OpLog as OpLog exposing (Action(..), Op, OpStore, Target, TargetStep(..))
import Crdt.Path as Path exposing (Path, Seg(..))
import Crdt.Rga as Rga
import Crdt.Schema as Schema exposing (Crdt)
import Crdt.Tree as Tree
import Dict
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Set


{-| An op-log document for a schema `a`: the op store, the local clock, the
schema, the empty-tree materialization `base`, and a **cached materialized
state** (`cached`) so `read` is O(1) rather than O(ops).

The cache is kept correct incrementally. A local edit's op causally follows
everything already in the store (its `deps` is the current frontier), so
`materialize (store ++ localOp) == applyOp (materialize store) localOp` exactly —
local ops are folded straight onto `cached` without a re-materialization. A
`merge` may interleave ops causally anywhere in the DAG, so it conservatively
re-materializes from `base`. Merges happen at network frequency, edits at
keystroke frequency, so this keeps the hot path O(1).

-}
type OpDoc a
    = OpDoc
        { -- the schema's decoder, kept kind-erased so `OpDoc a` needs no kind
          -- param (the root schema's edit-kind is irrelevant once stored — an
          -- `OpDoc` is only ever *read* through it, and edited via `Ref`s the
          -- caller holds separately).
          decode : Node -> Result Schema.Error a
        , base : Node
        , store : OpStore
        , ctx : Ctx
        , cached : Node

        -- The causal cut that `base` already incorporates. Starts empty (base =
        -- the schema's empty tree). `gc` folds ops at-or-below a frontier into
        -- `base` and advances this; it's the boundary below which history (and
        -- time-travel) has been compacted away.
        , baseFrontier : OpLog.Frontier

        -- Single-slot append fast-path: the (list target, id of the element we
        -- last appended there). Lets consecutive appends to the same list skip
        -- the O(n) `lastVisibleId` re-ordering. Lives here (never `==`-compared),
        -- never in Node/Rga, so it can't corrupt the convergence oracle. Any
        -- other mutation (`commit`) or a `merge` clears it.
        , lastAppend : Maybe ( List TargetStep, OpId )

        -- Named checkpoints (most recent first). Each pins a `Version` (a causal
        -- frontier) with a label + author, so a checkpoint is collaborative
        -- time-travel, not a local snapshot.
        , checkpoints : List Checkpoint

        -- Loro-style LOCAL undo/redo. Each entry brackets ONE local change with the
        -- versions before/after it. `undo` inverts exactly the ops that change
        -- added (recomputed fresh from the `before → after` range each time, so it
        -- never goes stale as other undos/redos re-mint ids), emitting the inverse
        -- as NEW ops. Because it inverts *specific* ops (delete this element, set
        -- this register) rather than diffing whole states, a peer's concurrent edit
        -- to the same container survives and same-path conflicts resolve by LWW
        -- (the undo, being newer, wins). The reverts are new ops, so they sync.
        --
        -- Local and non-replicated (like checkpoints); preserved across `merge`.
        , undoStack : List UndoEntry
        , redoStack : List UndoEntry

        -- Id remapping for undo/redo. Undoing a *delete* can't resurrect the
        -- original element (tombstones are permanent) — it re-creates a FRESH copy
        -- with a new id. A *later* inverse op that still names the original id (e.g.
        -- undoing the earlier insert that created it) would then miss the live copy
        -- and orphan it. This table records `originalId → revivedId` so every
        -- subsequent inverse op is retargeted onto the copy that actually exists.
        -- Chains resolve transitively (a copy re-revived maps through). Persistent
        -- across undo/redo steps (the original ops it rewrites never go away), and
        -- local like the stacks. This is Loro's UndoManager remapping.
        , idRemap : Dict.Dict String OpId
        }


{-| One undo/redo step: the document versions immediately **before** and **after** a
single tracked edit. Undo inverts the ops in the `before → after` range; the
inverse is recomputed at undo time (from these frozen versions against the live
store), so the entry stays valid no matter what other undos/redos ran in between.
-}
type alias UndoEntry =
    { before : Version, after : Version }


{-| One reverse step produced by inverting an edit's op. `Rev` is an action ready to
emit verbatim (with a fresh op id). `ReInsert` re-creates a list/text element a
delete removed (fresh id — tombstones are permanent). `ReGraft` re-creates a
deleted tree node **and its whole subtree** as one atomic step, so it inverts back
to a _single_ delete (keeping undo/redo symmetric across cycles).
-}
type RevAction
    = Rev Action
    | ReInsert { container : List TargetStep, elemId : OpId, afterValue : Maybe OpId, content : Node }
    | ReGraft { container : List TargetStep, parent : Maybe OpId, rootId : OpId, source : Tree.Tree Node }


{-| A named point in history: a label, the replica that saved it, and the
`Version` (causal frontier) it pins. `readCheckpoint` time-travels to it.
-}
type Checkpoint
    = Checkpoint
        { message : String
        , author : ReplicaId
        , version : Version
        }


{-| Why an edit failed: the path didn't resolve against the current state, or it
pointed at the wrong kind of node.
-}
type Error
    = PathNotFound String
    | WrongNodeType String



-- LIFECYCLE ------------------------------------------------------------------


{-| A fresh, empty op-document for a replica and schema.
-}
init : ReplicaId -> Crdt kind a -> OpDoc a
init replica schema =
    let
        ( base, ctx ) =
            Schema.emptyNode schema (Id.ctx replica)
    in
    OpDoc
        { decode = Schema.decodeNode schema
        , base = base
        , store = OpLog.empty
        , ctx = ctx
        , cached = base
        , lastAppend = Nothing
        , checkpoints = []
        , baseFrontier = []
        , undoStack = []
        , redoStack = []
        , idRemap = Dict.empty
        }


{-| The current materialized `Node` — the maintained cache (no re-fold).
-}
state : OpDoc a -> Node
state (OpDoc d) =
    d.cached


{-| Read the typed value by materializing the log and decoding through the schema.
-}
read : OpDoc a -> Result Schema.Error a
read ((OpDoc d) as doc) =
    d.decode (state doc)


{-| Merge another op-document into this one: op-store union, with the clock
advanced past everything seen so future ids never collide.
-}
merge : OpDoc a -> OpDoc a -> OpDoc a
merge (OpDoc local) (OpDoc incoming) =
    let
        store =
            OpLog.merge local.store incoming.store

        cached =
            OpLog.materialize local.base store
    in
    -- a merge can interleave ops causally anywhere, so re-materialize from base;
    -- a peer's concurrent append may now sit after our cached last id, so the
    -- append fast-path is invalidated. The clock must advance past EVERY stamp,
    -- including register stamps buried in insert-op seeds (which carry counters
    -- higher than the insert op's own id) — otherwise a later local edit to a
    -- peer-created register could mint a losing LWW stamp. `Node.maxCounter`
    -- walks all of them.
    OpDoc
        { local
            | store = store
            , ctx = Id.observe (Node.maxCounter cached) local.ctx
            , cached = cached
            , lastAppend = Nothing
        }


{-| How many operations the document holds. Useful to reason about transport
size / delta minimality without exposing the op representation.
-}
opCount : OpDoc a -> Int
opCount (OpDoc d) =
    List.length (OpLog.ops d.store)


{-| Whether the incrementally-maintained read cache equals a full
re-materialization from scratch. Always `True` for any sequence of edits and
merges — the Phase 2 correctness invariant (see `docs/02-oplog.md`). Exposed
(rather than the raw `Node`s it compares) so the invariant stays checkable
without leaking the internal state type.
-}
cacheConsistent : OpDoc a -> Bool
cacheConsistent (OpDoc d) =
    d.cached == OpLog.materialize d.base d.store



-- WIRE -----------------------------------------------------------------------


{-| Serialize the document for transport as a **full sync**: if the document has
been GC'd (`base` holds compacted history), this is a _snapshot_ — the
materialized base, its frontier, and the live tail ops — so a fresh peer can
catch up even though the early ops are gone. Otherwise it's just the op set.
-}
encode : OpDoc a -> JE.Value
encode (OpDoc d) =
    if List.isEmpty d.baseFrontier then
        opsPayload (OpLog.ops d.store)

    else
        snapshotPayload d.base d.baseFrontier (OpLog.ops d.store)


{-| Serialize only what a peer at `Version` is missing — a **delta**. If that peer
is at or ahead of our compacted `baseFrontier`, the delta is just the ops they
lack (`opsAfter`). If they are _behind_ our `baseFrontier`, the ops they need are
gone — so we send a snapshot (base + frontier + tail) instead.
-}
encodeSince : Version -> OpDoc a -> JE.Value
encodeSince (Version known) (OpDoc d) =
    if frontierCovers known d.baseFrontier then
        -- peer already has everything our base subsumes: a plain op delta
        opsPayload (OpLog.opsAfter known d.store)

    else
        -- peer is behind the compaction boundary: only a snapshot can catch them up
        snapshotPayload d.base d.baseFrontier (OpLog.ops d.store)


{-| Whether `have` (a peer's frontier) already includes every op of `needed`
(our base frontier) — i.e. the peer is not behind our compaction boundary. Each
base-frontier id must appear in the peer's frontier; since frontiers are causal
tips and our base ids are minted-once, set membership is the right check.
-}
frontierCovers : OpLog.Frontier -> OpLog.Frontier -> Bool
frontierCovers have needed =
    let
        haveKeys =
            List.map Id.opIdToString have |> Set.fromList
    in
    List.all (\id -> Set.member (Id.opIdToString id) haveKeys) needed


opsPayload : List Op -> JE.Value
opsPayload ops =
    JE.object
        [ ( "kind", JE.string "ops" )
        , ( "ops", OpJson.encodeOps ops )
        ]


snapshotPayload : Node -> OpLog.Frontier -> List Op -> JE.Value
snapshotPayload base frontier tail =
    JE.object
        [ ( "kind", JE.string "snapshot" )
        , ( "base", Json.encodeNode base )
        , ( "frontier", JE.list Json.encodeOpId frontier )
        , ( "ops", OpJson.encodeOps tail )
        ]


{-| Decode a peer's payload and merge it in. Two shapes:

  - **ops** — union the ops into our store (idempotent), re-materialize.
  - **snapshot** — the peer compacted history we may lack. We union the tail ops
    as usual; and if the snapshot's base is **ahead of ours** (its frontier
    covers our `baseFrontier`, and we're not already past it), we adopt the
    snapshot's base + frontier, dropping our now-redundant ops below it. A peer
    that is _not_ behind the snapshot ignores the base and just takes the ops.

The cache is re-materialized and the clock advanced past everything seen.

-}
decodeInto : JE.Value -> OpDoc a -> Result String (OpDoc a)
decodeInto value (OpDoc d) =
    JD.decodeValue payloadDecoder value
        |> Result.mapError JD.errorToString
        |> Result.map (\payload -> applyPayload payload (OpDoc d))


type Payload
    = OpsPayload (List Op)
    | SnapshotPayload Node OpLog.Frontier (List Op)


payloadDecoder : Decoder Payload
payloadDecoder =
    JD.field "kind" JD.string
        |> JD.andThen
            (\kind ->
                case kind of
                    "ops" ->
                        JD.map OpsPayload (JD.field "ops" OpJson.opsDecoder)

                    "snapshot" ->
                        JD.map3 SnapshotPayload
                            (JD.field "base" Json.nodeDecoder)
                            (JD.field "frontier" (JD.list Json.opIdDecoder))
                            (JD.field "ops" OpJson.opsDecoder)

                    other ->
                        JD.fail ("unknown payload kind: " ++ other)
            )


applyPayload : Payload -> OpDoc a -> OpDoc a
applyPayload payload (OpDoc d) =
    case payload of
        OpsPayload incomingOps ->
            rebuild (List.foldl OpLog.insert d.store incomingOps) d.base d.baseFrontier (OpDoc d)

        SnapshotPayload snapBase snapFrontier tailOps ->
            if frontierCovers snapFrontier d.baseFrontier && not (frontierCovers d.baseFrontier snapFrontier) then
                -- the snapshot is strictly ahead of our base: adopt it, keep only
                -- our ops the snapshot doesn't already subsume, plus the tail.
                let
                    keptOps =
                        OpLog.opsAfter snapFrontier d.store

                    store1 =
                        List.foldl OpLog.insert OpLog.empty (keptOps ++ tailOps)
                in
                rebuild store1 snapBase snapFrontier (OpDoc d)

            else
                -- we're at/ahead of the snapshot's base: ignore it, take the tail
                rebuild (List.foldl OpLog.insert d.store tailOps) d.base d.baseFrontier (OpDoc d)


{-| Re-materialize from a (possibly new) base + store and advance the clock.
-}
rebuild : OpStore -> Node -> OpLog.Frontier -> OpDoc a -> OpDoc a
rebuild store base baseFrontier (OpDoc d) =
    let
        cached =
            OpLog.materialize base store
    in
    OpDoc
        { d
            | store = store
            , base = base
            , baseFrontier = baseFrontier
            , cached = cached
            , ctx = Id.observe (Node.maxCounter cached) d.ctx
            , lastAppend = Nothing
        }



-- HISTORY / TIME-TRAVEL ------------------------------------------------------


{-| A point in the document's shared history — the causal frontier at some
moment. Unlike the local snapshot stacks of `Crdt.History`, a `Version` is
**collaborative**: it is derived from the op DAG, so any two peers that hold the
same ops agree on it, and it can be carried, stored, and checked out later.

A `Version` also doubles as a **branch handle** — checking out a version and
continuing to edit from the live document, then comparing, is the basis for
fork/branch workflows.

-}
type Version
    = Version OpLog.Frontier


{-| The current version (the live frontier). Capture it before an edit to be able
to return to "the state as of now" later.
-}
version : OpDoc a -> Version
version (OpDoc d) =
    case OpLog.frontier d.store of
        [] ->
            -- store empty (fresh, or fully compacted): the version is whatever
            -- `base` already incorporates.
            Version d.baseFrontier

        f ->
            Version f


{-| Garbage-collect history at a causal cut: fold every op at-or-below `cut`
into `base` and drop those ops from the store, advancing `baseFrontier`. The
**read model is unchanged** (`compact` is equivalence-preserving), so this is
purely a representation shrink — but it is irreversible: you can no longer
`readAt` a version below `cut` (the ops to replay are gone).

**Soundness is the caller's responsibility.** `compact` never loses information
`materialize` would use, but because `merge` is op-union, dropping ops is only
safe across replicas if every replica you will merge with has already
incorporated everything below `cut`. Passing your own `version` is always safe
for a _local_ store (single replica / before persistence); passing a frontier a
future merge partner hasn't reached can drop their not-yet-merged concurrent work
below `cut`. See `docs/04-gc.md`.

-}
gc : Version -> OpDoc a -> OpDoc a
gc (Version cut) (OpDoc d) =
    let
        ( base1, store1 ) =
            OpLog.compact d.base cut d.store
    in
    OpDoc
        { d
            | base = base1
            , store = store1
            , baseFrontier = cut

            -- `cached`/`ctx` are unchanged: read model is identical and every
            -- stamp folded into `base1` still contributes to `Node.maxCounter`.
            , lastAppend = Nothing
        }


{-| The materialized `Node` as of a `Version` — only ops causally at or before
that frontier are folded. Newer ops (and concurrent ops from peers) are excluded.
-}
stateAt : Version -> OpDoc a -> Node
stateAt (Version frontier) (OpDoc d) =
    OpLog.checkout frontier d.base d.store


{-| Read the typed value as of a `Version` — time-travel through the schema.
The live document is unchanged; this is a read-only view of the past.
-}
readAt : Version -> OpDoc a -> Result Schema.Error a
readAt v ((OpDoc d) as doc) =
    d.decode (stateAt v doc)


{-| How many ops the live history holds — the number of distinct edit steps you
can scrub through. `versionAt 0` is the empty document; `versionAt (historyLength
doc)` is the current state. (Ops folded away by `gc` are no longer scrubbable.)
-}
historyLength : OpDoc a -> Int
historyLength (OpDoc d) =
    List.length (OpLog.causalOrder d.store)


{-| The `Version` after the first `step` ops in causal order — a scrubber handle
into linear history. `readAt (versionAt n doc) doc` shows the document as it stood
after its `n`th edit. `step` is clamped to `[0, historyLength]`.

A prefix of the causal order is downward-closed (every op's deps precede it), so
the frontier of that prefix checks out exactly those ops.

-}
versionAt : Int -> OpDoc a -> Version
versionAt step (OpDoc d) =
    OpLog.causalOrder d.store
        |> List.take (max 0 step)
        |> frontierOfOps
        |> Version


{-| The causal tips of a set of ops: those no other op in the set depends on.
For a causal-order prefix this is the frontier that checks out exactly the prefix.
-}
frontierOfOps : List Op -> OpLog.Frontier
frontierOfOps ops =
    let
        depended =
            List.concatMap .deps ops
                |> List.map Id.opIdToString
                |> Set.fromList
    in
    ops
        |> List.map .id
        |> List.filter (\id -> not (Set.member (Id.opIdToString id) depended))


{-| Revert the live document to a past `Version`, **as new edits** — `restoreTo`
diffs the past state against the current one and emits the fresh, winning ops that
turn the latter back into the former. Because it goes through the op log like any
other edit, the revert **syncs to peers and converges**; it does not silently
rewind only the local replica (which a later merge would clobber).

Identity is preserved where it can be: the past state replays a subset of the same
ops that built the present, so unchanged registers/elements keep their ids (a
cursor or in-flight move anchored to a surviving item still resolves). Only items
that were _deleted_ since the version are re-created with fresh ids — the originals
are tombstoned forever. Restoring to the current version is a no-op.

-}
restoreTo : Version -> OpDoc a -> OpDoc a
restoreTo v doc =
    restoreNode [] (stateAt v doc) (state doc) doc


{-| Emit one op against the current frontier, advancing the clock and folding it
onto the cache (the same O(1) path as any single edit).
-}
emit : Action -> OpDoc a -> OpDoc a
emit action doc =
    let
        ( id, doc1 ) =
            mint doc
    in
    commit [ op id (frontierOf doc1) action ] doc1


{-| Emit an `InsertElem` whose op id _is_ the new element id (the convention the
materializer relies on), returning that id so callers can position it afterwards.

Anchors as a **right-child of `after`** (`side = Right`) — i.e. "immediately after
`after`", or a head root when `after` is `Nothing`. This is the anchor rule for
appends and for undo re-insertion (revival), where we always want to sit right after
a known predecessor. Fugue's `Left`-side anchoring is used only by `applyTextDiff`,
which chooses parent/side itself to keep concurrent runs from interleaving.

-}
emitInsert : List TargetStep -> Maybe OpId -> Node -> OpDoc a -> ( OpId, OpDoc a )
emitInsert target after seed doc =
    let
        ( elemId, doc1 ) =
            mint doc
    in
    ( elemId
    , commit [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, parent = after, side = Rga.Right, seed = seed }) ] doc1
    )


ctxOf : OpDoc a -> Ctx
ctxOf (OpDoc d) =
    d.ctx


withCtx : Ctx -> OpDoc a -> OpDoc a
withCtx ctx (OpDoc d) =
    OpDoc { d | ctx = ctx }


{-| Emit the ops that turn `current` (at `target`) back into `old`. Recurses
structurally; under one schema both nodes always share a shape at every path.
-}
restoreNode : List TargetStep -> Node -> Node -> OpDoc a -> OpDoc a
restoreNode target old current doc =
    case ( old, current ) of
        ( Node.Reg ro, Node.Reg rc ) ->
            if ro.value == rc.value then
                doc

            else
                emit (SetReg target ro.value) doc

        ( Node.Cnt _, Node.Cnt _ ) ->
            let
                diff =
                    Maybe.withDefault 0 (Node.asCounter old) - Maybe.withDefault 0 (Node.asCounter current)
            in
            if diff == 0 then
                doc

            else
                emit (Increment { target = target, delta = diff }) doc

        ( Node.Map mo, Node.Map mc ) ->
            restoreMap target mo mc doc

        ( Node.Seq _, Node.Seq _ ) ->
            restoreSeq target (visibleElems old) (visibleElems current) doc

        ( Node.Txt _, Node.Txt _ ) ->
            restoreSeq target (visibleElems old) (visibleElems current) doc

        ( Node.Mov mo, Node.Mov mc ) ->
            restoreMov target (MoveList.toEntries mo) (MoveList.toEntries mc) doc

        ( Node.Tree to, Node.Tree tc ) ->
            restoreTree target to tc doc

        _ ->
            -- shapes differ (shouldn't happen under a fixed schema): leave as-is
            doc


{-| Make the tree at `target` read like `old` again, diffing against `current`:

  - a node in BOTH (by id) that moved → move it back to its `old` parent/pos, and
    recurse into its payload;
  - a node only in `current` (added since) → delete it;
  - a node only in `old` (deleted since) → revive it + its subtree with fresh ids
    under its `old` parent.

Id-agnostic: works off the two node maps, so it's stable no matter how many other
undos/redos have re-minted ids around it.

-}
restoreTree : List TargetStep -> Tree.Tree Node -> Tree.Tree Node -> OpDoc a -> OpDoc a
restoreTree target old current doc =
    let
        oldIds =
            treeNodeIds old

        curIds =
            treeNodeIds current

        oldKeys =
            List.map Id.opIdToString oldIds |> Set.fromList

        curKeys =
            List.map Id.opIdToString curIds |> Set.fromList

        -- 1. delete nodes that exist now but not in `old` (added since). Deleting a
        -- parent hides its subtree, so only delete nodes whose parent is NOT itself
        -- being deleted (avoids redundant ops).
        toDelete =
            curIds
                |> List.filter (\id -> not (Set.member (Id.opIdToString id) oldKeys))
                |> List.filter
                    (\id ->
                        case Tree.parentOf id current of
                            Just p ->
                                Set.member (Id.opIdToString p) oldKeys

                            Nothing ->
                                True
                    )

        afterDeletes =
            List.foldl (\id d -> emit (DeleteElem { container = target, elem = id }) d) doc toDelete

        -- 2. revive nodes in `old` not present now (deleted since). Only revive
        -- subtree roots (whose parent still exists or is a root), to avoid double.
        toRevive =
            oldIds
                |> List.filter (\id -> not (Set.member (Id.opIdToString id) curKeys))
                |> List.filter
                    (\id ->
                        case Tree.parentOf id old of
                            Just p ->
                                Set.member (Id.opIdToString p) curKeys

                            Nothing ->
                                True
                    )

        afterRevives =
            List.foldl
                (\id d -> reviveNode target (Tree.parentOf id old) id old d)
                afterDeletes
                toRevive

        -- 3. for nodes in BOTH: move back if reparented/reordered, recurse payload
        shared =
            oldIds |> List.filter (\id -> Set.member (Id.opIdToString id) curKeys)

        afterMoves =
            List.foldl (restoreTreeNode target old) afterRevives shared
    in
    afterMoves


{-| For a node present in both `old` and the live doc: move it back to its `old`
parent/pos if it differs, then restore its payload.
-}
restoreTreeNode : List TargetStep -> Tree.Tree Node -> OpId -> OpDoc a -> OpDoc a
restoreTreeNode target old id doc =
    let
        liveTree =
            navigateTarget target (state doc) |> Maybe.andThen Node.asTree

        oldParent =
            Tree.parentOf id old

        moved =
            case liveTree of
                Just t ->
                    (Tree.parentOf id t /= oldParent)
                        || (Tree.siblingPos id t /= Tree.siblingPos id old)

                Nothing ->
                    False

        afterMove =
            if moved then
                case Tree.siblingPos id old of
                    Just pos ->
                        emit (TreeMove { container = target, child = id, parent = oldParent, pos = pos, seed = Nothing }) doc

                    Nothing ->
                        doc

            else
                doc

        -- restore the node's payload (recurse via the node ref path)
        payloadTarget =
            target ++ [ IntoElem id ]

        afterPayload =
            case ( Tree.get id old, navigateTarget payloadTarget (state afterMove) ) of
                ( Just oldContent, Just curContent ) ->
                    restoreNode payloadTarget oldContent curContent afterMove

                _ ->
                    afterMove
    in
    afterPayload


{-| All node ids in a tree, roots-first pre-order (parents before children, so a
revive/delete of a parent is emitted before its descendants).
-}
treeNodeIds : Tree.Tree c -> List OpId
treeNodeIds t =
    let
        go ids =
            ids |> List.concatMap (\id -> id :: go (Tree.childrenOf id t))
    in
    go (Tree.roots t)


{-| The visible (id, content) pairs of a `Seq`/`Txt` node, in order.
-}
visibleElems : Node -> List ( OpId, Node )
visibleElems node =
    let
        fromRga rga =
            Rga.toElementsInOrder rga
                |> List.filter (not << .deleted)
                |> List.map (\e -> ( e.id, e.content ))
    in
    case Node.asSeq node of
        Just rga ->
            fromRga rga

        Nothing ->
            case Node.asTxt node of
                Just rga ->
                    fromRga rga

                Nothing ->
                    []


restoreMap : List TargetStep -> Dict.Dict String Node.Entry -> Dict.Dict String Node.Entry -> OpDoc a -> OpDoc a
restoreMap target mo mc doc =
    (Dict.keys mo ++ Dict.keys mc)
        |> Set.fromList
        |> Set.foldl (\k d -> restoreMapKey (target ++ [ IntoKey k ]) (Dict.get k mo) (Dict.get k mc) d) doc


restoreMapKey : List TargetStep -> Maybe Node.Entry -> Maybe Node.Entry -> OpDoc a -> OpDoc a
restoreMapKey keyTarget mOld mCur doc =
    case ( mOld, mCur ) of
        ( Just oe, Just ce ) ->
            let
                d1 =
                    if oe.present == ce.present then
                        doc

                    else
                        emit (SetPresence { target = keyTarget, present = oe.present, seed = emptyMap }) doc
            in
            if oe.present then
                restoreNode keyTarget oe.value ce.value d1

            else
                d1

        ( Just oe, Nothing ) ->
            -- key existed at the version but not now: re-create it with a fresh,
            -- deep-restamped value subtree
            let
                ( seedNode, ctx1 ) =
                    Node.reStamp (ctxOf doc) oe.value
            in
            emit (SetPresence { target = keyTarget, present = oe.present, seed = seedNode }) (withCtx ctx1 doc)

        ( Nothing, Just ce ) ->
            -- key added after the version: tombstone it
            if ce.present then
                emit (SetPresence { target = keyTarget, present = False, seed = emptyMap }) doc

            else
                doc

        ( Nothing, Nothing ) ->
            doc


{-| Restore a `Seq`/`Txt`. Sequences cannot reorder, so survivors keep their
order: delete current-only elements, recurse into kept ones (identity preserved),
and re-insert version-only ones (deleted since) as fresh elements, chained into
position.
-}
restoreSeq : List TargetStep -> List ( OpId, Node ) -> List ( OpId, Node ) -> OpDoc a -> OpDoc a
restoreSeq target oldEls curEls doc =
    let
        oldKeys =
            List.map (Tuple.first >> Id.opIdToString) oldEls |> Set.fromList

        curById =
            List.map (\( id, c ) -> ( Id.opIdToString id, c )) curEls |> Dict.fromList

        afterDeletes =
            List.foldl
                (\( id, _ ) d ->
                    if Set.member (Id.opIdToString id) oldKeys then
                        d

                    else
                        emit (DeleteElem { container = target, elem = id }) d
                )
                doc
                curEls
    in
    List.foldl
        (\( oid, oContent ) ( d, after ) ->
            case Dict.get (Id.opIdToString oid) curById of
                Just cContent ->
                    ( restoreNode (target ++ [ IntoElem oid ]) oContent cContent d, Just oid )

                Nothing ->
                    let
                        ( seedNode, ctx1 ) =
                            Node.reStamp (ctxOf d) oContent

                        ( newId, d1 ) =
                            emitInsert target after seedNode (withCtx ctx1 d)
                    in
                    ( d1, Just newId )
        )
        ( afterDeletes, Nothing )
        oldEls
        |> Tuple.first


{-| Restore a `Mov` (movable list). Reconcile membership like `restoreSeq`
(delete extras, recurse into survivors, re-insert deleted), then a positioning
pass moves each item after the previous one's home cell so the final visible order
matches the version's.
-}
restoreMov : List TargetStep -> List ( OpId, Node ) -> List ( OpId, Node ) -> OpDoc a -> OpDoc a
restoreMov target oldEntries curEntries doc =
    let
        oldKeys =
            List.map (Tuple.first >> Id.opIdToString) oldEntries |> Set.fromList

        curById =
            List.map (\( id, c ) -> ( Id.opIdToString id, c )) curEntries |> Dict.fromList

        afterDeletes =
            List.foldl
                (\( id, _ ) d ->
                    if Set.member (Id.opIdToString id) oldKeys then
                        d

                    else
                        emit (DeleteElem { container = target, elem = id }) d
                )
                doc
                curEntries

        -- recurse into survivors / re-insert deleted; collect the live valueId of
        -- each version entry, in the version's order
        ( afterContent, liveOrder ) =
            List.foldl
                (\( oid, oContent ) ( d, acc ) ->
                    case Dict.get (Id.opIdToString oid) curById of
                        Just cContent ->
                            ( restoreNode (target ++ [ IntoElem oid ]) oContent cContent d, acc ++ [ oid ] )

                        Nothing ->
                            let
                                ( seedNode, ctx1 ) =
                                    Node.reStamp (ctxOf d) oContent

                                ( newId, d1 ) =
                                    emitInsert target Nothing seedNode (withCtx ctx1 d)
                            in
                            ( d1, acc ++ [ newId ] )
                )
                ( afterDeletes, [] )
                oldEntries
    in
    -- positioning pass: chain each item after the previous one's current home cell
    List.foldl
        (\valueId ( d, prev ) ->
            let
                anchor =
                    case prev of
                        Nothing ->
                            Nothing

                        Just prevId ->
                            navigateTarget target (state d)
                                |> Maybe.andThen Node.asMov
                                |> Maybe.andThen (MoveList.homeCell prevId)
            in
            ( emit (MoveElem { container = target, elem = valueId, after = anchor }) d, Just valueId )
        )
        ( afterContent, Nothing )
        liveOrder
        |> Tuple.first



-- LOCAL UNDO / REDO ----------------------------------------------------------


{-| Record a local change for undo, given the version **before** it and the doc
**after** it. The library does not know which of your `OpDoc` calls form one
user-level "edit", so you bracket them: capture `version doc` before, make your
edits, then call `recordEdit before edited`. The inverse of exactly the ops added
in between is pushed onto the undo stack and the redo stack is cleared (a new edit
forks history), matching every editor's undo model.

This is **local, Loro-style** undo: `undo` later inverts _your_ ops as fresh ops,
so a peer's concurrent edit to another field survives and the revert still syncs.
A no-op change (no ops added) records nothing.

-}
recordEdit : Version -> OpDoc a -> OpDoc a
recordEdit before ((OpDoc d) as doc) =
    let
        after =
            version doc
    in
    if before == after then
        -- no ops added: nothing to undo (a no-op edit)
        doc

    else
        OpDoc { d | undoStack = { before = before, after = after } :: d.undoStack, redoStack = [] }


{-| Whether there is a local edit to undo.
-}
canUndo : OpDoc a -> Bool
canUndo (OpDoc d) =
    not (List.isEmpty d.undoStack)


{-| Whether there is an undone local edit to redo.
-}
canRedo : OpDoc a -> Bool
canRedo (OpDoc d) =
    not (List.isEmpty d.redoStack)


{-| Undo the most recent recorded local edit: invert exactly the ops the edit added
(between its `before` and `after` versions) and emit them as fresh ops, so the undo
syncs to peers and only touches what the edit changed. Records the inverse range on
the redo stack. No-op when there is nothing to undo.

Robust across sequences: the inverse is recomputed from the frozen version range
each time, so earlier undos/redos re-minting ids never invalidate this entry.

-}
undo : OpDoc a -> OpDoc a
undo ((OpDoc d) as doc) =
    case d.undoStack of
        [] ->
            doc

        entry :: rest ->
            let
                -- the doc version *immediately before* this undo emits its ops; the
                -- redo must invert exactly the ops this undo adds, i.e. the range
                -- (preUndo, postUndo] — NOT (entry.after, postUndo], which would also
                -- sweep in ops emitted by earlier undos between entry.after and now.
                preUndo =
                    version doc

                applied =
                    applyRevs (inverseBetween entry.before entry.after doc) doc

                redoEntry =
                    { before = preUndo, after = version applied }
            in
            case applied of
                OpDoc ad ->
                    OpDoc { ad | undoStack = rest, redoStack = redoEntry :: ad.redoStack }


{-| Redo the most recently undone edit: symmetric to `undo` — invert the undo's own
ops (the range it recorded), restoring the edit.
-}
redo : OpDoc a -> OpDoc a
redo ((OpDoc d) as doc) =
    case d.redoStack of
        [] ->
            doc

        entry :: rest ->
            let
                -- symmetric to `undo`: the new undo entry must cover only the ops
                -- THIS redo emits — the range (preRedo, postRedo].
                preRedo =
                    version doc

                applied =
                    applyRevs (inverseBetween entry.before entry.after doc) doc

                undoEntry =
                    { before = preRedo, after = version applied }
            in
            case applied of
                OpDoc ad ->
                    OpDoc { ad | redoStack = rest, undoStack = undoEntry :: ad.undoStack }


{-| The reverse actions that undo the ops added between `before` and `after` — the
ops causally in `after` but not `before`, inverted, newest-first (so they unwind in
reverse emission order). Each inverse is computed against the state as of _just
before_ that op applied, so a register set inverts to its prior value, a delete to
a re-create, etc.
-}
inverseBetween : Version -> Version -> OpDoc a -> List RevAction
inverseBetween (Version beforeFrontier) (Version afterFrontier) (OpDoc d) =
    let
        beforeKeys =
            OpLog.ancestorKeys beforeFrontier d.store

        afterKeys =
            OpLog.ancestorKeys afterFrontier d.store

        -- ops in the (before, after] range: ancestors of `after`, not of `before`.
        added =
            OpLog.causalOrder d.store
                |> List.filter
                    (\o ->
                        Set.member (Id.opIdToString o.id) afterKeys
                            && not (Set.member (Id.opIdToString o.id) beforeKeys)
                    )
    in
    -- fold forward from the before-checkout, materializing each op's pre-state,
    -- collecting inverses; reverse so undo unwinds last-emitted first.
    added
        |> List.foldl
            (\o ( preState, acc ) ->
                ( OpLog.applyOps preState [ o ]
                , inverseOf o preState ++ acc
                )
            )
            ( OpLog.checkout beforeFrontier d.base d.store, [] )
        |> Tuple.second


{-| The reverse action(s) for one op, given the state **just before** it applied.
-}
inverseOf : Op -> Node -> List RevAction
inverseOf theOp preState =
    case theOp.action of
        SetReg target _ ->
            case navigateTarget target preState |> Maybe.andThen Node.asPrim of
                Just prior ->
                    [ Rev (SetReg target prior) ]

                Nothing ->
                    []

        Increment { target, delta } ->
            [ Rev (Increment { target = target, delta = -delta }) ]

        SetPresence { target, present } ->
            [ Rev (SetPresence { target = target, present = not present, seed = emptyMap }) ]

        InsertElem { container, elemId } ->
            [ Rev (DeleteElem { container = container, elem = elemId }) ]

        MoveElem { container, elem } ->
            case navigateTarget container preState of
                Just node ->
                    [ Rev (MoveElem { container = container, elem = elem, after = priorAnchor elem node }) ]

                Nothing ->
                    []

        DeleteElem { container, elem } ->
            -- undo a delete. Tombstones are permanent, so this revives a FRESH copy
            -- (new id), content/structure preserved, identity not.
            case navigateTarget container preState of
                Just (Node.Tree t) ->
                    -- a tree node: revive it + its subtree as ONE atomic graft, so
                    -- it inverts back to a single delete (keeps undo/redo symmetric).
                    if Tree.get elem t == Nothing then
                        []

                    else
                        [ ReGraft { container = container, parent = Tree.parentOf elem t, rootId = elem, source = t } ]

                Just node ->
                    case elementContent elem node of
                        Just content ->
                            [ ReInsert { container = container, elemId = elem, afterValue = priorAnchor elem node, content = content } ]

                        Nothing ->
                            []

                Nothing ->
                    []

        TreeMove { container, child, seed } ->
            case seed of
                Just _ ->
                    -- op CREATED this node: undo = delete it (subtree hides with it)
                    [ Rev (DeleteElem { container = container, elem = child }) ]

                Nothing ->
                    -- op RE-PARENTED: move it back to its prior parent/pos
                    case navigateTarget container preState |> Maybe.andThen Node.asTree of
                        Just t ->
                            case Tree.siblingPos child t of
                                Just pos ->
                                    [ Rev (TreeMove { container = container, child = child, parent = Tree.parentOf child t, pos = pos, seed = Nothing }) ]

                                Nothing ->
                                    []

                        Nothing ->
                            []


{-| Apply reverse actions as fresh ops (each syncs). Ids named by the inverse ops
are resolved through (and revivals recorded into) the doc's `idRemap` table, so a
delete undone as a fresh copy stays targetable by any later inverse — see `idRemap`.
-}
applyRevs : List RevAction -> OpDoc a -> OpDoc a
applyRevs revs doc =
    List.foldl applyRev doc revs


applyRev : RevAction -> OpDoc a -> OpDoc a
applyRev rev doc =
    case rev of
        Rev action ->
            emit (remapAction (remapOf doc) action) doc

        ReInsert { container, elemId, afterValue, content } ->
            let
                remap =
                    remapOf doc

                ( seedNode, ctx1, innerRemap ) =
                    Node.reStampWithMap (ctxOf doc) content

                after =
                    liveAnchor (remapTarget remap container) (Maybe.map (remapId remap) afterValue) (state doc)

                ( newId, doc1 ) =
                    emitInsert (remapTarget remap container) after seedNode (withCtx ctx1 doc)
            in
            -- record original elem → its fresh revived id, PLUS every id inside the
            -- re-stamped content (`innerRemap`), so later inverses that name the
            -- original element OR anything within it retarget onto this live copy.
            doc1
                |> registerRemapAll innerRemap
                |> registerRemap elemId newId

        ReGraft { container, parent, rootId, source } ->
            let
                remap =
                    remapOf doc
            in
            reviveNode (remapTarget remap container) (Maybe.map (remapId remap) parent) rootId source doc


{-| The doc's current id-remap table.
-}
remapOf : OpDoc a -> Dict.Dict String OpId
remapOf (OpDoc d) =
    d.idRemap


{-| Record `original → replacement` in the remap table.
-}
registerRemap : OpId -> OpId -> OpDoc a -> OpDoc a
registerRemap original replacement (OpDoc d) =
    OpDoc { d | idRemap = Dict.insert (Id.opIdToString original) replacement d.idRemap }


{-| Merge a whole `originalId → revivedId` map (from `Node.reStampWithMap`) into the
remap table. Existing entries win (they were recorded by more recent revivals).
-}
registerRemapAll : Dict.Dict String OpId -> OpDoc a -> OpDoc a
registerRemapAll mapping (OpDoc d) =
    OpDoc { d | idRemap = Dict.union d.idRemap mapping }


{-| Resolve an id through the remap table, transitively (a revived copy that was
itself later revived chains through). Ids are globally fresh, so the chain is acyclic
and terminates.
-}
remapId : Dict.Dict String OpId -> OpId -> OpId
remapId table id =
    case Dict.get (Id.opIdToString id) table of
        Just next ->
            remapId table next

        Nothing ->
            id


remapTarget : Dict.Dict String OpId -> Target -> Target
remapTarget table =
    List.map
        (\step ->
            case step of
                IntoElem id ->
                    IntoElem (remapId table id)

                IntoKey _ ->
                    step
        )


{-| Rewrite every element id an inverse action names through the remap table.
-}
remapAction : Dict.Dict String OpId -> Action -> Action
remapAction table action =
    case action of
        SetReg target prim ->
            SetReg (remapTarget table target) prim

        SetPresence r ->
            SetPresence { r | target = remapTarget table r.target }

        InsertElem r ->
            InsertElem { r | container = remapTarget table r.container, elemId = remapId table r.elemId, parent = Maybe.map (remapId table) r.parent }

        DeleteElem r ->
            DeleteElem { container = remapTarget table r.container, elem = remapId table r.elem }

        MoveElem r ->
            MoveElem { container = remapTarget table r.container, elem = remapId table r.elem, after = Maybe.map (remapId table) r.after }

        Increment r ->
            Increment { r | target = remapTarget table r.target }

        TreeMove r ->
            TreeMove { r | container = remapTarget table r.container, child = remapId table r.child, parent = Maybe.map (remapId table) r.parent }


{-| The element to anchor _after_ to put `elem` back where it was: the id of its
predecessor in the current order, or `Nothing` if it was at the head.
-}
priorAnchor : OpId -> Node -> Maybe OpId
priorAnchor elem node =
    let
        elemKey =
            Id.opIdToString elem
    in
    orderedIds node
        |> Maybe.withDefault []
        |> List.foldl
            (\id ( prev, found ) ->
                if found then
                    ( prev, found )

                else if Id.opIdToString id == elemKey then
                    ( prev, True )

                else
                    ( Just id, False )
            )
            ( Nothing, False )
        |> Tuple.first


{-| Resolve a re-insert anchor against the live container: use the recorded
predecessor if still present, else fall back to the head.
-}
liveAnchor : List TargetStep -> Maybe OpId -> Node -> Maybe OpId
liveAnchor container afterValue root =
    case afterValue of
        Nothing ->
            Nothing

        Just a ->
            case navigateTarget container root |> Maybe.andThen orderedIds of
                Just ids ->
                    if List.any (\id -> Id.opIdToString id == Id.opIdToString a) ids then
                        Just a

                    else
                        Nothing

                Nothing ->
                    Nothing


{-| The content node of element `elem` in a `Seq`/`Txt`/`Mov` container.
-}
elementContent : OpId -> Node -> Maybe Node
elementContent elem node =
    case Node.asMov node of
        Just ml ->
            MoveList.get elem ml

        Nothing ->
            seqRga node
                |> Maybe.andThen (Rga.get elem)
                |> Maybe.map .content


{-| Revive tree node `sourceId` (from `source`) and its subtree under `newParent`
in the live doc, as fresh create ops. Each node gets a new id (the original is
tombstoned); children are created under their re-minted parent, preserving order.
-}
reviveNode : List TargetStep -> Maybe OpId -> OpId -> Tree.Tree Node -> OpDoc a -> OpDoc a
reviveNode container newParent sourceId source doc =
    case Tree.get sourceId source of
        Nothing ->
            doc

        Just content ->
            let
                ( childId, ctx1 ) =
                    Id.nextId (ctxOf doc)

                -- fresh ids for the payload subtree too, keeping the inner id map so
                -- a later inverse referencing something inside the payload (e.g. a
                -- text char a follow-up insert anchored after) still resolves.
                ( seedNode, ctx2, innerRemap ) =
                    Node.reStampWithMap ctx1 content

                -- restore the node at its ORIGINAL sibling position (from `source`),
                -- not the end, so undo puts it back where it was among its siblings.
                pos =
                    Tree.siblingPos sourceId source
                        |> Maybe.withDefault (treeEndPos container newParent (state doc))

                doc1 =
                    withCtx ctx2 doc
                        |> emit (TreeMove { container = container, child = childId, parent = newParent, pos = pos, seed = Just seedNode })
                        |> registerRemapAll innerRemap
                        -- record original node id → its fresh revived id, so a later
                        -- inverse that still names the original node retargets here.
                        |> registerRemap sourceId childId
            in
            -- recurse into the source node's children, under the new id
            List.foldl
                (\srcChild d -> reviveNode container (Just childId) srcChild source d)
                doc1
                (Tree.childrenOf sourceId source)


{-| The end position for a new child under `parent` in the live tree at `container`
(so a revived node lands after existing siblings).
-}
treeEndPos : List TargetStep -> Maybe OpId -> Node -> Frac
treeEndPos container parent root =
    case navigateTarget container root |> Maybe.andThen Node.asTree of
        Just t ->
            let
                siblings =
                    case parent of
                        Just p ->
                            Tree.childrenOf p t

                        Nothing ->
                            Tree.roots t
            in
            case List.reverse siblings |> List.head of
                Just last ->
                    Crdt.Frac.between (Tree.siblingPos last t) Nothing

                Nothing ->
                    Crdt.Frac.between Nothing Nothing

        Nothing ->
            Crdt.Frac.between Nothing Nothing



-- STABLE CURSORS -------------------------------------------------------------


{-| Make a stable `Cursor` for a visible `offset` within the text/list addressed
by `path`. The cursor anchors to element identity, so it stays meaningful as
other replicas edit around it — resolve it back with `cursorOffset`.

`offset` 0 anchors before the first element; otherwise it anchors _after_ the
element currently at visible index `offset - 1`. Fails if `path` doesn't resolve
to a sequence/text container.

-}
cursorAt : Path -> Int -> OpDoc a -> Result Error Cursor
cursorAt path offset doc =
    resolve path doc
        |> Result.andThen
            (\( tgt, node ) ->
                case orderedIds node of
                    Just ids ->
                        let
                            anchor =
                                if offset <= 0 then
                                    Cursor.Start

                                else
                                    -- anchor to the element just before `offset`;
                                    -- clamp past-the-end to the last element.
                                    case List.drop (offset - 1) ids |> List.head of
                                        Just id ->
                                            Cursor.After id

                                        Nothing ->
                                            case List.reverse ids |> List.head of
                                                Just id ->
                                                    Cursor.After id

                                                Nothing ->
                                                    Cursor.Start
                        in
                        Ok (Cursor.fromParts tgt anchor)

                    Nothing ->
                        Err (WrongNodeType "expected a text or list at the cursor path")
            )


{-| Resolve a `Cursor` to its current visible offset in this document. `Nothing`
if the cursor's container no longer exists here. For `Seq`/`Txt` this is robust
across deletion of the anchored element (tombstones retained — see
`Crdt.Rga.liveCountThrough`); for `Mov` it counts live values at-or-before the
anchor in the current order.
-}
cursorOffset : Cursor -> OpDoc a -> Maybe Int
cursorOffset cursor doc =
    let
        node =
            navigateTarget (Cursor.steps cursor) (state doc)
    in
    case Cursor.anchor cursor of
        Cursor.Start ->
            node |> Maybe.andThen orderedIds |> Maybe.map (always 0)

        Cursor.After id ->
            case node |> Maybe.andThen seqRga of
                Just rga ->
                    -- RGA path: robust across deletion of the anchor (tombstones)
                    Just (Rga.liveCountThrough id rga)

                Nothing ->
                    -- Mov (or other ordered): count visible ids up to & incl. the anchor
                    node
                        |> Maybe.andThen orderedIds
                        |> Maybe.map (countThrough id)


{-| 1 + the index of `anchor` in `ids` (i.e. the offset just after it); if the
anchor isn't present, the count of all ids (caret at the end).
-}
countThrough : OpId -> List OpId -> Int
countThrough anchor ids =
    let
        anchorKey =
            Id.opIdToString anchor

        go n remaining =
            case remaining of
                [] ->
                    n

                x :: rest ->
                    if Id.opIdToString x == anchorKey then
                        n + 1

                    else
                        go (n + 1) rest
    in
    go 0 ids


{-| Resolve a selection `Range` to a `(start, end)` pair of visible offsets in
this document, normalized so `start <= end`. `Nothing` if either endpoint's
container is gone.
-}
cursorRange : Cursor.Range -> OpDoc a -> Maybe ( Int, Int )
cursorRange r doc =
    Maybe.map2
        (\a f -> ( min a f, max a f ))
        (cursorOffset (Cursor.rangeAnchor r) doc)
        (cursorOffset (Cursor.rangeFocus r) doc)


{-| The RGA inside a `Seq` or `Txt` node.
-}
seqRga : Node -> Maybe Node.RgaNode
seqRga node =
    case Node.asSeq node of
        Just rga ->
            Just rga

        Nothing ->
            Node.asTxt node


{-| The visible element/value ids of an ordered node — `Seq`/`Txt` (RGA) **or**
`Mov` (movable list) — in order. The uniform "ordered, id-addressed sequence"
view that list edits and cursors resolve against, so they work for both kinds.
-}
orderedIds : Node -> Maybe (List OpId)
orderedIds node =
    case Node.asMov node of
        Just ml ->
            Just (MoveList.toEntries ml |> List.map Tuple.first)

        Nothing ->
            seqRga node |> Maybe.map Rga.visibleIds


{-| The id of the element/value at visible index `i` of an ordered node.
-}
elemIdAt : Int -> Node -> Maybe OpId
elemIdAt i node =
    orderedIds node |> Maybe.andThen (List.drop i >> List.head)


{-| The id of the last visible element/value (the append anchor).
-}
lastElemId : Node -> Maybe OpId
lastElemId node =
    orderedIds node |> Maybe.andThen (List.reverse >> List.head)


{-| Navigate an **id-based** `Target` into a node, returning the addressed node.
Mirrors `walk` but keyed by element id rather than visible index, so it is the
read-only inverse used to resolve a stable cursor. Handles `Mov` (value-by-id) as
well as `Seq`/`Txt` (element-by-id).
-}
navigateTarget : Target -> Node -> Maybe Node
navigateTarget tgt node =
    case tgt of
        [] ->
            Just node

        (IntoKey k) :: rest ->
            Node.asMap node
                |> Maybe.andThen (Dict.get k)
                |> Maybe.andThen (\entry -> navigateTarget rest entry.value)

        (IntoElem id) :: rest ->
            case Node.asMov node of
                Just ml ->
                    MoveList.get id ml
                        |> Maybe.andThen (\content -> navigateTarget rest content)

                Nothing ->
                    case Node.asTree node of
                        Just t ->
                            -- descend into a tree node by its stable id
                            Tree.get id t
                                |> Maybe.andThen (\content -> navigateTarget rest content)

                        Nothing ->
                            seqRga node
                                |> Maybe.andThen (Rga.get id)
                                |> Maybe.andThen (\el -> navigateTarget rest el.content)


{-| Save a named checkpoint pinning the current version. Records the label and
the saving replica; does not change the document (no op is emitted).
-}
checkpoint : String -> OpDoc a -> OpDoc a
checkpoint message ((OpDoc d) as doc) =
    let
        cp =
            Checkpoint
                { message = message
                , author = Id.ctxReplica d.ctx
                , version = version doc
                }
    in
    OpDoc { d | checkpoints = cp :: d.checkpoints }


{-| All saved checkpoints, most recent first.
-}
checkpoints : OpDoc a -> List Checkpoint
checkpoints (OpDoc d) =
    d.checkpoints


{-| The label of a checkpoint.
-}
checkpointMessage : Checkpoint -> String
checkpointMessage (Checkpoint cp) =
    cp.message


{-| The replica that saved a checkpoint.
-}
checkpointAuthor : Checkpoint -> ReplicaId
checkpointAuthor (Checkpoint cp) =
    cp.author


{-| The version a checkpoint pins (pass to `readAt` to time-travel to it).
-}
checkpointVersion : Checkpoint -> Version
checkpointVersion (Checkpoint cp) =
    cp.version



-- EMITTING OPS ---------------------------------------------------------------


{-| Append ops to the log and advance the clock. Each op causally follows the
frontier as it stood before this batch, so the ops apply straight onto the cached
state in emission order — no re-materialization (the O(1) hot path).
-}
commit : List Op -> OpDoc a -> OpDoc a
commit newOps (OpDoc d) =
    OpDoc
        { d
            | store = List.foldl OpLog.insert d.store newOps
            , cached = OpLog.applyOps d.cached newOps

            -- any committed edit invalidates the append fast-path by default;
            -- `emitAppend` re-establishes it for a genuine append.
            , lastAppend = Nothing
        }


{-| Mint a fresh op id, advancing the clock.
-}
mint : OpDoc a -> ( OpId, OpDoc a )
mint (OpDoc d) =
    let
        ( id, ctx1 ) =
            Id.nextId d.ctx
    in
    ( id, OpDoc { d | ctx = ctx1 } )



-- PRIMITIVE SETTERS ----------------------------------------------------------


{-| Set a register leaf (LWW) to a primitive.
-}
setPrim : Path -> Prim -> OpDoc a -> Result Error (OpDoc a)
setPrim path prim doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (SetReg target prim) ] doc1
            )


{-| Set a boolean register.
-}
setBool : Path -> Bool -> OpDoc a -> Result Error (OpDoc a)
setBool path b =
    setPrim path (PBool b)


{-| Set an integer register.
-}
setInt : Path -> Int -> OpDoc a -> Result Error (OpDoc a)
setInt path n =
    setPrim path (PInt n)


{-| Set a string register (overwrite; for collaborative text use `setText`).
-}
setString : Path -> String -> OpDoc a -> Result Error (OpDoc a)
setString path s =
    setPrim path (PString s)


{-| Add `delta` to a counter field (use a negative `delta` to decrement).
Concurrent increments from different replicas sum, rather than one clobbering the
other.
-}
increment : Path -> Int -> OpDoc a -> Result Error (OpDoc a)
increment path delta doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (Increment { target = target, delta = delta }) ] doc1
            )



-- TEXT -----------------------------------------------------------------------


{-| Edit a text field so it reads as `value`, emitting the minimal run of
character insert/delete ops (a common-prefix/suffix diff) so concurrent edits in
other regions survive.
-}
setText : Path -> String -> OpDoc a -> Result Error (OpDoc a)
setText path value doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case node of
                    Node.Txt rga ->
                        Ok (applyTextDiff target rga value doc)

                    _ ->
                        Err (WrongNodeType "expected text node for setText")
            )


applyTextDiff : List TargetStep -> Rga.Rga Node -> String -> OpDoc a -> OpDoc a
applyTextDiff target rga value doc =
    let
        -- compute the visible order ONCE; index into it rather than re-ordering
        -- the array for every position (which made a replace O(D*N) per edit)
        ids =
            Rga.visibleIds rga |> Array.fromList

        current =
            Rga.toList rga
                |> List.filterMap
                    (\n ->
                        case Node.asPrim n of
                            Just (PString s) ->
                                Just s

                            _ ->
                                Nothing
                    )
                |> String.concat
                |> String.toList

        target_ =
            String.toList value

        prefix =
            commonPrefix current target_ 0

        maxSuffix =
            min (List.length current - prefix) (List.length target_ - prefix)

        suffix =
            commonSuffix (List.reverse current) (List.reverse target_) 0 maxSuffix

        -- visible indices [prefix .. len-suffix-1] of the CURRENT text are deleted
        deleteIds =
            List.range prefix (List.length current - suffix - 1)
                |> List.filterMap (\i -> Array.get i ids)

        -- characters to insert at the prefix boundary
        insertChars =
            target_
                |> List.drop prefix
                |> List.take (List.length target_ - prefix - suffix)

        -- Fugue anchoring for the FIRST inserted char. `leftAnchor` = the surviving
        -- visible element just before the gap (index prefix-1, which survives since
        -- deletions start at prefix); `rightAnchor` = the first surviving visible
        -- element after the gap (index len-suffix, first past the deleted range).
        leftAnchor =
            if prefix <= 0 then
                Nothing

            else
                Array.get (prefix - 1) ids

        rightAnchor =
            Array.get (List.length current - suffix) ids

        startPlacement =
            fuguePlacement rga leftAnchor rightAnchor

        deps =
            frontierOf doc

        -- delete ops
        ( afterDeletes, deleteOps ) =
            List.foldl
                (\elemId ( d, acc ) ->
                    let
                        ( id, d1 ) =
                            mint d
                    in
                    ( d1, op id deps (DeleteElem { container = target, elem = elemId }) :: acc )
                )
                ( doc, [] )
                deleteIds

        -- insert ops. The first char uses the Fugue placement; each subsequent char
        -- chains as a RIGHT-CHILD of the previous one, so the whole run is a single
        -- right-spine subtree — that is what keeps a concurrent run at the same gap
        -- from interleaving with this one (it renders as one contiguous block).
        ( finalDoc, insertOpsRev, _ ) =
            List.foldl
                (\char ( d, acc, placement ) ->
                    let
                        ( elemId, d1 ) =
                            mint d

                        ( parent, side ) =
                            placement

                        seedNode =
                            Node.reg (PString (String.fromChar char)) elemId
                    in
                    ( d1
                    , op elemId deps (InsertElem { container = target, elemId = elemId, parent = parent, side = side, seed = seedNode }) :: acc
                    , ( Just elemId, Rga.Right )
                    )
                )
                ( afterDeletes, [], startPlacement )
                insertChars
    in
    commit (deleteOps ++ List.reverse insertOpsRev) finalDoc


{-| Choose a Fugue `(parent, side)` for a new element inserted between visible
neighbors `left` and `right` (either may be absent at the ends of the text).

The rule keeps the element strictly between L and R while making concurrent
insertions at the same gap attach to the **same** anchor (so they order as whole
blocks, not interleaved):

  - if `right` exists and descends from `left` (R sits in L's right subtree — the
    common case, since L and R are adjacent) → attach as a **left-child of R**;
  - else if `left` exists → attach as a **right-child of L** (e.g. appending at the
    end, where R is absent);
  - else if `right` exists → attach as a **left-child of R** (inserting at the head);
  - else (empty sequence) → a root (`parent = Nothing, side = Right`).

-}
fuguePlacement : Rga.Rga Node -> Maybe OpId -> Maybe OpId -> ( Maybe OpId, Rga.Side )
fuguePlacement rga left right =
    case ( left, right ) of
        ( Just l, Just r ) ->
            if descendsFrom rga r l then
                ( Just r, Rga.Left )

            else
                ( Just l, Rga.Right )

        ( Just l, Nothing ) ->
            ( Just l, Rga.Right )

        ( Nothing, Just r ) ->
            ( Just r, Rga.Left )

        ( Nothing, Nothing ) ->
            ( Nothing, Rga.Right )


{-| Is element `node` a descendant of `ancestor` in the Fugue parent tree? Walks
parent pointers up from `node`; cycle-safe via a visited set (adversarial input).
-}
descendsFrom : Rga.Rga Node -> OpId -> OpId -> Bool
descendsFrom rga node ancestor =
    let
        ancestorKey =
            Id.opIdToString ancestor

        climb : Maybe OpId -> Set.Set String -> Bool
        climb current visited =
            case current of
                Nothing ->
                    False

                Just id ->
                    let
                        key =
                            Id.opIdToString id
                    in
                    if key == ancestorKey then
                        True

                    else if Set.member key visited then
                        False

                    else
                        climb (Rga.get id rga |> Maybe.andThen .parent) (Set.insert key visited)
    in
    -- start from `node`'s parent (a node is not its own descendant)
    climb (Rga.get node rga |> Maybe.andThen .parent) Set.empty



-- LIST -----------------------------------------------------------------------


{-| Append a fresh subtree (built by a `Seed`) to the end of a list.

Fast path: if the previous edit was an append to this same list, we already know
the last element's id (`lastAppend`) and skip the O(n) `Rga.lastVisibleId`
re-ordering — so a run of appends to one list is O(1) each instead of O(n²)
overall. Otherwise we compute it once and start the run.

-}
listAppend : Path -> Seed -> OpDoc a -> Result Error (OpDoc a)
listAppend path seed doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                if isOrdered node then
                    let
                        after =
                            case appendCacheFor target doc of
                                Just cachedLast ->
                                    Just cachedLast

                                Nothing ->
                                    appendAnchor node
                    in
                    Ok (emitAppend target after seed doc)

                else
                    Err (WrongNodeType "expected list node for listAppend")
            )


{-| Whether a node is an ordered, id-addressed sequence (`Seq`/`Txt`/`Mov`).
-}
isOrdered : Node -> Bool
isOrdered node =
    orderedIds node /= Nothing


{-| The anchor to append after: the last visible element's id for `Seq`/`Txt`, or
the last value's **home cell** for `Mov` (inserts/moves anchor after a cell).
-}
appendAnchor : Node -> Maybe OpId
appendAnchor node =
    case Node.asMov node of
        Just ml ->
            lastElemId node |> Maybe.andThen (\vid -> MoveList.homeCell vid ml)

        Nothing ->
            lastElemId node


{-| The cell/element to anchor _after_ when inserting/moving to visible index `i`
(i.e. just after the item currently at `i-1`). `Nothing` = head.
-}
anchorBefore : Int -> Node -> Maybe OpId
anchorBefore i node =
    if i <= 0 then
        Nothing

    else
        case Node.asMov node of
            Just ml ->
                elemIdAt (i - 1) node |> Maybe.andThen (\vid -> MoveList.homeCell vid ml)

            Nothing ->
                elemIdAt (i - 1) node


{-| The cached last-appended id for `target`, if the append fast-path is live for
exactly this list.
-}
appendCacheFor : List TargetStep -> OpDoc a -> Maybe OpId
appendCacheFor target (OpDoc d) =
    case d.lastAppend of
        Just ( cachedTarget, lastId ) ->
            if cachedTarget == target then
                Just lastId

            else
                Nothing

        Nothing ->
            Nothing


{-| Tombstone the element at a visible index in a list.
-}
listRemove : Path -> Int -> OpDoc a -> Result Error (OpDoc a)
listRemove path i doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                if isOrdered node then
                    case elemIdAt i node of
                        Just elemId ->
                            let
                                ( id, doc1 ) =
                                    mint doc
                            in
                            Ok (commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = elemId }) ] doc1)

                        Nothing ->
                            Err (PathNotFound ("list index " ++ String.fromInt i))

                else
                    Err (WrongNodeType "expected list node for listRemove")
            )


{-| Move the item at visible index `from` to sit at visible index `to`, on a
`movableList`. The item keeps its identity (nested edits and cursors follow it).
On a plain `list` (`Seq`) this fails — only `movableList` supports moves.
-}
listMove : Path -> Int -> Int -> OpDoc a -> Result Error (OpDoc a)
listMove path from to doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case Node.asMov node of
                    Just _ ->
                        case elemIdAt from node of
                            Just valueId ->
                                let
                                    -- We anchor the moved item *after* the item that
                                    -- should precede it at the destination, computed
                                    -- against the list with the moved item removed.
                                    -- Moving DOWN (to > from): removing the item
                                    -- shifts later indices down by one, so the new
                                    -- predecessor is the item currently at `to`.
                                    -- Moving UP (to <= from): the predecessor is the
                                    -- item currently at `to - 1`.
                                    after =
                                        if to <= 0 then
                                            Nothing

                                        else if to > from then
                                            anchorBefore (to + 1) node

                                        else
                                            anchorBefore to node

                                    ( id, doc1 ) =
                                        mint doc
                                in
                                Ok (commit [ op id (frontierOf doc1) (MoveElem { container = target, elem = valueId, after = after }) ] doc1)

                            Nothing ->
                                Err (PathNotFound ("list index " ++ String.fromInt from))

                    Nothing ->
                        Err (WrongNodeType "expected movable list for listMove")
            )


{-| Add a new node to the tree at `path`, as the **last child** of `parent`
(`Nothing` = a new root), seeded from `seed`. The new node's id is minted here.
-}
treeAddChild : Path -> Maybe OpId -> Seed -> OpDoc a -> Result Error (OpDoc a)
treeAddChild path parent seed doc =
    treeContainer path doc
        |> Result.map
            (\( target, t ) ->
                let
                    ( childId, ctx1 ) =
                        Id.nextId (ctxOf doc)

                    ( moveOp, ctx2 ) =
                        Id.nextId ctx1

                    ( seedNode, ctx3 ) =
                        I.runSeed seed ctx2

                    pos =
                        endPos parent t

                    doc1 =
                        withCtx ctx3 doc
                in
                commit
                    [ op moveOp (frontierOf doc1) (TreeMove { container = target, child = childId, parent = parent, pos = pos, seed = Just seedNode }) ]
                    doc1
            )


{-| Re-parent `child` to be the **last child** of `parent` (`Nothing` = a root).
Cycle-forming moves are skipped at read (the node stays put), so this always
converges. No seed — the node keeps its content.
-}
treeMoveInto : Path -> OpId -> Maybe OpId -> OpDoc a -> Result Error (OpDoc a)
treeMoveInto path child parent doc =
    treeMoveTo path child parent (\t -> endPos parent t) doc


{-| Move `child` to sit immediately **before** `sibling` (same parent as sibling).
-}
treeMoveBefore : Path -> OpId -> OpId -> OpDoc a -> Result Error (OpDoc a)
treeMoveBefore path child sibling doc =
    treeMoveTo path child (currentParent path sibling doc) (\t -> beforePos sibling t) doc


{-| Move `child` to sit immediately **after** `sibling` (same parent as sibling).
-}
treeMoveAfter : Path -> OpId -> OpId -> OpDoc a -> Result Error (OpDoc a)
treeMoveAfter path child sibling doc =
    treeMoveTo path child (currentParent path sibling doc) (\t -> afterPos sibling t) doc


{-| Delete a tree node (and its subtree, at read) at `path`.
-}
treeRemove : Path -> OpId -> OpDoc a -> Result Error (OpDoc a)
treeRemove path child doc =
    treeContainer path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = child }) ] doc1
            )


{-| Shared move emitter: resolve the tree container, compute the position with
`posOf`, emit a seedless `TreeMove`.
-}
treeMoveTo : Path -> OpId -> Maybe OpId -> (Tree.Tree Node -> Crdt.Frac.Frac) -> OpDoc a -> Result Error (OpDoc a)
treeMoveTo path child parent posOf doc =
    treeContainer path doc
        |> Result.map
            (\( target, t ) ->
                let
                    ( moveOp, doc1 ) =
                        mint doc
                in
                commit
                    [ op moveOp (frontierOf doc1) (TreeMove { container = target, child = child, parent = parent, pos = posOf t, seed = Nothing }) ]
                    doc1
            )


{-| Resolve a path to a tree node: its id-target plus the `Tree` value.
-}
treeContainer : Path -> OpDoc a -> Result Error ( List TargetStep, Tree.Tree Node )
treeContainer path doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case Node.asTree node of
                    Just t ->
                        Ok ( target, t )

                    Nothing ->
                        Err (WrongNodeType "expected tree")
            )


{-| The current parent of `sibling` in the tree at `path` (for before/after moves).
-}
currentParent : Path -> OpId -> OpDoc a -> Maybe OpId
currentParent path sibling doc =
    treeContainer path doc
        |> Result.toMaybe
        |> Maybe.andThen (\( _, t ) -> Tree.parentOf sibling t)


{-| A fractional position after the last child of `parent` (append at end).
-}
endPos : Maybe OpId -> Tree.Tree Node -> Crdt.Frac.Frac
endPos parent t =
    let
        siblings =
            childIds parent t
    in
    case List.reverse siblings |> List.head of
        Just last ->
            Crdt.Frac.between (Tree.siblingPos last t) Nothing

        Nothing ->
            Crdt.Frac.between Nothing Nothing


{-| A position immediately before `sibling`.
-}
beforePos : OpId -> Tree.Tree Node -> Crdt.Frac.Frac
beforePos sibling t =
    let
        siblings =
            childIds (Tree.parentOf sibling t) t

        prev =
            precedingSibling sibling siblings
    in
    Crdt.Frac.between (prev |> Maybe.andThen (\p -> Tree.siblingPos p t)) (Tree.siblingPos sibling t)


{-| A position immediately after `sibling`.
-}
afterPos : OpId -> Tree.Tree Node -> Crdt.Frac.Frac
afterPos sibling t =
    let
        siblings =
            childIds (Tree.parentOf sibling t) t

        next =
            followingSibling sibling siblings
    in
    Crdt.Frac.between (Tree.siblingPos sibling t) (next |> Maybe.andThen (\n -> Tree.siblingPos n t))


childIds : Maybe OpId -> Tree.Tree Node -> List OpId
childIds parent t =
    case parent of
        Just p ->
            Tree.childrenOf p t

        Nothing ->
            Tree.roots t


precedingSibling : OpId -> List OpId -> Maybe OpId
precedingSibling target ids =
    List.foldl
        (\id ( prev, found ) ->
            if found then
                ( prev, found )

            else if Id.opIdToString id == Id.opIdToString target then
                ( prev, True )

            else
                ( Just id, False )
        )
        ( Nothing, False )
        ids
        |> Tuple.first


followingSibling : OpId -> List OpId -> Maybe OpId
followingSibling target ids =
    case ids of
        x :: rest ->
            if Id.opIdToString x == Id.opIdToString target then
                List.head rest

            else
                followingSibling target rest

        [] ->
            Nothing


{-| Emit an append op after `after`, then record `elemId` as the new last id for
`target` so the next append to this list is O(1). `commit` clears `lastAppend`
first (any edit invalidates it), so we re-establish it here afterwards.
-}
emitAppend : List TargetStep -> Maybe OpId -> Seed -> OpDoc a -> OpDoc a
emitAppend target after seed (OpDoc d) =
    let
        ( elemId, ctx1 ) =
            Id.nextId d.ctx

        ( seedNode, ctx2 ) =
            I.runSeed seed ctx1

        doc1 =
            OpDoc { d | ctx = ctx2 }

        committed =
            commit
                [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, parent = after, side = Rga.Right, seed = seedNode }) ]
                doc1
    in
    case committed of
        OpDoc cd ->
            OpDoc { cd | lastAppend = Just ( target, elemId ) }



-- DICT -----------------------------------------------------------------------


{-| Set (or overwrite) a dictionary key to a fresh subtree, marking it present.
-}
setKey : Path -> String -> Seed -> OpDoc a -> Result Error (OpDoc a)
setKey path k seed doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    (OpDoc d) =
                        doc

                    ( id, ctx1 ) =
                        Id.nextId d.ctx

                    ( seedNode, ctx2 ) =
                        I.runSeed seed ctx1

                    doc1 =
                        OpDoc { d | ctx = ctx2 }
                in
                commit
                    [ op id (frontierOf doc1) (SetPresence { target = target ++ [ IntoKey k ], present = True, seed = seedNode }) ]
                    doc1
            )


{-| Remove a dictionary key (LWW presence tombstone).
-}
removeKey : Path -> String -> OpDoc a -> Result Error (OpDoc a)
removeKey path k doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit
                    [ op id (frontierOf doc1) (SetPresence { target = target ++ [ IntoKey k ], present = False, seed = emptyMap }) ]
                    doc1
            )



-- PATH RESOLUTION ------------------------------------------------------------


{-| Resolve a visible-index `Path` against the current materialized state into a
stable, id-based `Target` plus the node found there. This is the bridge from the
index-addressed public API to the identity-addressed op model — list indices
become element `OpId`s, so the emitted op is position-independent.
-}
resolve : Path -> OpDoc a -> Result Error ( List TargetStep, Node )
resolve path doc =
    walk (Path.segments path) (state doc) []



-- REF PRIMITIVES -------------------------------------------------------------
-- Node-free entry points that `Crdt.Ref` builds its typed `set`/`over` on.
-- They keep the `Node` type internal: callers pass a `Seed` (opaque) or a
-- sub-schema, never a `Node`.


{-| Overwrite whatever is at `path` so it reads as the value the `Seed` builds,
emitting the **minimal** ops to get there (so concurrent edits elsewhere survive).
The seed's node is compared against the current node with the same diff engine
`restoreTo` uses; a text target additionally gets a character-level diff so
collaborative text still merges by character. Used by `Crdt.Ref`'s `set`.
-}
seedNodeAt : Path -> Seed -> OpDoc a -> Result Error (OpDoc a)
seedNodeAt path seed doc =
    resolve path doc
        |> Result.map
            (\( target, current ) ->
                let
                    ( seeded, ctx1 ) =
                        I.runSeed seed (ctxOf doc)

                    doc1 =
                        withCtx ctx1 doc
                in
                case ( current, seeded ) of
                    ( Node.Txt rga, Node.Txt _ ) ->
                        -- preserve character-wise merge for text leaves
                        applyTextDiff target rga (textOfNode seeded) doc1

                    _ ->
                        -- general case: emit the diff ops (registers, counters,
                        -- maps/records, sum-type $tag switches, sequences)
                        restoreNode target seeded current doc1
            )


{-| Read the typed value at `path` through a sub-schema. `Crdt.Ref`'s `over` uses
this to fetch the current value, apply a function, and write it back with
`seedNodeAt`. Keeps `Node` internal (the sub-schema decodes it).
-}
subValue : Crdt kind sub -> Path -> OpDoc a -> Result Error sub
subValue schema path doc =
    resolve path doc
        |> Result.andThen
            (\( _, node ) ->
                Schema.decodeNode schema node
                    |> Result.mapError (\e -> WrongNodeType (Schema.errorToString e))
            )


{-| The visible string of a `Txt` node (its char registers concatenated).
-}
textOfNode : Node -> String
textOfNode node =
    case Node.asTxt node of
        Just rga ->
            Rga.toList rga
                |> List.filterMap
                    (\n ->
                        case Node.asPrim n of
                            Just (PString s) ->
                                Just s

                            _ ->
                                Nothing
                    )
                |> String.concat

        Nothing ->
            ""


walk : List Seg -> Node -> List TargetStep -> Result Error ( List TargetStep, Node )
walk segs node acc =
    case segs of
        [] ->
            Ok ( List.reverse acc, node )

        seg :: rest ->
            case seg of
                Field name ->
                    intoKey name rest node acc

                Key name ->
                    intoKey name rest node acc

                Index i ->
                    case node of
                        Node.Seq rga ->
                            intoElem i rest rga node acc

                        Node.Txt rga ->
                            intoElem i rest rga node acc

                        Node.Mov ml ->
                            -- descend into the value at visible index `i` by its
                            -- valueId, so nested edits address it stably across moves
                            case elemIdAt i node of
                                Just valueId ->
                                    case MoveList.get valueId ml of
                                        Just content ->
                                            walk rest content (IntoElem valueId :: acc)

                                        Nothing ->
                                            Err (PathNotFound ("index " ++ String.fromInt i))

                                Nothing ->
                                    Err (PathNotFound ("index " ++ String.fromInt i))

                        _ ->
                            Err (WrongNodeType ("expected sequence at index " ++ String.fromInt i))

                NodeId nodeId ->
                    -- descend into a tree node by its stable id
                    case node of
                        Node.Tree t ->
                            case Tree.get nodeId t of
                                Just content ->
                                    walk rest content (IntoElem nodeId :: acc)

                                Nothing ->
                                    Err (PathNotFound ("tree node " ++ Id.opIdToString nodeId))

                        _ ->
                            Err (WrongNodeType "expected tree for node id")


intoKey : String -> List Seg -> Node -> List TargetStep -> Result Error ( List TargetStep, Node )
intoKey name rest node acc =
    case Node.asMap node of
        Just entries ->
            case Dict.get name entries of
                Just entry ->
                    walk rest entry.value (IntoKey name :: acc)

                Nothing ->
                    Err (PathNotFound ("key/field " ++ name))

        Nothing ->
            Err (WrongNodeType ("expected map at " ++ name))


intoElem : Int -> List Seg -> Rga.Rga Node -> Node -> List TargetStep -> Result Error ( List TargetStep, Node )
intoElem i rest rga _ acc =
    case Rga.idAtVisibleIndex i rga of
        Just elemId ->
            case Rga.get elemId rga of
                Just el ->
                    walk rest el.content (IntoElem elemId :: acc)

                Nothing ->
                    Err (PathNotFound ("index " ++ String.fromInt i))

        Nothing ->
            Err (PathNotFound ("index " ++ String.fromInt i))



-- HELPERS --------------------------------------------------------------------


op : OpId -> OpLog.Frontier -> Action -> Op
op id deps action =
    { id = id, deps = deps, action = action }


frontierOf : OpDoc a -> OpLog.Frontier
frontierOf (OpDoc d) =
    OpLog.frontier d.store


emptyMap : Node
emptyMap =
    Node.mapFromEntries Dict.empty


commonPrefix : List Char -> List Char -> Int -> Int
commonPrefix a b acc =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            if x == y then
                commonPrefix xs ys (acc + 1)

            else
                acc

        _ ->
            acc


commonSuffix : List Char -> List Char -> Int -> Int -> Int
commonSuffix ra rb acc cap =
    if acc >= cap then
        acc

    else
        case ( ra, rb ) of
            ( x :: xs, y :: ys ) ->
                if x == y then
                    commonSuffix xs ys (acc + 1) cap

                else
                    acc

            _ ->
                acc
