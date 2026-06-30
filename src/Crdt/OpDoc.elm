module Crdt.OpDoc exposing
    ( OpDoc, Error(..)
    , init, read, merge
    , setText, setBool, setInt, setString, increment
    , listAppend, listRemove, listMove
    , setKey, removeKey
    , opCount, cacheConsistent
    , cursorAt, cursorOffset, cursorRange
    , Version, version, readAt
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
    , gc
    , encode, encodeSince, decodeInto
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
@docs Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
@docs gc
@docs encode, encodeSince, decodeInto

-}

import Array
import Crdt.Cursor as Cursor exposing (Cursor)
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
        { schema : Crdt a
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
        }


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
init : ReplicaId -> Crdt a -> OpDoc a
init replica schema =
    let
        ( base, ctx ) =
            Schema.emptyNode schema (Id.ctx replica)
    in
    OpDoc
        { schema = schema
        , base = base
        , store = OpLog.empty
        , ctx = ctx
        , cached = base
        , lastAppend = Nothing
        , checkpoints = []
        , baseFrontier = []
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
    Schema.decodeNode d.schema (state doc)


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
    Schema.decodeNode d.schema (stateAt v doc)



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

        -- origin for the first inserted char: the visible element before `prefix`
        startOrigin =
            if prefix <= 0 then
                Nothing

            else
                Array.get (prefix - 1) ids

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

        -- insert ops, chaining each char after the previous
        ( finalDoc, insertOpsRev, _ ) =
            List.foldl
                (\char ( d, acc, origin ) ->
                    let
                        ( elemId, d1 ) =
                            mint d

                        seedNode =
                            Node.reg (PString (String.fromChar char)) elemId
                    in
                    ( d1
                    , op elemId deps (InsertElem { container = target, elemId = elemId, after = origin, seed = seedNode }) :: acc
                    , Just elemId
                    )
                )
                ( afterDeletes, [], startOrigin )
                insertChars
    in
    commit (deleteOps ++ List.reverse insertOpsRev) finalDoc



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
                [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, after = after, seed = seedNode }) ]
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
