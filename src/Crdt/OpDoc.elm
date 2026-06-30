module Crdt.OpDoc exposing
    ( OpDoc, Error(..)
    , init, read, merge
    , setText, setBool, setInt, setString, increment
    , listAppend, listRemove
    , setKey, removeKey
    , opCount, cacheConsistent
    , Version, version, readAt
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
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
@docs listAppend, listRemove
@docs setKey, removeKey
@docs opCount, cacheConsistent
@docs Version, version, readAt
@docs Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
@docs encode, encodeSince, decodeInto

-}

import Array
import Crdt.Id as Id exposing (Ctx, OpId, ReplicaId)
import Crdt.Internal as I exposing (Seed)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.OpJson as OpJson
import Crdt.OpLog as OpLog exposing (Action(..), Op, OpStore, TargetStep(..))
import Crdt.Path as Path exposing (Path, Seg(..))
import Crdt.Rga as Rga
import Crdt.Schema as Schema exposing (Crdt)
import Dict
import Json.Decode as JD
import Json.Encode as JE


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


{-| Serialize the document's full op set to JSON for transport. Use for first
contact / catch-up; for steady-state sync prefer `encodeSince`.
-}
encode : OpDoc a -> JE.Value
encode (OpDoc d) =
    OpJson.encodeOps (OpLog.ops d.store)


{-| Serialize only the ops a peer at `Version` is missing — the delta. The peer
advertises its `version`; we send back everything not causally behind it. Far
smaller than `encode` once a document has history, and correct regardless of
delivery order (the delta is defined by the causal DAG, not counter comparison).
-}
encodeSince : Version -> OpDoc a -> JE.Value
encodeSince (Version known) (OpDoc d) =
    OpJson.encodeOps (OpLog.opsAfter known d.store)


{-| Decode ops received from a peer and merge them into this document. Unknown or
already-held ops are harmless (op-store union is idempotent); the cache is
re-materialized and the clock advanced past everything seen.
-}
decodeInto : JE.Value -> OpDoc a -> Result String (OpDoc a)
decodeInto value (OpDoc d) =
    JD.decodeValue OpJson.opsDecoder value
        |> Result.mapError JD.errorToString
        |> Result.map
            (\incomingOps ->
                let
                    store =
                        List.foldl OpLog.insert d.store incomingOps

                    cached =
                        OpLog.materialize d.base store
                in
                OpDoc
                    { d
                        | store = store
                        , ctx = Id.observe (Node.maxCounter cached) d.ctx
                        , cached = cached
                        , lastAppend = Nothing
                    }
            )



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
    Version (OpLog.frontier d.store)


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
                case node of
                    Node.Seq rga ->
                        let
                            after =
                                case appendCacheFor target doc of
                                    Just cachedLast ->
                                        Just cachedLast

                                    Nothing ->
                                        Rga.lastVisibleId rga
                        in
                        Ok (emitAppend target after seed doc)

                    _ ->
                        Err (WrongNodeType "expected list node for listAppend")
            )


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
                case node of
                    Node.Seq rga ->
                        case Rga.idAtVisibleIndex i rga of
                            Just elemId ->
                                let
                                    ( id, doc1 ) =
                                        mint doc
                                in
                                Ok (commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = elemId }) ] doc1)

                            Nothing ->
                                Err (PathNotFound ("list index " ++ String.fromInt i))

                    _ ->
                        Err (WrongNodeType "expected list node for listRemove")
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
