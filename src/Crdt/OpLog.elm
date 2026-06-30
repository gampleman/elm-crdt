module Crdt.OpLog exposing
    ( Op, Action(..), Target, TargetStep(..), Frontier
    , OpStore, empty, insert, ops, member, merge
    , causalOrder, applyOps, materialize, checkout
    , frontier, opsAfter, compact, ancestorKeys
    )

{-| The operation log: the source of truth for an op-log-based document.

A document's state is _derived_ by folding the ops into a `Node`
(`materialize`), which the typed `Crdt.Schema` layer then reads. Conflict
resolution (LWW for registers, RGA for sequences) lives in the fold, not in a
structural `merge`. Merging two documents is set-union of their op stores —
trivially commutative, associative and idempotent.

Each `Op` records its causal `deps` (the frontier it was created against), so the
store forms a DAG. `materialize` folds ops in a **causal order** (every op after
all its dependencies); the result is independent of which valid causal
linearization is chosen. Causal order matters: a `DeleteElem` applied before its
target's `InsertElem` would be a silent no-op, so only an order that honours
`deps` is correct.

This is the Phase 1 core from `docs/02-oplog.md`: real DAG + frontiers + causal
materialize + checkout. It is not yet wired into the public `Crdt` module.

@docs Op, Action, Target, TargetStep, Frontier
@docs OpStore, empty, insert, ops, member, merge
@docs causalOrder, applyOps, materialize, checkout
@docs frontier, opsAfter, compact, ancestorKeys

-}

import Crdt.Id as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node(..), Prim)
import Crdt.Rga as Rga
import Dict exposing (Dict)
import Set exposing (Set)


{-| A point in the causal DAG: the set of op ids that are the current "tips"
(ops not yet depended upon). An op created against a frontier lists it as `deps`.
-}
type alias Frontier =
    List OpId


{-| A single operation: its unique id, the frontier it causally follows, and what
it does.
-}
type alias Op =
    { id : OpId
    , deps : Frontier
    , action : Action
    }


{-| What an op does. `Target` addresses state by stable identity, so every action
is position-independent.
-}
type Action
    = SetReg Target Prim
    | SetPresence { target : Target, present : Bool, seed : Node }
    | InsertElem { container : Target, elemId : OpId, after : Maybe OpId, seed : Node }
    | DeleteElem { container : Target, elem : OpId }
    | MoveElem { container : Target, elem : OpId, after : Maybe OpId }
    | Increment { target : Target, delta : Int }


{-| A path into the document by stable identity: map keys by name, sequence/text
elements by their `OpId`.
-}
type alias Target =
    List TargetStep


{-| One navigation step of a `Target`.
-}
type TargetStep
    = IntoKey String
    | IntoElem OpId



-- STORE ----------------------------------------------------------------------


{-| A set of ops, keyed by id. The DAG; ops are immutable and ids unique, so a
shared id always maps to an identical op.
-}
type OpStore
    = OpStore (Dict String Op)


{-| The empty store.
-}
empty : OpStore
empty =
    OpStore Dict.empty


{-| Add an op (idempotent — re-adding an existing id is a no-op).
-}
insert : Op -> OpStore -> OpStore
insert op (OpStore d) =
    OpStore (Dict.insert (Id.opIdToString op.id) op d)


{-| All ops, unordered.
-}
ops : OpStore -> List Op
ops (OpStore d) =
    Dict.values d


{-| Whether an op id is present.
-}
member : OpId -> OpStore -> Bool
member id (OpStore d) =
    Dict.member (Id.opIdToString id) d


{-| Merge two stores: set-union by id. Commutative, associative, idempotent.
-}
merge : OpStore -> OpStore -> OpStore
merge (OpStore a) (OpStore b) =
    OpStore (Dict.union a b)


{-| The current frontier: ops that no other op depends on (the DAG tips).
-}
frontier : OpStore -> Frontier
frontier (OpStore d) =
    let
        depended : Set String
        depended =
            Dict.foldl
                (\_ op acc -> List.foldl (\dep s -> Set.insert (Id.opIdToString dep) s) acc op.deps)
                Set.empty
                d
    in
    Dict.values d
        |> List.map .id
        |> List.filter (\id -> not (Set.member (Id.opIdToString id) depended))



-- CAUSAL ORDER ---------------------------------------------------------------


{-| Linearize the store into a causal order: every op appears after all of its
`deps`. Among ops that are simultaneously ready, ties break by `OpId` so the
order is deterministic.

Implementation is a Kahn-style topological sort. Ops whose `deps` point outside
the store (e.g. a delta that hasn't brought in an ancestor) are treated as ready
once their _present_ deps are satisfied, so a partial store still materializes.

-}
causalOrder : OpStore -> List Op
causalOrder (OpStore d) =
    let
        present : Set String
        present =
            Set.fromList (Dict.keys d)

        -- count of still-unprocessed deps that are actually in the store
        initialPending : Op -> Int
        initialPending op =
            op.deps
                |> List.filter (\dep -> Set.member (Id.opIdToString dep) present)
                |> List.length
    in
    kahn d (Dict.map (\_ op -> initialPending op) d) []


{-| Kahn's algorithm. `pending` maps each op key to how many in-store deps remain
unprocessed; we repeatedly emit the lowest-`OpId` op with zero pending and
decrement its dependents.
-}
kahn : Dict String Op -> Dict String Int -> List Op -> List Op
kahn allOps pending acc =
    let
        ready =
            pending
                |> Dict.filter (\_ n -> n <= 0)
                |> Dict.keys
                |> List.filterMap (\k -> Dict.get k allOps)
                |> List.sortWith (\x y -> Id.compareOpId x.id y.id)
    in
    case ready of
        [] ->
            -- nothing ready: either done, or a cycle (impossible under Lamport
            -- deps). Emit any remaining by id to stay total.
            List.reverse acc
                ++ (Dict.values (Dict.filter (\k _ -> not (processed acc k)) allOps)
                        |> List.sortWith (\x y -> Id.compareOpId x.id y.id)
                   )

        next :: _ ->
            let
                nextKey =
                    Id.opIdToString next.id

                -- remove `next` from pending and decrement everyone depending on it
                pending1 =
                    pending
                        |> Dict.remove nextKey
                        |> Dict.map
                            (\k n ->
                                case Dict.get k allOps of
                                    Just op ->
                                        if List.any (\dep -> Id.opIdToString dep == nextKey) op.deps then
                                            n - 1

                                        else
                                            n

                                    Nothing ->
                                        n
                            )
            in
            kahn allOps pending1 (next :: acc)


processed : List Op -> String -> Bool
processed acc key =
    List.any (\op -> Id.opIdToString op.id == key) acc



-- MATERIALIZE ----------------------------------------------------------------


{-| Fold ops over a base node **in the given order** (no sorting). Callers are
responsible for passing a causal order; `materialize` does that for you.
-}
applyOps : Node -> List Op -> Node
applyOps base orderedOps =
    List.foldl applyOp base orderedOps


{-| Materialize the whole store onto a base node (the schema's empty structure),
folding in causal order.
-}
materialize : Node -> OpStore -> Node
materialize base store =
    applyOps base (causalOrder store)


{-| Materialize the state as of a `Frontier`: only ops that are causal ancestors
of (or equal to) the frontier are folded. This is time-travel / checkout.
-}
checkout : Frontier -> Node -> OpStore -> Node
checkout target base store =
    materialize base (ancestorsOf target store)


{-| The set of op-id keys that are causal ancestors of (or equal to) the frontier
ops. Walks `deps` transitively.
-}
ancestorKeys : Frontier -> OpStore -> Set String
ancestorKeys target store =
    let
        collect : List OpId -> Set String -> Set String
        collect queue seen =
            case queue of
                [] ->
                    seen

                id :: rest ->
                    let
                        key =
                            Id.opIdToString id
                    in
                    if Set.member key seen then
                        collect rest seen

                    else
                        case lookup id store of
                            Just op ->
                                collect (op.deps ++ rest) (Set.insert key seen)

                            Nothing ->
                                collect rest seen
    in
    collect target Set.empty


{-| The sub-store of all causal ancestors of the frontier ops, plus the frontier
ops themselves.
-}
ancestorsOf : Frontier -> OpStore -> OpStore
ancestorsOf target store =
    let
        keep =
            ancestorKeys target store
    in
    ops store
        |> List.filter (\op -> Set.member (Id.opIdToString op.id) keep)
        |> List.foldl insert empty


{-| The ops a peer at `known` (their frontier) has **not** yet seen — every op
that is not a causal ancestor of `known`. This is the delta to send them.

Correct regardless of delivery order or counter gaps: it is defined purely by the
causal DAG, not by comparing Lamport counters (which a version-vector would do,
and which is unsafe under our gappy `Id.observe` clock).

-}
opsAfter : Frontier -> OpStore -> List Op
opsAfter known store =
    let
        seen =
            ancestorKeys known store
    in
    ops store
        |> List.filter (\op -> not (Set.member (Id.opIdToString op.id) seen))


{-| Compact the store at a causal `cut`: fold every op at-or-below `cut` into
`base` (producing a new base that already incorporates them), and return that new
base together with the store of the ops **not** below `cut`.

The defining guarantee (the read model is unchanged):

    materialize base store
        ==  let ( base', store' ) = compact base cut store
            in materialize base' store'

for **any** `cut`. This holds because folding is associative over a causal order
and `cut` is a causal cut: the ≤cut ops fold into `base'` exactly as they would
during a full materialize, and the remaining ops — whose `deps` may now point
into `base'` — still linearize correctly (`causalOrder` already treats deps
absent from the store as satisfied, and `base'` contains their effect).

`compact` is a pure, equivalence-preserving rewrite of `(base, store)`. It is
**not** a soundness decision: whether a given `cut` is safe to _discard_ across
merges is the caller's policy (see `docs/04-gc.md`) — `compact` itself never
loses information that `materialize` would have used.

-}
compact : Node -> Frontier -> OpStore -> ( Node, OpStore )
compact base cut store =
    let
        belowCut =
            ancestorKeys cut store

        ( below, above ) =
            ops store
                |> List.partition (\op -> Set.member (Id.opIdToString op.id) belowCut)

        base1 =
            applyOps base (causalOrder (List.foldl insert empty below))

        store1 =
            List.foldl insert empty above
    in
    ( base1, store1 )


lookup : OpId -> OpStore -> Maybe Op
lookup id (OpStore d) =
    Dict.get (Id.opIdToString id) d



-- APPLY ONE OP ---------------------------------------------------------------


applyOp : Op -> Node -> Node
applyOp { id, action } root =
    case action of
        SetReg target prim ->
            updateAt target id (setRegLww id prim) root

        SetPresence { target, present, seed } ->
            setPresenceAt target id present seed root

        InsertElem { container, elemId, after, seed } ->
            updateAt container id (insertElem elemId after seed) root

        DeleteElem { container, elem } ->
            updateAt container id (deleteElem elem) root

        MoveElem { container, elem, after } ->
            updateAt container id (moveElem id elem after) root

        Increment { target, delta } ->
            updateAt target id (incrementBy id delta) root


{-| Apply `f` at `steps`, creating intermediate map entries (stamped `stamp`) as
needed. Sequence/text elements are descended into by id.
-}
updateAt : List TargetStep -> OpId -> (Node -> Node) -> Node -> Node
updateAt steps stamp f node =
    case steps of
        [] ->
            f node

        (IntoKey k) :: rest ->
            let
                entries =
                    case node of
                        Map d ->
                            d

                        _ ->
                            Dict.empty

                existing =
                    Dict.get k entries

                childNode =
                    case existing of
                        Just e ->
                            e.value

                        Nothing ->
                            emptyMap

                newEntry =
                    case existing of
                        Just e ->
                            { e | value = updateAt rest stamp f childNode }

                        Nothing ->
                            Node.entry stamp True (updateAt rest stamp f childNode)
            in
            Node.mapFromEntries (Dict.insert k newEntry entries)

        (IntoElem x) :: rest ->
            case node of
                Seq rga ->
                    Seq (Rga.updateElement x (updateAt rest stamp f) rga)

                Txt rga ->
                    Txt (Rga.updateElement x (updateAt rest stamp f) rga)

                Mov ml ->
                    -- descend into the value `x` (by valueId) and edit its content
                    Mov (MoveList.updateValue x (updateAt rest stamp f) ml)

                _ ->
                    node


{-| Set a map key's presence (LWW by stamp), creating the key if absent. This is
how dict set/remove is expressed as an op. A newly-created key is seeded with the
value sub-schema's empty structure (carried by the op), not a bare map — the same
seeding rule `InsertElem` follows.
-}
setPresenceAt : List TargetStep -> OpId -> Bool -> Node -> Node -> Node
setPresenceAt steps stamp present seed node =
    case steps of
        [] ->
            node

        [ IntoKey k ] ->
            let
                entries =
                    case node of
                        Map d ->
                            d

                        _ ->
                            Dict.empty
            in
            case Dict.get k entries of
                Just e ->
                    if Id.compareOpId stamp e.stamp == GT then
                        Node.mapFromEntries (Dict.insert k { e | present = present, stamp = stamp } entries)

                    else
                        Node.mapFromEntries entries

                Nothing ->
                    Node.mapFromEntries (Dict.insert k (Node.entry stamp present seed) entries)

        step :: rest ->
            updateAt [ step ] stamp (\child -> setPresenceAt rest stamp present seed child) node


emptyMap : Node
emptyMap =
    Node.mapFromEntries Dict.empty


setRegLww : OpId -> Prim -> Node -> Node
setRegLww stamp prim current =
    case current of
        Reg r ->
            if Id.compareOpId stamp r.stamp == GT then
                Node.reg prim stamp

            else
                current

        _ ->
            Node.reg prim stamp


insertElem : OpId -> Maybe OpId -> Node -> Node -> Node
insertElem elemId after seed current =
    case current of
        Txt rga ->
            Txt (Rga.put (Rga.element elemId after seed False) rga)

        Mov ml ->
            -- movable list: elemId is the valueId, `after` the cell to follow
            Mov (MoveList.insert elemId after seed ml)

        Seq rga ->
            Seq (Rga.put (Rga.element elemId after seed False) rga)

        _ ->
            Seq (Rga.put (Rga.element elemId after seed False) Rga.empty)


deleteElem : OpId -> Node -> Node
deleteElem elem current =
    case current of
        Seq rga ->
            Seq (Rga.delete elem rga)

        Txt rga ->
            Txt (Rga.delete elem rga)

        Mov ml ->
            Mov (MoveList.delete elem ml)

        _ ->
            current


{-| Move value `elem` after cell `after` in a movable list (no-op on other nodes).
The move op's id (`moveOp`) becomes the new cell id, so it wins by max-id LWW.
-}
moveElem : OpId -> OpId -> Maybe OpId -> Node -> Node
moveElem moveOp elem after current =
    case current of
        Mov ml ->
            Mov (MoveList.move moveOp elem after ml)

        _ ->
            current


{-| Add a signed contribution (keyed by this op's id) to a counter node, creating
the counter if the target was empty. Idempotent: replaying the op re-inserts the
same keyed contribution.
-}
incrementBy : OpId -> Int -> Node -> Node
incrementBy stamp delta current =
    let
        contributions =
            case current of
                Cnt d ->
                    d

                _ ->
                    Dict.empty
    in
    Cnt (Dict.insert (Id.opIdToString stamp) (Node.increment stamp delta) contributions)
