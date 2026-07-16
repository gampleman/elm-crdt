module Crdt.OpLog exposing
    ( Op, Action(..), Target, TargetStep(..), Frontier
    , OpStore, empty, insert, ops, member, merge
    , causalOrder, applyOps, opsMaxCounter, materialize, addedOpsInOrder, addedFromCandidates, checkout
    , frontier, advanceFrontier, opsAfter, ancestorsOf, compact, ancestorKeys, stableFrontier
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

The op-log core: real DAG + frontiers + causal materialize + checkout. `Crdt.Doc.Internal`
drives it — every document is materialized from an `OpStore`.

@docs Op, Action, Target, TargetStep, Frontier
@docs OpStore, empty, insert, ops, member, merge
@docs causalOrder, applyOps, opsMaxCounter, materialize, addedOpsInOrder, addedFromCandidates, checkout
@docs frontier, advanceFrontier, opsAfter, ancestorsOf, compact, ancestorKeys, stableFrontier

-}

import Crdt.Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node(..), Prim)
import Crdt.Rga as Rga
import Crdt.Tree.Internal as Tree
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
    | InsertElem { container : Target, elemId : OpId, parent : Maybe OpId, side : Rga.Side, seed : Node }
    | InsertText { container : Target, start : OpId, text : String, parent : Maybe OpId, side : Rga.Side }
    | DeleteElem { container : Target, elem : OpId }
    | MoveElem { container : Target, elem : OpId, after : Maybe OpId }
    | Increment { target : Target, delta : Int }
    | TreeMove { container : Target, child : OpId, parent : Maybe OpId, pos : Frac, seed : Maybe Node }
    | AddMark { container : Target, markId : OpId, type_ : String, value : Prim, start : Node.MarkAnchor, end : Node.MarkAnchor }


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


{-| Advance a known frontier by a batch of newly-added ops, **without** rescanning the
whole store: the new tips are the old ones plus the new ops' ids, minus everything any
of the new ops depends on. `O(batch + |frontier|)` instead of `frontier`'s `O(store)`.

Correct only when `newOps` are genuinely new (their ids aren't already tips) and their
`deps` reference existing ops — which holds for both local `commit` (deps = the current
frontier) and applying a delta. For a wholesale store rebuild use `frontier`.

-}
advanceFrontier : Frontier -> List Op -> Frontier
advanceFrontier current newOps =
    let
        newDepended : Set String
        newDepended =
            List.foldl
                (\op acc -> List.foldl (\dep s -> Set.insert (Id.opIdToString dep) s) acc op.deps)
                Set.empty
                newOps

        keptOld =
            List.filter (\id -> not (Set.member (Id.opIdToString id) newDepended)) current

        addedTips =
            newOps
                |> List.map .id
                |> List.filter (\id -> not (Set.member (Id.opIdToString id) newDepended))
    in
    keptOld ++ addedTips



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
    -- Sorting ascending by `OpId` (counter-major) IS a valid causal linearization, in
    -- O(n log n). An op's `deps` are the frontier it was minted against, so by Lamport's
    -- rule (`Id.nextId` takes a counter strictly greater than everything observed, which
    -- includes all deps) **every dep has a strictly smaller counter than the op** — hence a
    -- smaller `OpId`. So in ascending-id order every op appears after all its deps, exactly
    -- what a causal order requires; and among concurrent ops it ties by id, deterministically.
    --
    -- This equals the old topological sort's output for well-formed input: that sort emitted
    -- the lowest-id *ready* op each step, and by the invariant the lowest-id unemitted op is
    -- always ready (its deps, all smaller-id, are already emitted). Its own fallback for the
    -- "impossible under Lamport deps" cycle case was likewise "emit remaining by id". So the
    -- id sort is faithful in every case and drops the O(n²) per-step rescans.
    Dict.values d
        |> List.sortWith (\x y -> Id.compareOpId x.id y.id)



-- MATERIALIZE ----------------------------------------------------------------


{-| Fold ops over a base node **in the given order** (no sorting). Callers are
responsible for passing a causal order; `materialize` does that for you.
-}
applyOps : Node -> List Op -> Node
applyOps base orderedOps =
    List.foldl applyOp base orderedOps


{-| The largest Lamport counter referenced by a batch of ops: each op's own id, plus
any stamps buried in an insert/move/tree/mark seed (a seed register can carry a counter
_higher_ than the op's own id — it was minted by a later edit on the source replica).

This is the O(batch) clock catch-up for ingesting a delta of untrusted wire ops, where
no incoming `Ctx` is available — replacing an O(whole-tree) `Node.maxCounter` scan of the
materialized result. (Merging two in-memory `Doc`s needs neither: each already keeps its
`Ctx` past its own stamps, so `max` of the two counters suffices.)

-}
opsMaxCounter : List Op -> Int
opsMaxCounter batch =
    List.foldl (\op acc -> max acc (opMaxCounter op)) 0 batch


opMaxCounter : Op -> Int
opMaxCounter op =
    let
        seedMax =
            case op.action of
                InsertElem { seed } ->
                    Node.maxCounter seed

                InsertText { start, text } ->
                    -- a run consumes `len` consecutive counters (start .. start+len-1),
                    -- each a char element id; the clock must clear the whole span.
                    Id.opIdCounter start + max 0 (String.length text - 1)

                SetPresence { seed } ->
                    Node.maxCounter seed

                TreeMove { seed } ->
                    seed |> Maybe.map Node.maxCounter |> Maybe.withDefault 0

                AddMark { start, end } ->
                    let
                        anchorMax a =
                            a.ref |> Maybe.map Id.opIdCounter |> Maybe.withDefault 0
                    in
                    max (anchorMax start) (anchorMax end)

                _ ->
                    0
    in
    max (Id.opIdCounter op.id) seedMax


{-| Materialize the whole store onto a base node (the schema's empty structure),
folding in causal order.
-}
materialize : Node -> OpStore -> Node
materialize base store =
    applyOps base (causalOrder store)


{-| The ops present in `merged` but **not** in `prior`, in `merged`'s causal order.

This is what lets a merge apply **incrementally**: a document whose cache already
reflects `prior` can fold just these added ops onto that cache instead of
re-materializing the whole store from base — the result is identical because every
`Action` is a commutative, idempotent function of the op _set_ with resolution deferred
to read (LWW-by-stamp, RGA/Fugue order, accretive counters/marks, tree/move re-fold).

They are causally ordered among **themselves only** — we sort a store of just the added
ops, not the whole merged store. `causalOrder` treats a dep absent from the store it is
sorting as already satisfied; the deps we drop this way are exactly the `prior` ops that
are already folded into the cache, so the order is valid **relative to that cache**.
Sorting only the added ops keeps a steady-state merge O(k²) in the small delta `k`
rather than O(n²) in the whole document — the difference between "merge cost scales with
the delta" and "with the doc".

-}
addedOpsInOrder : OpStore -> OpStore -> List Op
addedOpsInOrder prior (OpStore merged) =
    let
        added =
            Dict.foldl
                (\key op acc ->
                    if member op.id prior then
                        acc

                    else
                        Dict.insert key op acc
                )
                Dict.empty
                merged
    in
    causalOrder (OpStore added)


{-| The genuinely-new ops among `candidates` (those not already in `prior`), in causal
order. Like `addedOpsInOrder` but scanning only the **candidate list** rather than the
whole merged store — so ingesting a delta of `k` ops into an `n`-op store is `O(k)` to
find the new ops instead of `O(n)`. Use when the incoming ops are already in hand (the
`decodeInto`/delta path); `addedOpsInOrder` is for when only the merged store is known
(the `merge` path, which unions two full stores).

Candidates are deduped by id first (idempotent re-delivery of the same op is harmless),
then causally ordered among themselves — valid relative to the cache for the same reason
`addedOpsInOrder` is: the deps we drop are exactly the `prior` ops already folded in.

-}
addedFromCandidates : OpStore -> List Op -> List Op
addedFromCandidates prior candidates =
    candidates
        |> List.foldl
            (\op acc ->
                if member op.id prior then
                    acc

                else
                    Dict.insert (Id.opIdToString op.id) op acc
            )
            Dict.empty
        |> OpStore
        |> causalOrder


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


{-| The **stable frontier** across a set of peer frontiers: the causal cut that _every_
peer has delivered past. It is the frontier (tips) of the **intersection** of all the
peers' ancestor sets — the ops that every listed peer holds — computed against this store.

Compacting below it is safe across exactly those peers: any op missing from even one peer
is absent from the intersection (so it survives), and concurrent work a peer hasn't shipped
yet is nobody's ancestor (so it is never in the intersection). A peer **not** in the list
(disconnected/behind) is not protected — it must be caught up by a snapshot transfer on
reconnect, which the wire layer already does when a peer is behind `baseFrontier`.

An empty list yields the empty frontier (nothing is common → compact nothing).

-}
stableFrontier : List Frontier -> OpStore -> Frontier
stableFrontier peers ((OpStore d) as store) =
    case peers of
        [] ->
            []

        first :: rest ->
            let
                -- ops every peer has: intersect the ancestor-key sets.
                common =
                    List.foldl
                        (\f acc -> Set.intersect acc (ancestorKeys f store))
                        (ancestorKeys first store)
                        rest

                -- keep only ops in the common set, then take that sub-DAG's frontier.
                commonStore =
                    OpStore (Dict.filter (\k _ -> Set.member k common) d)
            in
            frontier commonStore


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

        InsertElem { container, elemId, parent, side, seed } ->
            updateAt container id (insertElem elemId parent side seed) root

        InsertText { container, start, text, parent, side } ->
            updateAt container id (insertTextRun start text parent side) root

        DeleteElem { container, elem } ->
            updateAt container id (deleteElem elem) root

        MoveElem { container, elem, after } ->
            updateAt container id (moveElem id elem after) root

        Increment { target, delta } ->
            updateAt target id (incrementBy id delta) root

        TreeMove { container, child, parent, pos, seed } ->
            updateAt container id (treeMove id child parent pos seed) root

        AddMark { container, markId, type_, value, start, end } ->
            updateAt container id (addMark markId type_ value start end) root


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

                Tree t ->
                    -- descend into tree node `x` (by nodeId) and edit its payload
                    Tree (Tree.updateValue x (updateAt rest stamp f) t)

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


insertElem : OpId -> Maybe OpId -> Rga.Side -> Node -> Node -> Node
insertElem elemId parent side seed current =
    case current of
        Txt rga ->
            Txt (Rga.put (Rga.element elemId parent side seed False) rga)

        Mov ml ->
            -- movable list: elemId is the valueId, `parent` the cell to follow. A
            -- MoveList cell is always a right-child (structural), so `side` is
            -- irrelevant here — MoveList manages its own cell ordering.
            Mov (MoveList.insert elemId parent seed ml)

        Seq rga ->
            Seq (Rga.put (Rga.element elemId parent side seed False) rga)

        Rich r ->
            -- rich text: characters live in the node's `.text` sequence
            Rich { r | text = Rga.put (Rga.element elemId parent side seed False) r.text }

        _ ->
            Seq (Rga.put (Rga.element elemId parent side seed False) Rga.empty)


{-| **Explode a run-length text op** into the same per-character right-spine the store
used to hold one op at a time. Char 0 anchors at `(parent, side)` with id `start`; char
`i` (i≥1) has the derived id `(start.counter + i, start.replica)` and anchors as a
**right-child of char i-1** — byte-for-byte the chain `applyCharDiff` emits.

The derivation is what makes this safe with no split logic: a concurrent insert that
anchors "after char i of this run" references exactly `start + i`, an id this explosion
already materialises, so the two runs order by the ordinary Fugue rule and `==`
convergence is preserved. Each char is a single-char `PString` register stamped with its
own id (same as the old per-char seed), so the materialised `Node` is identical to the
pre-change one.

-}
insertTextRun : OpId -> String -> Maybe OpId -> Rga.Side -> Node -> Node
insertTextRun start text parent side current =
    let
        replica =
            Id.opIdReplica start

        base =
            Id.opIdCounter start

        -- fold the chars left→right into `rga0`, threading the previous char's id as the
        -- next char's parent; the first char uses the op's own (parent, side).
        putChars rga0 =
            String.toList text
                |> List.foldl
                    (\ch ( i, prevId, rga ) ->
                        let
                            elemId =
                                if i == 0 then
                                    start

                                else
                                    Id.opId (base + i) replica

                            ( p, s ) =
                                case prevId of
                                    Just prev ->
                                        ( Just prev, Rga.Right )

                                    Nothing ->
                                        ( parent, side )

                            seed =
                                Node.reg (Node.PString (String.fromChar ch)) elemId
                        in
                        ( i + 1, Just elemId, Rga.put (Rga.element elemId p s seed False) rga )
                    )
                    ( 0, Nothing, rga0 )
                |> (\( _, _, rga ) -> rga)
    in
    case current of
        Txt rga ->
            Txt (putChars rga)

        Seq rga ->
            Seq (putChars rga)

        Rich r ->
            Rich { r | text = putChars r.text }

        _ ->
            Txt (putChars Rga.empty)


deleteElem : OpId -> Node -> Node
deleteElem elem current =
    case current of
        Seq rga ->
            Seq (Rga.delete elem rga)

        Txt rga ->
            Txt (Rga.delete elem rga)

        Mov ml ->
            Mov (MoveList.delete elem ml)

        Tree t ->
            Tree (Tree.delete elem t)

        Rich r ->
            Rich { r | text = Rga.delete elem r.text }

        _ ->
            current


{-| Place `child` under `parent` at `pos` in a tree. On creation the op carries a
`seed` (the new node's content); a re-parent/reorder carries `Nothing` and keeps
the existing payload. `moveOp` (this op's id) keys the move, so the latest move
wins by causal-order fold. No-op on non-tree nodes.
-}
treeMove : OpId -> OpId -> Maybe OpId -> Frac -> Maybe Node -> Node -> Node
treeMove moveOp child parent pos seed current =
    let
        t =
            case current of
                Tree existing ->
                    existing

                _ ->
                    Tree.empty
    in
    case seed of
        Just content ->
            Tree (Tree.move moveOp child parent pos content t)

        Nothing ->
            Tree (Tree.moveOnly moveOp child parent pos t)


{-| Record a mark op in a rich-text node's append-only mark set, keyed by the op's
id (idempotent — re-applying the same op is a no-op). No-op on non-rich nodes.
-}
addMark : OpId -> String -> Prim -> Node.MarkAnchor -> Node.MarkAnchor -> Node -> Node
addMark markId type_ value start end current =
    case current of
        Rich r ->
            let
                markOp =
                    { id = markId, type_ = type_, value = value, start = start, end = end }
            in
            Rich { r | marks = Dict.insert (Id.opIdToString markId) markOp r.marks }

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
