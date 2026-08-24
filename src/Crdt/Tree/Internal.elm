module Crdt.Tree.Internal exposing
    ( Tree
    , empty, fromParts, moves, payloads, deletedIds
    , move, moveOnly, updateValue, mapPayloads, delete
    , roots, childrenOf, get, parentOf, siblingPos, lastChildPos, maxCounter, reStamp
    , Move
    , Forest, Item, toForest, itemId, itemValue, itemChildren
    )

{-| A **movable tree**: hierarchical data (outlines, file trees, threaded comments)
where nodes can be re-parented _and_ reordered among their siblings, converging
under concurrent edits.

Two hard problems, two orthogonal mechanisms:

  - **Re-parenting can form a cycle** (A moved under B while B moved under A). We
    store every move in an append-only, union-merged set and derive the tree by
    folding the moves **sorted by `moveOp`** (`(counter, replica)` ascending),
    **skipping any move that would make a node its own ancestor**. Every replica
    holds the same move-set, sorts it identically, and makes identical skip
    decisions → identical trees. This is Kleppmann et al. 2020's move algorithm; it
    fits because the tree is derived at **read** time — `resolve` re-folds the whole
    move-set on every read — so a move that arrives late (out of `moveOp` order) is
    incorporated without their incremental undo/redo. A skipped move leaves its node
    at its previous parent (nothing vanishes).
  - **Sibling order** uses a **fractional index** (`Crdt.Frac`), a position _value_
    (not a "after sibling X" reference — that would let concurrent reorders cycle
    the way `Crdt.MoveList` avoids). Concurrent inserts at the same gap pick the
    same key and tiebreak by child `OpId`.

Storage is grow-only in every component (moves, payloads and tombstones only ever
accumulate, in any order), so convergence of the representation under op replay is free;
the tree is a pure function of it. `c` is the node payload (a `Node` in practice).

@docs Tree
@docs empty, fromParts, moves, payloads, deletedIds
@docs move, moveOnly, updateValue, mapPayloads, delete
@docs roots, childrenOf, get, parentOf, siblingPos, lastChildPos, maxCounter, reStamp
@docs Move
@docs Forest, Item, toForest, itemId, itemValue, itemChildren

-}

import Crdt.Frac as Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId)
import Dict exposing (Dict)
import Set exposing (Set)


{-| A movable tree of payloads `c`.

  - `moveSet` — append-only, keyed by `moveOp` string; a node's creation is its
    first move. Grow-only.
  - `payload` — nodeId → content, stable across moves; edited in place by ops that
    target the node.
  - `tombstones` — deleted nodeIds (grow-only, delete-wins). A deleted node's
    subtree is dropped at read.

-}
type Tree c
    = Tree
        { moveSet : Dict String Move
        , payload : Dict String c
        , tombstones : Set String
        }


{-| One recorded move: place `child` under `parent` (`Nothing` = a root) at
sibling position `pos`. Keyed in the set by `moveOp` (its minting `OpId`).
-}
type alias Move =
    { moveOp : OpId
    , child : OpId
    , parent : Maybe OpId
    , pos : Frac
    }


{-| The empty tree.
-}
empty : Tree c
empty =
    Tree { moveSet = Dict.empty, payload = Dict.empty, tombstones = Set.empty }


{-| Build from raw parts (JSON decode).
-}
fromParts : Dict String Move -> Dict String c -> Set String -> Tree c
fromParts moveSet payload tombstones =
    Tree { moveSet = moveSet, payload = payload, tombstones = tombstones }


{-| The move set (for serialization).
-}
moves : Tree c -> Dict String Move
moves (Tree t) =
    t.moveSet


{-| The nodeId → content map (for serialization).
-}
payloads : Tree c -> Dict String c
payloads (Tree t) =
    t.payload


{-| The deleted nodeId set (for serialization).
-}
deletedIds : Tree c -> Set String
deletedIds (Tree t) =
    t.tombstones



-- EDITS ----------------------------------------------------------------------


{-| Record a move of `child` under `parent` at position `pos`, minted as `moveOp`,
setting the node's payload to `content`. Creating a node is its first `move` (which is
what seeds that payload); use `moveOnly` to re-parent/reorder a node that already has
one. Appending a move never removes an older one — the older moves become inert
history (skipped at read), like RGA tombstones.
-}
move : OpId -> OpId -> Maybe OpId -> Frac -> c -> Tree c -> Tree c
move moveOp child parent pos content (Tree t) =
    Tree
        { t
            | moveSet =
                Dict.insert (Id.opIdToString moveOp)
                    { moveOp = moveOp, child = child, parent = parent, pos = pos }
                    t.moveSet
            , payload = Dict.insert (Id.opIdToString child) content t.payload
        }


{-| Record a move that carries no (new) payload — used when re-parenting/reordering
an existing node whose content already lives in `payload`.
-}
moveOnly : OpId -> OpId -> Maybe OpId -> Frac -> Tree c -> Tree c
moveOnly moveOp child parent pos (Tree t) =
    Tree
        { t
            | moveSet =
                Dict.insert (Id.opIdToString moveOp)
                    { moveOp = moveOp, child = child, parent = parent, pos = pos }
                    t.moveSet
        }


{-| Transform a node's content in place (nested edits into a node's payload).
-}
updateValue : OpId -> (c -> c) -> Tree c -> Tree c
updateValue child f (Tree t) =
    Tree { t | payload = Dict.update (Id.opIdToString child) (Maybe.map f) t.payload }


{-| Map `f` over every node's payload. Used to recurse tombstone-compaction into nested
sequences; the move-set (which encodes structure/order) is left untouched.
-}
mapPayloads : (c -> c) -> Tree c -> Tree c
mapPayloads f (Tree t) =
    Tree { t | payload = Dict.map (\_ v -> f v) t.payload }


{-| Delete a node by id (delete-wins; idempotent). Its subtree is dropped at read.
-}
delete : OpId -> Tree c -> Tree c
delete child (Tree t) =
    Tree { t | tombstones = Set.insert (Id.opIdToString child) t.tombstones }



-- READ -----------------------------------------------------------------------


{-| The resolved parent/position of every node named by a move, after folding all
moves in `moveOp` order and skipping cycle-forming ones. Liveness is _not_ applied
here — tombstoned nodes still appear; readers filter them. Key = child nodeId string.
-}
type alias Resolved =
    Dict String { child : OpId, parent : Maybe OpId, pos : Frac }


resolve : Tree c -> Resolved
resolve (Tree t) =
    t.moveSet
        |> Dict.values
        |> List.sortWith (\x y -> Id.compareOpId x.moveOp y.moveOp)
        |> List.foldl applyMove Dict.empty


{-| Apply one move to the resolved map, unless it would make `child` its own
ancestor (a cycle). A skipped move leaves the node at its current parent.
-}
applyMove : Move -> Resolved -> Resolved
applyMove m resolved =
    if wouldCycle m.child m.parent resolved then
        resolved

    else
        Dict.insert (Id.opIdToString m.child) { child = m.child, parent = m.parent, pos = m.pos } resolved


{-| Would placing `child` under `newParent` create a cycle? True iff `newParent`
is `child` itself or a descendant of `child` in the current resolved map.
-}
wouldCycle : OpId -> Maybe OpId -> Resolved -> Bool
wouldCycle child newParent resolved =
    let
        childKey =
            Id.opIdToString child

        -- walk up from `newParent`; if we reach `child`, it's a cycle
        climb : Maybe OpId -> Set String -> Bool
        climb cur seen =
            case cur of
                Nothing ->
                    False

                Just p ->
                    let
                        pk =
                            Id.opIdToString p
                    in
                    if pk == childKey then
                        True

                    else if Set.member pk seen then
                        -- defensive: never loop forever even on malformed input
                        False

                    else
                        climb (Dict.get pk resolved |> Maybe.andThen .parent) (Set.insert pk seen)
    in
    climb newParent Set.empty


{-| Whether a node itself is un-tombstoned. Callers pair this with the other
admission tests (a resolved position, a present payload). A node under a
tombstoned _ancestor_ is dropped by the read, not here.
-}
isLive : Tree c -> String -> Bool
isLive (Tree t) key =
    not (Set.member key t.tombstones)


{-| The top-level nodes (parent = `Nothing`), in sibling order.
-}
roots : Tree c -> List OpId
roots tree =
    childIdsOf Nothing (resolve tree) tree


{-| The children of a node, in sibling order (by `pos`, then child `OpId`).
-}
childrenOf : OpId -> Tree c -> List OpId
childrenOf parent tree =
    childIdsOf (Just parent) (resolve tree) tree


{-| The highest sibling **position** among the live children of `parent` (`Nothing` =
roots), or `Nothing` if there are none. Resolves the move-set **once** (versus
`childrenOf` + `siblingPos`, which resolves twice) — for the append hot path, where a
new child just needs a fractional position after the current last sibling. Only the max
`pos` is needed (the read-time `(pos, id)` tiebreak doesn't affect where to append).
-}
lastChildPos : Maybe OpId -> Tree c -> Maybe Frac
lastChildPos parent ((Tree t) as tree) =
    let
        parentKey =
            Maybe.map Id.opIdToString parent
    in
    Dict.foldl
        (\childKey r acc ->
            if
                (Maybe.map Id.opIdToString r.parent == parentKey)
                    && isLive tree childKey
                    && Dict.member childKey t.payload
            then
                case acc of
                    Just best ->
                        if Frac.compare r.pos best == GT then
                            Just r.pos

                        else
                            acc

                    Nothing ->
                        Just r.pos

            else
                acc
        )
        Nothing
        (resolve tree)


{-| Live child ids under `parent` (Nothing = roots), sorted by (pos, childId), with
tombstoned nodes removed.
-}
childIdsOf : Maybe OpId -> Resolved -> Tree c -> List OpId
childIdsOf parent resolved ((Tree t) as tree) =
    let
        parentKey =
            Maybe.map Id.opIdToString parent
    in
    Dict.toList resolved
        |> List.filter
            (\( childKey, r ) ->
                (Maybe.map Id.opIdToString r.parent == parentKey)
                    && isLive tree childKey
                    && not (ancestorTombstoned r.parent resolved tree)
            )
        |> List.filterMap
            (\( childKey, r ) ->
                Dict.get childKey t.payload
                    |> Maybe.map (\_ -> ( childKey, r ))
            )
        |> List.sortWith (\( ak, a ) ( bk, b ) -> compareSibling ( ak, a.pos ) ( bk, b.pos ))
        |> List.map (\( _, r ) -> r.child)


{-| Is any ancestor of `parent` tombstoned? (So a live node under a deleted subtree
is not surfaced as a root.) `parent = Nothing` is the true root — never tombstoned.
-}
ancestorTombstoned : Maybe OpId -> Resolved -> Tree c -> Bool
ancestorTombstoned parent resolved (Tree t) =
    case parent of
        Nothing ->
            False

        Just p ->
            let
                pk =
                    Id.opIdToString p
            in
            if Set.member pk t.tombstones then
                True

            else
                ancestorTombstoned (Dict.get pk resolved |> Maybe.andThen .parent) resolved (Tree t)


{-| Order siblings by fractional position, tiebreaking by child id string so
concurrent same-gap inserts converge.
-}
compareSibling : ( String, Frac ) -> ( String, Frac ) -> Order
compareSibling ( ak, ap ) ( bk, bp ) =
    case Frac.compare ap bp of
        EQ ->
            Basics.compare ak bk

        other ->
            other


{-| A node's content by id (`Nothing` if deleted/absent).
-}
get : OpId -> Tree c -> Maybe c
get child (Tree t) =
    if Set.member (Id.opIdToString child) t.tombstones then
        Nothing

    else
        Dict.get (Id.opIdToString child) t.payload


{-| A node's current resolved parent (`Nothing` if it's a root or unknown).
-}
parentOf : OpId -> Tree c -> Maybe OpId
parentOf child tree =
    resolve tree
        |> Dict.get (Id.opIdToString child)
        |> Maybe.andThen .parent


{-| A node's current sibling position (for allocating a new key relative to it).
-}
siblingPos : OpId -> Tree c -> Maybe Frac
siblingPos child tree =
    resolve tree
        |> Dict.get (Id.opIdToString child)
        |> Maybe.map .pos


{-| Largest Lamport counter anywhere (move ops, child ids, nested content), for
clock catch-up after merge.
-}
maxCounter : (c -> Int) -> Tree c -> Int
maxCounter contentMax (Tree t) =
    let
        moveMax =
            Dict.foldl
                (\_ m acc -> max acc (max (Id.opIdCounter m.moveOp) (Id.opIdCounter m.child)))
                0
                t.moveSet

        payloadMax =
            Dict.foldl (\_ c acc -> max acc (contentMax c)) 0 t.payload
    in
    max moveMax payloadMax


{-| Rebuild the tree as a **copy with entirely fresh node ids and move ids**, preserving
structure and sibling order, re-stamping each node's payload with `reStampPayload`.
Returns the `oldId → newId` map alongside, so anchors into the original can be re-pointed
(see `Crdt.Node.reStampWithMap`).

The walk is **pre-order — each node before its children — and that is load-bearing**: a
child's move names its parent, so the parent's re-minted id has to exist before the child
is recorded, or the copy's move-set would point at nothing and the subtree would be
unreachable. Each node keeps its source sibling position, or the midpoint if it had none.

Only live, readable nodes are copied, and tombstones are not carried over: a fresh id has
no history for an in-flight op to reference.

Takes the payload re-stamper as an argument for the same reason `maxCounter` and
`mapPayloads` do — this module is content-polymorphic and must not know about `Crdt.Node`.

-}
reStamp :
    (Id.Ctx -> c -> ( c, Id.Ctx, Dict String OpId ))
    -> Id.Ctx
    -> Tree c
    -> ( Tree c, Id.Ctx, Dict String OpId )
reStamp reStampPayload ctx tree =
    let
        step : Maybe OpId -> OpId -> ( Tree c, Id.Ctx, Dict String OpId ) -> ( Tree c, Id.Ctx, Dict String OpId )
        step newParent oldId ( acc, c, remap ) =
            case get oldId tree of
                Nothing ->
                    ( acc, c, remap )

                Just payload ->
                    let
                        ( content, c1, payloadRemap ) =
                            reStampPayload c payload

                        ( newId, c2 ) =
                            Id.nextId c1

                        ( moveId, c3 ) =
                            Id.nextId c2

                        pos =
                            siblingPos oldId tree
                                |> Maybe.withDefault (Frac.between Nothing Nothing)
                    in
                    -- disjoint tables (an old id is unique document-wide), so union order
                    -- here is immaterial
                    List.foldl (step (Just newId))
                        ( move moveId newId newParent pos content acc
                        , c3
                        , Dict.union payloadRemap (Dict.insert (Id.opIdToString oldId) newId remap)
                        )
                        (childrenOf oldId tree)
    in
    List.foldl (step Nothing) ( empty, ctx, Dict.empty ) (roots tree)



-- READ SHAPE -----------------------------------------------------------------


{-| A tree read as a nested value: an ordered list of top-level items, each with
its children. This is what `Crdt.tree` decodes to.
-}
type alias Forest a =
    List (Item a)


{-| One node in a read tree: its stable id (the `OpId` refs address it by), its
decoded payload, and its ordered children. A `type` (not an alias) because it is
mutually recursive with `Forest`; read its fields with `itemId`/`itemValue`/
`itemChildren`.
-}
type Item a
    = Item OpId a (Forest a)


{-| The stable id of a read item (refs address a node by this).
-}
itemId : Item a -> OpId
itemId (Item id _ _) =
    id


{-| The decoded payload of a read item.
-}
itemValue : Item a -> a
itemValue (Item _ v _) =
    v


{-| The ordered children of a read item.
-}
itemChildren : Item a -> Forest a
itemChildren (Item _ _ cs) =
    cs


{-| Materialize the tree as a `Forest`, mapping each live node's payload with `f`
(the schema's decoder). Children are in sibling order; deleted subtrees are
dropped. `f` returning `Nothing` drops that node (a decode error).

Single-pass: `resolve` the move-set once, then bucket every node under its parent key
(sorting each sibling group once) into a children-index, and walk that index top-down
from the roots. The recursion only descends through live nodes, so a tombstoned node
is dropped and its subtree is never visited — which is exactly the "no ancestor
tombstoned" rule, without a per-node ancestor climb. This is O(N log N) overall, versus
the O(N² log N) of resolving + scanning the whole node set once per node.

-}
toForest : (c -> Maybe a) -> Tree c -> Forest a
toForest f ((Tree t) as tree) =
    let
        resolved =
            resolve tree

        -- parentKey -> that parent's children, already in sibling order. Only nodes
        -- that are live and have a payload are bucketed (the same admission test
        -- `childIdsOf` applies); `""` is the key for roots (parent = Nothing).
        rootKey =
            ""

        childIndex : Dict String (List { child : OpId, pos : Frac })
        childIndex =
            Dict.foldl
                (\childKey r acc ->
                    if isLive tree childKey && Dict.member childKey t.payload then
                        let
                            pKey =
                                Maybe.map Id.opIdToString r.parent |> Maybe.withDefault rootKey
                        in
                        Dict.update pKey
                            (\existing ->
                                Just ({ child = r.child, pos = r.pos } :: Maybe.withDefault [] existing)
                            )
                            acc

                    else
                        acc
                )
                Dict.empty
                resolved

        childrenSorted : String -> List OpId
        childrenSorted pKey =
            Dict.get pKey childIndex
                |> Maybe.withDefault []
                |> List.sortWith
                    (\a b ->
                        compareSibling ( Id.opIdToString a.child, a.pos ) ( Id.opIdToString b.child, b.pos )
                    )
                |> List.map .child

        walk : OpId -> Maybe (Item a)
        walk id =
            Dict.get (Id.opIdToString id) t.payload
                |> Maybe.andThen f
                |> Maybe.map
                    (\v -> Item id v (childrenSorted (Id.opIdToString id) |> List.filterMap walk))
    in
    childrenSorted rootKey |> List.filterMap walk
