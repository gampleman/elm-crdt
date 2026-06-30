module Crdt.Rga exposing
    ( Rga, Element
    , empty, element, fromElements, elements, put
    , insertAfter, delete, merge
    , toList, toElementsInOrder, idAtVisibleIndex, originForVisibleIndex, lastVisibleId
    , visibleIds
    , get, updateElement
    , maxCounter
    )

{-| A Replicated Growable Array — the sequence CRDT backing both lists and text.

Each element carries a globally-unique `OpId`, the id of the element it was
inserted _after_ (`origin`, `Nothing` = head), its `content` (an opaque payload
— a `Node` in practice, kept polymorphic here as `c`), and a `deleted`
tombstone flag.

The crucial property: the visible order is a **pure function of the element set**
(ids + origins), computed in `toList`, never mutated incrementally. That is what
makes ordering independent of merge order. Tombstones are retained forever (in
v1) so that state-based merge stays idempotent — dropping a tombstone would
resurrect deleted elements on a later merge.

The store is keyed by a string form of the `OpId` so that Elm structural
equality (`==`) is a sound convergence oracle.

@docs Rga, Element
@docs empty, element, fromElements, elements, put
@docs insertAfter, delete, merge
@docs toList, toElementsInOrder, idAtVisibleIndex, originForVisibleIndex, lastVisibleId
@docs visibleIds
@docs get, updateElement
@docs maxCounter

-}

import Crdt.Id as Id exposing (OpId)
import Dict exposing (Dict)
import Set exposing (Set)


{-| One element of the array.
-}
type alias Element c =
    { id : OpId
    , origin : Maybe OpId
    , content : c
    , deleted : Bool
    }


{-| The array: a map from the string form of each element's id to the element.
Order is not stored; it is derived in `toList`.
-}
type Rga c
    = Rga (Dict String (Element c))


{-| Construct an element record.
-}
element : OpId -> Maybe OpId -> c -> Bool -> Element c
element id origin content deleted =
    { id = id, origin = origin, content = content, deleted = deleted }


{-| The empty array.
-}
empty : Rga c
empty =
    Rga Dict.empty


{-| Build from a list of elements (later duplicates win by merge rule).
-}
fromElements : List (Element c) -> Rga c
fromElements =
    List.foldl (\el (Rga d) -> Rga (insertElement el d)) empty


{-| All elements, unordered (for serialization).
-}
elements : Rga c -> List (Element c)
elements (Rga d) =
    Dict.values d


insertElement : Element c -> Dict String (Element c) -> Dict String (Element c)
insertElement el d =
    Dict.insert (Id.opIdToString el.id) el d


{-| Insert a single element directly (O(log n)), without rebuilding the array.
Used by the op-log fold so applying an insert op is not O(n). Order is still
derived in `toList`, so where the element ends up is governed by its id/origin,
not by insertion time.
-}
put : Element c -> Rga c -> Rga c
put el (Rga d) =
    Rga (insertElement el d)



-- EDITS ----------------------------------------------------------------------


{-| Insert `content` immediately after `origin` (or at the head when `Nothing`),
using a freshly minted id. Returns the updated array and advanced context.
-}
insertAfter : Id.Ctx -> Maybe OpId -> c -> Rga c -> ( Rga c, Id.Ctx )
insertAfter ctx origin content (Rga d) =
    let
        ( id, ctx1 ) =
            Id.nextId ctx
    in
    ( Rga (insertElement (element id origin content False) d), ctx1 )


{-| Tombstone the element with the given id. Idempotent; unknown ids are no-ops.
-}
delete : OpId -> Rga c -> Rga c
delete id (Rga d) =
    Rga (Dict.update (Id.opIdToString id) (Maybe.map (\el -> { el | deleted = True })) d)



-- MERGE ----------------------------------------------------------------------


{-| Merge two arrays: union of elements by id; on a shared id, a deletion on
either side wins (tombstone OR) and the element **contents are merged
recursively** with `mergeContent` (so nested containers inside a list element —
e.g. a record sitting in a list — converge). The operation is commutative,
associative and idempotent as long as `mergeContent` is.

In normal use an id is minted exactly once, so the two copies of a shared id are
identical. But decoded (possibly corrupt or adversarial) input could disagree on
an element's `origin`; to keep `merge` commutative on _any_ input we resolve a
conflicting origin deterministically rather than keeping whichever side happened
to be the left argument.

-}
merge : (c -> c -> c) -> Rga c -> Rga c -> Rga c
merge mergeContent (Rga a) (Rga b) =
    Rga
        (Dict.merge
            Dict.insert
            (\k ea eb -> Dict.insert k (mergeElement mergeContent ea eb))
            Dict.insert
            a
            b
            Dict.empty
        )


mergeElement : (c -> c -> c) -> Element c -> Element c -> Element c
mergeElement mergeContent a b =
    { id = a.id
    , origin = mergeOrigin a.origin b.origin
    , deleted = a.deleted || b.deleted
    , content = mergeContent a.content b.content
    }


{-| Deterministically combine two (possibly conflicting) origins so merge is
order-independent: prefer the larger `OpId`, treating `Nothing` (head) as
smallest. Identical origins — the normal case — pass through unchanged.
-}
mergeOrigin : Maybe OpId -> Maybe OpId -> Maybe OpId
mergeOrigin a b =
    case ( a, b ) of
        ( Just x, Just y ) ->
            if Id.compareOpId x y == LT then
                b

            else
                a

        ( Just _, Nothing ) ->
            a

        ( Nothing, _ ) ->
            b



-- ORDERING -------------------------------------------------------------------


{-| The visible content, in deterministic order, with tombstones dropped.
-}
toList : Rga c -> List c
toList rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.map .content


{-| All elements (including tombstones) in the deterministic RGA order. This is
the single source of truth for ordering.

The order is a stable walk of the insertion forest: starting from the head
(elements with no origin), each element is followed by its children (elements
whose origin is this element's id). Siblings sharing the same origin are ordered
by `compareOpId` **descending**, so a later concurrent insert at the same anchor
sorts before an earlier one — the standard RGA rule that guarantees convergence.

-}
toElementsInOrder : Rga c -> List (Element c)
toElementsInOrder (Rga d) =
    let
        all =
            Dict.values d

        -- an element is a *root* if it has no origin, or if its origin points to
        -- an id that isn't present (a dangling reference — never drop it). Such
        -- elements hang off the head so they always appear in the output.
        isRoot : Element c -> Bool
        isRoot el =
            case el.origin of
                Nothing ->
                    True

                Just o ->
                    not (Dict.member (Id.opIdToString o) d)

        -- children grouped by their origin's string key; roots collected under
        -- the head key regardless of where their (dangling) origin pointed.
        childrenOf : Dict String (List (Element c))
        childrenOf =
            List.foldl
                (\el acc ->
                    let
                        key =
                            if isRoot el then
                                headKey

                            else
                                originKey el.origin
                    in
                    Dict.update key
                        (\existing -> Just (el :: Maybe.withDefault [] existing))
                        acc
                )
                Dict.empty
                all

        -- siblings ordered by id descending (later concurrent insert first)
        sortedChildren : String -> List (Element c)
        sortedChildren key =
            Dict.get key childrenOf
                |> Maybe.withDefault []
                |> List.sortWith (\x y -> Id.compareOpId y.id x.id)

        -- Pre-order DFS of the insertion forest via an explicit work-stack, so it
        -- is tail-recursive (stack-safe for long origin-chains — a list built by
        -- appending is a chain of depth N) and O(N) overall. The visited set
        -- guards against origin cycles in adversarial/corrupt input. Children are
        -- prepended to the stack so they are emitted before later siblings, in
        -- `sortedChildren` order; `acc` is built reversed and flipped once.
        loop : List (Element c) -> Set String -> List (Element c) -> ( List (Element c), Set String )
        loop stack visited acc =
            case stack of
                [] ->
                    ( acc, visited )

                el :: rest ->
                    let
                        key =
                            Id.opIdToString el.id
                    in
                    if Set.member key visited then
                        loop rest visited acc

                    else
                        loop (sortedChildren key ++ rest) (Set.insert key visited) (el :: acc)

        ( ordered, seen ) =
            loop (sortedChildren headKey) Set.empty []

        -- Concurrent moves can form an origin *cycle* (A after B, B after A), whose
        -- members are unreachable from the head. Never drop them: take the
        -- lowest-id unvisited element as an extra root and keep walking, until all
        -- elements appear. Deterministic (id order) so it still converges.
        sweep : List (Element c) -> Set String -> List (Element c)
        sweep acc visited =
            case
                all
                    |> List.filter (\el -> not (Set.member (Id.opIdToString el.id) visited))
                    |> List.sortWith (\x y -> Id.compareOpId x.id y.id)
            of
                [] ->
                    acc

                root :: _ ->
                    let
                        ( acc1, visited1 ) =
                            loop [ root ] visited acc
                    in
                    sweep acc1 visited1
    in
    List.reverse (sweep ordered seen)


headKey : String
headKey =
    ""


originKey : Maybe OpId -> String
originKey origin =
    case origin of
        Nothing ->
            headKey

        Just id ->
            Id.opIdToString id



-- VISIBLE-INDEX HELPERS (for the Edit layer) ---------------------------------


{-| The id of the element at a given _visible_ index (tombstones skipped).
-}
idAtVisibleIndex : Int -> Rga c -> Maybe OpId
idAtVisibleIndex i rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.drop i
        |> List.head
        |> Maybe.map .id


{-| The origin to use when inserting _at_ a given visible index — i.e. the id of
the visible element just before it (`Nothing` for index 0 / the head).
-}
originForVisibleIndex : Int -> Rga c -> Maybe OpId
originForVisibleIndex i rga =
    if i <= 0 then
        Nothing

    else
        idAtVisibleIndex (i - 1) rga


{-| The id of the last visible element, for appends.
-}
lastVisibleId : Rga c -> Maybe OpId
lastVisibleId rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.reverse
        |> List.head
        |> Maybe.map .id


{-| The ids of all visible (non-tombstoned) elements, in order. Computes the
ordering **once** — callers that need several visible indices (e.g. a text-diff
delete range) should use this instead of repeated `idAtVisibleIndex`, which
re-orders the whole array each call (turning an edit O(D·N)).
-}
visibleIds : Rga c -> List OpId
visibleIds rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.map .id


{-| Look up an element by id.
-}
get : OpId -> Rga c -> Maybe (Element c)
get id (Rga d) =
    Dict.get (Id.opIdToString id) d


{-| Update the element at a visible index by transforming its content. No-op if
the index is out of range.
-}
updateElement : OpId -> (c -> c) -> Rga c -> Rga c
updateElement id f (Rga d) =
    Rga (Dict.update (Id.opIdToString id) (Maybe.map (\el -> { el | content = f el.content })) d)



-- MISC -----------------------------------------------------------------------


{-| Largest Lamport counter referenced anywhere in the array (for clock catch-up
on merge). Considers both element ids and their origins.
-}
maxCounter : Rga c -> Int
maxCounter (Rga d) =
    Dict.foldl
        (\_ el acc ->
            let
                originCounter =
                    el.origin |> Maybe.map Id.opIdCounter |> Maybe.withDefault 0
            in
            max acc (max (Id.opIdCounter el.id) originCounter)
        )
        0
        d
