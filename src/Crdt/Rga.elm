module Crdt.Rga exposing
    ( Rga, Element, Side(..)
    , empty, element, fromElements, elements, put
    , insertAfter, delete, merge
    , toList, toElementsInOrder, idAtVisibleIndex, lastVisibleId, compactTombstones
    , visibleIds, liveCountThrough
    , get, updateElement
    , maxCounter
    )

{-| A replicated growable sequence — the CRDT backing both lists and text.

Each element carries a globally-unique `OpId`, a **`parent` anchor** (the id of
another element it hangs off, `Nothing` = a root at the head), a **`side`** saying
whether it sits to the `Left` or `Right` of that parent, its `content` (an opaque
payload — a `Node` in practice, kept polymorphic here as `c`), and a `deleted`
tombstone flag.

Ordering is **Fugue** (Weidner et al., 2023), not classic RGA. The `parent`+`side`
pair makes each element a node in a tree: left-children render before their parent,
right-children after, and concurrent siblings on the same side are ordered by id
(descending). The payoff over plain RGA (a single left anchor) is that two
_concurrent runs_ inserted at the same spot stay **contiguous blocks** instead of
interleaving character-by-character — each run is a right-spine subtree, so the
whole subtree renders before the other. See `docs/09-fugue.md`.

(`origin = Just L` in old RGA is exactly `parent = L, side = Right`; the new
expressible case is `side = Left`, "immediately before parent", which is what keeps
runs from interleaving. Roots always have `side = Right`.)

The crucial invariant is unchanged: the visible order is a **pure function of the
element set** (ids + parent + side), computed in `toElementsInOrder`, never mutated
incrementally — that is what makes ordering independent of merge order. Tombstones
are retained forever (in v1) so state-based merge stays idempotent — dropping a
tombstone would resurrect deleted elements on a later merge.

The store is keyed by a string form of the `OpId` so that Elm structural
equality (`==`) is a sound convergence oracle.

@docs Rga, Element, Side
@docs empty, element, fromElements, elements, put
@docs insertAfter, delete, merge
@docs toList, toElementsInOrder, idAtVisibleIndex, lastVisibleId, compactTombstones
@docs visibleIds, liveCountThrough
@docs get, updateElement
@docs maxCounter

-}

import Crdt.Id.Internal as Id exposing (OpId)
import Dict exposing (Dict)
import Set exposing (Set)


{-| Which side of its `parent` an element sits on. `Left` = renders immediately
before the parent (and its earlier left-siblings); `Right` = immediately after.
-}
type Side
    = Left
    | Right


{-| One element of the sequence: a Fugue tree node.
-}
type alias Element c =
    { id : OpId
    , parent : Maybe OpId
    , side : Side
    , content : c
    , deleted : Bool
    }


{-| The sequence: a map from the string form of each element's id to the element.
Order is not stored; it is derived in `toElementsInOrder`.
-}
type Rga c
    = Rga (Dict String (Element c))


{-| Construct an element record.
-}
element : OpId -> Maybe OpId -> Side -> c -> Bool -> Element c
element id parent side content deleted =
    { id = id, parent = parent, side = side, content = content, deleted = deleted }


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

"After `origin`" is expressed as a **right-child of `origin`** (`side = Right`); at
the head it is a root (`parent = Nothing, side = Right`). This is the classic-RGA
anchoring rule, used by the `Crdt.Text` char layer and `Node.reStamp`. The op-log
text layer (`Crdt.Doc.Internal.applyTextDiff`) chooses `parent`/`side` itself to get
Fugue's non-interleaving runs.

-}
insertAfter : Id.Ctx -> Maybe OpId -> c -> Rga c -> ( Rga c, Id.Ctx )
insertAfter ctx origin content (Rga d) =
    let
        ( id, ctx1 ) =
            Id.nextId ctx
    in
    ( Rga (insertElement (element id origin Right content False) d), ctx1 )


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
an element's `parent`/`side`; to keep `merge` commutative on _any_ input we resolve
a conflicting anchor deterministically (see `mergeAnchor`) rather than keeping
whichever side happened to be the left argument.

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
    let
        ( parent, side ) =
            mergeAnchor ( a.parent, a.side ) ( b.parent, b.side )
    in
    { id = a.id
    , parent = parent
    , side = side
    , deleted = a.deleted || b.deleted
    , content = mergeContent a.content b.content
    }


{-| Deterministically combine two (possibly conflicting) `(parent, side)` anchors so
merge is order-independent. In normal use an id is minted exactly once, so both
copies of a shared id carry an identical anchor and this passes through unchanged;
the resolver only matters for decoded (possibly corrupt/adversarial) input that
disagrees. Fixed rule: prefer the larger parent `OpId` (treating `Nothing` = head as
smallest); on an equal parent, prefer `Right` over `Left`.
-}
mergeAnchor : ( Maybe OpId, Side ) -> ( Maybe OpId, Side ) -> ( Maybe OpId, Side )
mergeAnchor ( pa, sa ) ( pb, sb ) =
    case ( pa, pb ) of
        ( Just x, Just y ) ->
            case Id.compareOpId x y of
                LT ->
                    ( pb, sb )

                GT ->
                    ( pa, sa )

                EQ ->
                    ( pa, mergeSide sa sb )

        ( Just _, Nothing ) ->
            ( pa, sa )

        ( Nothing, Just _ ) ->
            ( pb, sb )

        ( Nothing, Nothing ) ->
            ( Nothing, mergeSide sa sb )


mergeSide : Side -> Side -> Side
mergeSide a b =
    case ( a, b ) of
        ( Right, _ ) ->
            Right

        ( _, Right ) ->
            Right

        _ ->
            Left



-- ORDERING -------------------------------------------------------------------


{-| The visible content, in deterministic order, with tombstones dropped.
-}
toList : Rga c -> List c
toList rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.map .content


{-| **Physically drop every tombstone**, re-anchoring the surviving live elements as a
single **right-spine** — each becomes the right-child of the previous survivor, the first
a head root. A right-spine is a one-child-per-node chain, so its in-order traversal is
exactly the chain order: the visible order is preserved verbatim, and every element keeps
its `id` (so cursors/targets anchored to a survivor still resolve). All `parent` pointers
now reference another survivor, so nothing dangles. Each survivor's `content` is passed
through `compactContent`, so nested sequences inside a live element compact too.

This is **not** merge-safe on its own: an incoming op that anchors after a dropped
tombstone would dangle (Fugue treats a missing parent as a head root, so it would jump to
the front). Only sound below a **stable** cut every replica has already incorporated — the
same envelope as `compact`/`gc` discarding ops (see `docs/04-gc.md`). The caller owns that;
this function just performs the rewrite.

-}
compactTombstones : (c -> c) -> Rga c -> Rga c
compactTombstones compactContent rga =
    let
        live =
            toElementsInOrder rga |> List.filter (not << .deleted)

        -- chain each live element after the previous survivor (first = head root),
        -- keeping id; content recurses; all become Right-children so the spine reads
        -- in visible order.
        chained =
            List.foldl
                (\el ( prev, acc ) ->
                    ( Just el.id
                    , { el | parent = prev, side = Right, content = compactContent el.content } :: acc
                    )
                )
                ( Nothing, [] )
                live
                |> Tuple.second
    in
    fromElements chained


{-| One item of the explicit traversal stack: either **expand** an element's
subtree (push its children + a self-emit) or **emit** the element itself. Making
the emit a distinct item is what lets a single flat loop do an _in-order_ walk
(left-subtree, node, right-subtree) without recursion, so it is stack-safe.
-}
type Work c
    = Expand (Element c)
    | Emit (Element c)


{-| All elements (including tombstones) in the deterministic order. This is the
single source of truth for ordering.

The order is the **Fugue** in-order traversal of the `parent`/`side` tree. Every
element is a child of its `parent` on its `side`; the virtual head (`headKey`) is
the parent of all roots (elements with `parent = Nothing`, or a dangling `parent`
pointing at a missing id — never dropped). For each node we emit its **left**
children (and their subtrees), then the node, then its **right** children. Within a
side, concurrent siblings are ordered by `compareOpId`: right-children **descending**
(a later concurrent insert sorts nearest the parent — the classic RGA rule, so an
all-right tree orders exactly as RGA did), left-children **ascending** (symmetric:
the newest left insert sits nearest the parent, just before it).

Because each typed run chains as a right-spine (char₂ is a right-child of char₁),
an inserted run is one contiguous subtree, so a _concurrent_ run at the same anchor
renders entirely before or after it — never interleaved. That is the whole point of
Fugue over RGA. See `docs/09-fugue.md`.

-}
toElementsInOrder : Rga c -> List (Element c)
toElementsInOrder (Rga d) =
    let
        all =
            Dict.values d

        -- the key of an element's effective parent: the head for roots and for
        -- dangling parents (a `parent` pointing at a missing id — kept as a root
        -- so nothing ever silently vanishes).
        childKey : Element c -> String
        childKey el =
            case el.parent of
                Nothing ->
                    headKey

                Just p ->
                    if Dict.member (Id.opIdToString p) d then
                        Id.opIdToString p

                    else
                        headKey

        -- all children grouped by their effective parent key (unsorted; split by
        -- side and sorted per-key at expand time, so each key sorts once).
        grouped : Dict String (List (Element c))
        grouped =
            List.foldl
                (\el acc ->
                    Dict.update (childKey el)
                        (\existing -> Just (el :: Maybe.withDefault [] existing))
                        acc
                )
                Dict.empty
                all

        leftChildren : String -> List (Element c)
        leftChildren k =
            Dict.get k grouped
                |> Maybe.withDefault []
                |> List.filter (\el -> el.side == Left)
                -- ascending: newest (highest id) renders last, nearest the parent
                |> List.sortWith (\x y -> Id.compareOpId x.id y.id)

        rightChildren : String -> List (Element c)
        rightChildren k =
            Dict.get k grouped
                |> Maybe.withDefault []
                |> List.filter (\el -> el.side == Right)
                -- descending: newest (highest id) renders first, nearest the parent
                |> List.sortWith (\x y -> Id.compareOpId y.id x.id)

        -- expand node `k` into stack work, front-first: left subtrees, then the
        -- node itself (when real — the head passes `Nothing`), then right subtrees.
        expandKey : String -> Maybe (Element c) -> List (Work c) -> List (Work c)
        expandKey k maybeSelf rest =
            List.map Expand (leftChildren k)
                ++ (case maybeSelf of
                        Just el ->
                            Emit el :: List.map Expand (rightChildren k)

                        Nothing ->
                            List.map Expand (rightChildren k)
                   )
                ++ rest

        -- Flat in-order walk via the explicit work-stack (stack-safe for long
        -- right-spines — a typed run is a chain of depth N). `visited` guards
        -- against parent cycles in adversarial/corrupt input; marking at expand
        -- time means each node is emitted at most once. `acc` is built reversed.
        loop : List (Work c) -> Set String -> List (Element c) -> ( List (Element c), Set String )
        loop stack visited acc =
            case stack of
                [] ->
                    ( acc, visited )

                (Emit el) :: rest ->
                    loop rest visited (el :: acc)

                (Expand el) :: rest ->
                    let
                        key =
                            Id.opIdToString el.id
                    in
                    if Set.member key visited then
                        loop rest visited acc

                    else
                        loop (expandKey key (Just el) rest) (Set.insert key visited) acc

        ( ordered, seen ) =
            loop (expandKey headKey Nothing []) Set.empty []

        -- A parent *cycle* (A child-of B, B child-of A) leaves its members
        -- unreachable from the head. Never drop them: take the lowest-id unvisited
        -- element as an extra root and keep walking, until all elements appear.
        -- Deterministic (id order) so it still converges.
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
                            loop [ Expand root ] visited acc
                    in
                    sweep acc1 visited1
    in
    List.reverse (sweep ordered seen)


headKey : String
headKey =
    ""



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


{-| The count of **live** (non-tombstoned) elements at or before `anchor` in RGA
order. This is the basis for stable cursors: a caret anchored _after_ element
`anchor` sits at this visible offset, and it stays correct as other replicas
insert/delete elsewhere.

Robust across deletion of the anchor itself: ordering uses `toElementsInOrder`,
which retains tombstones, so a deleted anchor still has an order position and we
count the live elements up to it — the caret lands at the nearest surviving spot.
If `anchor` isn't present at all (e.g. its op hasn't arrived yet), returns the
count of all live elements (caret at the end), which converges once it arrives.

-}
liveCountThrough : OpId -> Rga c -> Int
liveCountThrough anchor rga =
    let
        anchorKey =
            Id.opIdToString anchor

        step el ( count, stop ) =
            if stop then
                ( count, stop )

            else if Id.opIdToString el.id == anchorKey then
                -- include the anchor itself if it's live, then stop
                ( count
                    + (if el.deleted then
                        0

                       else
                        1
                      )
                , True
                )

            else
                ( count
                    + (if el.deleted then
                        0

                       else
                        1
                      )
                , False
                )
    in
    toElementsInOrder rga
        |> List.foldl step ( 0, False )
        |> Tuple.first


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
on merge). Considers both element ids and their parent anchors.
-}
maxCounter : Rga c -> Int
maxCounter (Rga d) =
    Dict.foldl
        (\_ el acc ->
            let
                parentCounter =
                    el.parent |> Maybe.map Id.opIdCounter |> Maybe.withDefault 0
            in
            max acc (max (Id.opIdCounter el.id) parentCounter)
        )
        0
        d
