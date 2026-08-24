module Crdt.Rga exposing
    ( Rga, Element, Side(..)
    , empty, element, fromElements, elements, put, putRight
    , insertAfter, delete
    , toList, toElementsInOrder, idAtVisibleIndex, lastVisibleId, compactTombstones, reStamp
    , visibleIds, liveCountThroughWith
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
whole subtree renders before the other. See `design-docs/09-fugue.md`.

(`origin = Just L` in old RGA is exactly `parent = L, side = Right`; the new
expressible case is `side = Left`, "immediately before parent", which is what keeps
runs from interleaving. Roots always have `side = Right`.)

The crucial invariant is unchanged: the visible order is a **pure function of the
element set** (ids + parent + side), computed in `toElementsInOrder`, never mutated
incrementally — that is what makes ordering independent of merge order.

**Tombstones.** A deleted element keeps its `deleted` flag rather than leaving the store,
because ordering is derived from the whole element set: a live element may anchor its
`parent` to a dead one, and an op still in flight may anchor after it. So a tombstone is
load-bearing for as long as any op could reference it. They are _not_ retained forever,
though — `compactTombstones` physically drops them, rebuilding the survivors as a
right-spine (same visible order, same ids, so cursors still resolve). That is only sound
**below a stable cut** every replica has already incorporated, which is why the only caller
is `Crdt.Doc.compact`, and only once the whole op log has folded into the base. See
`design-docs/04-gc.md`.

The store is keyed by a string form of the `OpId` so that Elm structural
equality (`==`) is a sound convergence oracle.

@docs Rga, Element, Side
@docs empty, element, fromElements, elements, put, putRight
@docs insertAfter, delete
@docs toList, toElementsInOrder, idAtVisibleIndex, lastVisibleId, compactTombstones, reStamp
@docs visibleIds, liveCountThroughWith
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


{-| Build from a list of elements (on a repeated id the later element overwrites the
earlier one, except that `deleted` is retained once set — see `insertElement`; this is a
plain insert, not a set-union join).
-}
fromElements : List (Element c) -> Rga c
fromElements =
    List.foldl (\el (Rga d) -> Rga (insertElement el d)) empty


{-| All elements, unordered (for serialization).
-}
elements : Rga c -> List (Element c)
elements (Rga d) =
    Dict.values d


{-| Write an element into the store, **preserving an existing tombstone**: the record is
replaced wholesale, except that `deleted` is the OR of the old and new flags.

Deletion is monotone in every other part of the design (tombstones only ever accumulate;
`Crdt.MoveList` and `Crdt.Tree` keep grow-only tombstone sets), and it has to be monotone
here too, because an id is not a promise that it is written once:

  - an insert op can be **re-applied** — deliberately, when a pending op is retried after
    its anchor arrives, or incidentally on any re-delivery — and if the element was deleted
    in between, a plain `Dict.insert` would resurrect it;
  - a **hostile or corrupt** op can claim an `elemId` that an existing tombstone already
    holds. It cannot resurrect the victim's content (the element is rebuilt from the op's
    own seed, so what appears is the forger's), but without the OR it could un-delete the
    position and clobber a tombstone other elements anchor to.

So `deleted` never goes from `True` back to `False` through this door, which is what makes
`put` idempotent as a function of the op _set_ rather than of arrival order.

-}
insertElement : Element c -> Dict String (Element c) -> Dict String (Element c)
insertElement el d =
    Dict.insert (Id.opIdToString el.id)
        (case Dict.get (Id.opIdToString el.id) d of
            Just old ->
                { el | deleted = el.deleted || old.deleted }

            Nothing ->
                el
        )
        d


{-| Insert a single element directly (O(log n)), without rebuilding the array.
Used by the op-log fold so applying an insert op is not O(n). Order is still
derived in `toElementsInOrder`, so where the element ends up is governed by its
id and its `parent`/`side` anchor, not by insertion time.

Re-putting an id that is already tombstoned leaves it tombstoned (see `insertElement`),
which is what makes re-delivering or retrying an insert op a no-op.

-}
put : Element c -> Rga c -> Rga c
put el (Rga d) =
    Rga (insertElement el d)


{-| Insert a live right-child of `parent` (`Nothing` = a root at the head) — the only
two `Element` shapes a caller that never deletes and never anchors to the `Left` can
produce. Use this instead of `element`+`put` when `side = Right` and `deleted = False`
are **invariants of your structure** rather than choices: naming them once here means
they cannot be got wrong at a call site, and a reader can see from the type that there
is no other case to handle.

`Crdt.MoveList`'s position cells are the motivating caller — see the `cellRga` note on
`Crdt.MoveList.MoveList` for why cells are always live right-children, and what the
wire format does with that.

-}
putRight : OpId -> Maybe OpId -> c -> Rga c -> Rga c
putRight id parent content rga =
    put (element id parent Right content False) rga



-- EDITS ----------------------------------------------------------------------


{-| Insert `content` immediately after `origin` (or at the head when `Nothing`),
using a freshly minted id. Returns the updated array and advanced context.

"After `origin`" is expressed as a **right-child of `origin`** (`side = Right`); at
the head it is a root (`parent = Nothing, side = Right`). This is the classic-RGA
anchoring rule, used where a whole sequence is built locally in one go: the
`Crdt.Text` char layer, the schema's list/text seeding, and `Node.reStamp`. The
op-log text layer (`Crdt.Doc.Internal.applyTextDiff`) chooses `parent`/`side`
itself to get Fugue's non-interleaving runs.

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
same envelope as `Crdt.Doc.compact` discarding ops (see `design-docs/04-gc.md`). The caller
owns that; this function just performs the rewrite.

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


{-| Rebuild the sequence as a **copy with entirely fresh element ids**, in the same
visible order, re-stamping each element's content with `reStampContent`. Tombstones are
dropped: a copy has no history for an in-flight op to anchor into, which is the whole
point (`compactTombstones` keeps ids and so must stay below a stable cut; this mints new
ones and so is unconditional).

Also returns the `oldId → newId` map, because a copy is useless without it — mark anchors,
cursors and reverse-ops all name the originals and have to be re-pointed (see
`Crdt.Node.reStampWithMap`).

Takes the content re-stamper as an argument for the same reason `compactTombstones` and
`maxCounter` do: this module is content-polymorphic and must not know about `Crdt.Node`.

-}
reStamp :
    (Id.Ctx -> c -> ( c, Id.Ctx, Dict String OpId ))
    -> Id.Ctx
    -> Rga c
    -> ( Rga c, Id.Ctx, Dict String OpId )
reStamp reStampContent ctx rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.foldl
            (\old ( acc, c, ( prev, remap ) ) ->
                let
                    ( content, c1, contentRemap ) =
                        reStampContent c old.content

                    ( acc1, c2 ) =
                        insertAfter c1 prev content acc

                    newId =
                        lastVisibleId acc1

                    entry =
                        case newId of
                            Just nid ->
                                Dict.insert (Id.opIdToString old.id) nid remap

                            Nothing ->
                                remap
                in
                -- union order is immaterial: an old id is unique across the whole
                -- document, so this element's key cannot also appear in its own content's
                -- table and the two are disjoint
                ( acc1, c2, ( newId, Dict.union contentRemap entry ) )
            )
            ( empty, ctx, ( Nothing, Dict.empty ) )
        |> (\( acc, c, ( _, remap ) ) -> ( acc, c, remap ))


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
Fugue over RGA. See `design-docs/09-fugue.md`.

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


{-| The ids of all visible (non-tombstoned) elements, in order. Computes the
ordering **once** — callers that need several visible indices (e.g. a text-diff
delete range) should use this instead of repeated `idAtVisibleIndex`, which
re-orders the whole array each call (turning an edit O(D·N)).

Deliberately a `List` rather than an `Array`. The one thing an `Array` would buy is
`Array.get` in place of `List.drop i >> List.head` — and `toElementsInOrder`, which
every caller here goes through, produces a `List`, so the conversion would cost a pass
on the whole sequence to save a partial one. It would also split this from the
identically-shaped `Doc.Internal.cursorIds` (which filters to characters and so cannot
come from here), and every consumer downstream — membership, predecessor search, the
typed read models — wants a list anyway. Index lookup is not the hot path: the paths
that _are_ hot (`applyTextDiff`) exist precisely to walk this once.

-}
visibleIds : Rga c -> List OpId
visibleIds rga =
    toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.map .id


{-| The id of the element at a given _visible_ index (tombstones skipped).
-}
idAtVisibleIndex : Int -> Rga c -> Maybe OpId
idAtVisibleIndex i rga =
    visibleIds rga |> List.drop i |> List.head


{-| The id of the last visible element, for appends.
-}
lastVisibleId : Rga c -> Maybe OpId
lastVisibleId rga =
    visibleIds rga |> List.reverse |> List.head


{-| The count of live (non-tombstoned) elements that the predicate accepts, at or before
`anchor` in sequence order. This is the basis for stable cursors: a caret anchored _after_
element `anchor` sits at this visible offset, and it stays correct as other replicas
insert/delete elsewhere. For a plain sequence pass `(always True)`; rich text passes a
char-only predicate, because its sequence interleaves characters with block markers and
nest tokens while the offsets an editor speaks in count **characters only** (counting
every element instead drifts the caret by one per preceding marker).

Robust across deletion of the anchor itself: ordering uses `toElementsInOrder`,
which retains tombstones, so a deleted anchor still has an order position and we
count the live elements up to it — the caret lands at the nearest surviving spot.
If `anchor` isn't present at all (e.g. its op hasn't arrived yet), returns the
count of all counted elements (caret at the end), which converges once it arrives.

-}
liveCountThroughWith : (Element c -> Bool) -> OpId -> Rga c -> Int
liveCountThroughWith keep anchor rga =
    let
        anchorKey =
            Id.opIdToString anchor

        counts el =
            if el.deleted || not (keep el) then
                0

            else
                1

        -- A real early exit rather than a `List.foldl` carrying a `stop` flag, which
        -- walked every element past the anchor to reach a decision it had already made.
        -- It bounds the counting at the anchor, not the whole ordering: `toElementsInOrder`
        -- above is still O(N) and remains the cost that matters here.
        countTo els count =
            case els of
                [] ->
                    count

                el :: rest ->
                    if Id.opIdToString el.id == anchorKey then
                        -- include the anchor itself if it's live (and counted), then stop
                        count + counts el

                    else
                        countTo rest (count + counts el)
    in
    countTo (toElementsInOrder rga) 0


{-| Look up an element by id.
-}
get : OpId -> Rga c -> Maybe (Element c)
get id (Rga d) =
    Dict.get (Id.opIdToString id) d


{-| Update the element with the given id by transforming its content. No-op if no
element carries that id (tombstoned elements are updated like any other).
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
