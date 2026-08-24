module Crdt.RichText.Internal exposing
    ( toSpans, plainText
    , valueInRange
    , empty, fromSpans
    , toBlocks, blockTypeMark
    , markerNode, nestTokenNode, isMarker, isNestToken
    )

{-| The read model for rich (formatted) text: flatten a `Node.RichNode` — a Fugue
character sequence plus an append-only set of Peritext **mark operations** — into a
list of `Span`s, each a maximal run of characters sharing the same active formatting.

A mark op sets (or clears, with `value = PNull`) a formatting mark over a range
anchored to character **identities** (`OpId`s), not offsets, so marks survive
concurrent editing and reordering. When two ops touch the same character and mark
type, the one with the larger `OpId` wins (per-character last-writer-wins — Peritext's
resolution rule). This is a pure read-time computation over the stored op set; no
range splitting or normalization is persisted.

**Boundary / expansion semantics (v1).** A mark covers characters strictly between
its start and end boundaries, where each boundary sits just before/after its anchor
character (per the anchor `side`). So:

  - an insert **inside** a marked range falls between the anchor chars and is
    therefore covered automatically (the range "grows" over interior edits) — this
    is what makes marks robust to concurrent text edits;
  - an insert **at the very boundary** (right after the end char) is _not_ auto-
    covered — extending a mark as you type at its edge is driven by the editor, which
    emits a fresh mark op over the newly typed text (ProseMirror's inclusive/stored
    marks). The `side` field is stored so true sticky/concurrent-boundary expansion
    (the hardest Peritext guarantee) can be added later without a wire change; it is
    a documented v1 limitation, not a divergence — concurrent edits still converge.

@docs toSpans, plainText
@docs valueInRange
@docs empty, fromSpans
@docs toBlocks, blockTypeMark
@docs markerNode, nestTokenNode, isMarker, isNestToken

-}

import Crdt.Id.Internal as Id exposing (Ctx, OpId)
import Crdt.Node as Node exposing (AnchorSide(..), MarkAnchor, MarkOp, Prim(..), RichNode)
import Crdt.Rga as Rga
import Crdt.RichText exposing (Block, MarkValue(..), Span)
import Dict exposing (Dict)


{-| The mark `type_` that carries a block's app-defined type on its marker element.
The library never interprets the value (an opaque string like a `link` href); the app
picks the vocabulary (`"h1"`, `"blockquote"`, `"ul"`, …). No mark = the default block.
-}
blockTypeMark : String
blockTypeMark =
    "block"


{-| The visible (non-tombstoned) characters concatenated, ignoring formatting.
-}
plainText : RichNode -> String
plainText r =
    Rga.toElementsInOrder r.text
        |> List.filter (not << .deleted)
        |> List.filterMap (\el -> charOf el.content)
        |> String.concat


{-| Flatten a rich-text node into maximal same-formatting spans, in document order.
-}
toSpans : RichNode -> List Span
toSpans r =
    let
        -- All elements in Fugue order, tombstones INCLUDED, so a mark anchored to a
        -- since-deleted character still has a stable order position (as with
        -- cursors). Boundary math uses these indices; only live chars are emitted.
        ordered =
            Rga.toElementsInOrder r.text

        indexOf =
            ordered
                |> List.indexedMap (\i el -> ( Id.opIdToString el.id, i ))
                |> Dict.fromList

        marksList =
            Dict.values r.marks

        step ( i, el ) acc =
            if el.deleted then
                acc

            else
                case charOf el.content of
                    Nothing ->
                        acc

                    Just ch ->
                        let
                            active =
                                activeMarks marksList indexOf i
                        in
                        case acc of
                            span :: rest ->
                                if span.marks == active then
                                    { span | text = span.text ++ ch } :: rest

                                else
                                    { text = ch, marks = active } :: span :: rest

                            [] ->
                                [ { text = ch, marks = active } ]
    in
    ordered
        |> List.indexedMap Tuple.pair
        |> List.foldl step []
        |> List.reverse



-- BLOCK STRUCTURE ------------------------------------------------------------
--
-- The sequence carries two *structural* element kinds besides text (see
-- `design-docs/11`):
--   • a MARKER = a block boundary (its `block` mark gives the following block's type,
--     the live nest tokens right after it give the block's depth);
--   • a NEST TOKEN = one unit of indent depth for the block it follows a marker in.
-- These are constructors of `Node.RichElem`, so the compiler enforces that an element is
-- exactly one of the three and every walk over the sequence has to say what it does with
-- each. They used to be non-string prims smuggled inside a register (marker = PInt 0,
-- token = PInt 1), distinguishable only by convention.


{-| Is a mark op the **leading-block type mark** — the `block`-type mark that carries
block 0's type? Block 0 has no marker element to anchor to (it is the run of text
before the first real marker), so its type is stored as a `block` mark anchored to the
document **head**: both endpoints `ref = Nothing`. Such an op never covers any
character (start and end coincide at −∞), so it is inert for normal per-character mark
resolution and is read only by `toBlocks` via `leadingTypeMark`. Concurrent block-0
formatting converges by the same per-target LWW as any mark (highest `OpId` wins) — no
constant id, no phantom element.
-}
isLeadingTypeMark : MarkOp -> Bool
isLeadingTypeMark m =
    m.type_ == blockTypeMark && m.start.ref == Nothing && m.end.ref == Nothing


{-| The winning app-defined type for **block 0** from the head-anchored `block` marks
(see `isLeadingTypeMark`): the highest-`OpId` such op's value, or `""` if none / the
winner is a clear.
-}
leadingTypeMark : List MarkOp -> String
leadingTypeMark marks =
    marks
        |> List.filter isLeadingTypeMark
        |> List.foldl
            (\m acc ->
                case acc of
                    Just prev ->
                        if Id.compareOpId m.id prev.id == GT then
                            Just m

                        else
                            acc

                    Nothing ->
                        Just m
            )
            Nothing
        |> Maybe.andThen (\m -> markValue m.value)
        |> Maybe.map
            (\v ->
                case v of
                    Value s ->
                        s

                    Flag ->
                        ""
            )
        |> Maybe.withDefault ""


{-| The content of a block-boundary marker element (used when seeding / emitting a split).
-}
markerNode : Node.RichElem
markerNode =
    Node.Token Node.Marker


{-| The content of one nest-token (indent-unit) element.
-}
nestTokenNode : Node.RichElem
nestTokenNode =
    Node.Token Node.Nest


{-| Is this element content a block marker?
-}
isMarker : Node.RichElem -> Bool
isMarker elem =
    elem == Node.Token Node.Marker


{-| Is this element content a nest token?
-}
isNestToken : Node.RichElem -> Bool
isNestToken elem =
    elem == Node.Token Node.Nest


{-| Flatten a rich-text node into its **blocks**, each with its app-defined type,
indent depth, and inline spans (see `design-docs/11`). The document always has at least the
implicit leading block (characters before the first marker), so an empty/marker-free
node reads as one default block.

Walk the elements in Fugue order: a live **marker** ends the current block and starts
the next (its resolved `block` mark = the next block's type; the run of live nest
tokens immediately following it = the next block's depth); **nest tokens** are
consumed as depth, not emitted; **chars** flatten into the current block's spans via
the same per-character active-mark logic as `toSpans`. Tombstoned markers/tokens are
skipped (a merged block / an outdent).

-}
toBlocks : RichNode -> List Block
toBlocks r =
    let
        ordered =
            Rga.toElementsInOrder r.text

        indexOf =
            ordered
                |> List.indexedMap (\i el -> ( Id.opIdToString el.id, i ))
                |> Dict.fromList

        marksList =
            Dict.values r.marks

        -- resolve the `block` mark on a marker element (its own single-char range) to
        -- the app-defined type string; `""` when unmarked (the default block).
        markerType : Int -> String
        markerType i =
            case markValueAt marksList indexOf blockTypeMark i of
                Just (Value s) ->
                    s

                _ ->
                    ""

        emptyBlock marker type_ depth =
            { marker = marker, type_ = type_, depth = depth, spans = [] }

        addChar ch active block =
            case List.reverse block.spans of
                span :: restRev ->
                    if span.marks == active then
                        { block | spans = List.reverse ({ span | text = span.text ++ ch } :: restRev) }

                    else
                        { block | spans = block.spans ++ [ { text = ch, marks = active } ] }

                [] ->
                    { block | spans = [ { text = ch, marks = active } ] }

        -- fold state: (finished blocks reversed, current block, depth accumulating
        -- for the current block from nest tokens seen since its marker)
        step ( i, el ) ( done, cur ) =
            if el.deleted then
                ( done, cur )

            else if isMarker el.content then
                -- a separator marker: close current block, open next (its `block` mark
                -- gives the next block's type; nest tokens after it give its depth).
                ( cur :: done, emptyBlock (Just el.id) (markerType i) 0 )

            else if isNestToken el.content then
                ( done, { cur | depth = cur.depth + 1 } )

            else
                case charOf el.content of
                    Just ch ->
                        ( done, addChar ch (activeMarks marksList indexOf i) cur )

                    Nothing ->
                        ( done, cur )

        -- Block 0 has no marker element: its type comes from the head-anchored `block`
        -- mark (`leadingTypeMark`) and its depth from the nest tokens before the first
        -- separator (counted by the same `isNestToken` branch above, since they precede
        -- any marker). `marker = Nothing`.
        block0 =
            emptyBlock Nothing (leadingTypeMark marksList) 0

        ( doneRev, lastBlock ) =
            ordered
                |> List.indexedMap Tuple.pair
                |> List.foldl step ( [], block0 )
    in
    List.reverse (lastBlock :: doneRev)


{-| The active mark set for the character at order-index `i`: for each mark `type_`,
the winning value as a `MarkValue`, with `PNull` (cleared) winners dropped so an
absent key means "not active".
-}
activeMarks : List MarkOp -> Dict String Int -> Int -> Dict String MarkValue
activeMarks marks indexOf i =
    winningOps marks indexOf i
        |> Dict.toList
        |> List.filterMap (\( t, m ) -> markValue m.value |> Maybe.map (Tuple.pair t))
        |> Dict.fromList


{-| For the character at order-index `i`, the winning (highest-`OpId`) mark op per
`type_` whose range covers it — including ops whose value is `PNull` (a clear), since
a clear can be the winner. The public views (`activeMarks`) drop those.
-}
winningOps : List MarkOp -> Dict String Int -> Int -> Dict String MarkOp
winningOps marks indexOf i =
    marks
        |> List.filter (\m -> covers indexOf m i)
        |> List.foldl
            (\m acc ->
                Dict.update m.type_
                    (\existing ->
                        case existing of
                            Just prev ->
                                if Id.compareOpId m.id prev.id == GT then
                                    Just m

                                else
                                    Just prev

                            Nothing ->
                                Just m
                    )
                    acc
            )
            Dict.empty


{-| Convert a stored mark `Prim` value into a public `MarkValue`, or `Nothing` for a
cleared mark (`PNull`).
-}
markValue : Prim -> Maybe MarkValue
markValue prim =
    case prim of
        PNull ->
            Nothing

        PBool True ->
            Just Flag

        PString s ->
            Just (Value s)

        _ ->
            -- any other prim used as a boolean-mark truthy value reads as a flag
            Just Flag


{-| The active `MarkValue` of mark `type_` at order-index `i`, or `Nothing` if that
mark is not active there. Convenience over `activeMarks` for a single type.
-}
markValueAt : List MarkOp -> Dict String Int -> String -> Int -> Maybe MarkValue
markValueAt marks indexOf type_ i =
    activeMarks marks indexOf i |> Dict.get type_


{-| The winning value of mark `type_` at the **first character covered** by the range
`[start, end]`, or `PNull` if the mark is not active there. Used by undo to sample
the value a range had before a mark op was applied so it can re-assert it. This reads
the value at the start of the range only; a range whose prior formatting was _not_
uniform is a documented v1 approximation (undo restores the range's leading value
across the whole range).
-}
valueInRange : RichNode -> String -> MarkAnchor -> MarkAnchor -> Prim
valueInRange r type_ start end =
    let
        ordered =
            Rga.toElementsInOrder r.text

        indexOf =
            ordered
                |> List.indexedMap (\i el -> ( Id.opIdToString el.id, i ))
                |> Dict.fromList

        marksList =
            Dict.values r.marks

        probe =
            { id = Id.opId 0 (Id.replica ""), type_ = type_, value = PNull, start = start, end = end }

        firstCovered =
            ordered
                |> List.indexedMap Tuple.pair
                |> List.filter (\( i, _ ) -> covers indexOf probe i)
                |> List.head
    in
    case firstCovered of
        Just ( i, _ ) ->
            winningOps marksList indexOf i
                |> Dict.get type_
                |> Maybe.map .value
                |> Maybe.withDefault PNull

        Nothing ->
            PNull


{-| Does mark op `m`'s range cover the character at order-index `i`? The endpoints
are boundaries _between_ characters (or ±∞ for start/end of text), so a
character sits strictly between them: `start < i < end`. A missing anchor `ref` (the
referenced character hasn't arrived yet) means the mark covers nothing until it
resolves.
-}
covers : Dict String Int -> MarkOp -> Int -> Bool
covers indexOf m i =
    case ( boundaryPos indexOf m.start, boundaryPos indexOf m.end ) of
        ( Just s, Just e ) ->
            s < toFloat i && toFloat i < e

        _ ->
            False


{-| The numeric boundary position of a mark anchor. A `Before`/`After` side places
the boundary just before/after its `ref` character (at a half-integer, so no
character lands exactly on it); a `Nothing` ref is the start (`Before` → −∞) or end
(`After` → +∞) of the text. `Nothing` result = the ref character is not present.
-}
boundaryPos : Dict String Int -> MarkAnchor -> Maybe Float
boundaryPos indexOf anchor =
    case anchor.ref of
        Nothing ->
            case anchor.side of
                Node.Before ->
                    Just (-1 / 0)

                Node.After ->
                    Just (1 / 0)

        Just id ->
            Dict.get (Id.opIdToString id) indexOf
                |> Maybe.map
                    (\i ->
                        case anchor.side of
                            Node.Before ->
                                toFloat i - 0.5

                            Node.After ->
                                toFloat i + 0.5
                    )


{-| The character an element holds, or `Nothing` for a structural token.
-}
charOf : Node.RichElem -> Maybe String
charOf elem =
    case elem of
        Node.TextChar ch ->
            Just ch

        Node.Token _ ->
            Nothing



-- CONSTRUCTION ---------------------------------------------------------------


{-| An empty rich-text node.
-}
empty : RichNode
empty =
    { text = Rga.empty, marks = Dict.empty }


{-| Build a rich-text node from a list of formatted spans, minting fresh ids from
`ctx` for every character and every mark op. Characters chain left-to-right; each
span's marks become one mark op per `(type, value)` covering exactly that span's
characters (start `Before` the span's first char, end `After` its last). Used to
seed a fresh rich-text value (`Schema.with`).
-}
fromSpans : Ctx -> List Span -> ( RichNode, Ctx )
fromSpans ctx spans =
    let
        -- lay down every character, remembering the id of each so a span can anchor
        -- its marks to its first/last char.
        ( text, ctxAfterChars, spanRanges ) =
            List.foldl
                (\span ( rga, c, ranges ) ->
                    let
                        ( rga1, c1, ids ) =
                            String.toList span.text
                                |> List.foldl
                                    (\ch ( r, cc, acc ) ->
                                        let
                                            ( id, cc1 ) =
                                                Id.nextId cc

                                            parent =
                                                lastId r

                                            r1 =
                                                Rga.put (Rga.element id parent Rga.Right (Node.TextChar (String.fromChar ch)) False) r
                                        in
                                        ( r1, cc1, id :: acc )
                                    )
                                    ( rga, c, [] )

                        charIds =
                            List.reverse ids
                    in
                    ( rga1, c1, ranges ++ [ ( span.marks, charIds ) ] )
                )
                ( Rga.empty, ctx, [] )
                spans

        -- for each span with a nonempty mark set + nonempty char range, emit a mark
        -- op per (type,value) anchored to that span's first/last char.
        ( marks, ctxFinal ) =
            List.foldl
                (\( markSet, charIds ) ( acc, c ) ->
                    case ( firstAndLast charIds, Dict.isEmpty markSet ) of
                        ( Just ( first, last ), False ) ->
                            Dict.foldl
                                (\type_ value ( a, cc ) ->
                                    let
                                        ( markId, cc1 ) =
                                            Id.nextId cc

                                        m =
                                            { id = markId
                                            , type_ = type_
                                            , value = primOf value
                                            , start = { ref = Just first, side = Before }
                                            , end = { ref = Just last, side = After }
                                            }
                                    in
                                    ( Dict.insert (Id.opIdToString markId) m a, cc1 )
                                )
                                ( acc, c )
                                markSet

                        _ ->
                            ( acc, c )
                )
                ( Dict.empty, ctxAfterChars )
                spanRanges
    in
    ( { text = text, marks = marks }, ctxFinal )


{-| The stored `Prim` for a public `MarkValue` (inverse of `markValue`).
-}
primOf : MarkValue -> Prim
primOf mv =
    case mv of
        Flag ->
            PBool True

        Value s ->
            PString s


lastId : Rga.Rga c -> Maybe OpId
lastId rga =
    Rga.toElementsInOrder rga |> List.reverse |> List.head |> Maybe.map .id


firstAndLast : List OpId -> Maybe ( OpId, OpId )
firstAndLast ids =
    case ( List.head ids, List.reverse ids |> List.head ) of
        ( Just f, Just l ) ->
            Just ( f, l )

        _ ->
            Nothing
