module Crdt.RichText exposing
    ( Span, MarkValue(..), toSpans, plainText
    , covers, markValueAt
    , valueInRange
    , empty, fromSpans
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

@docs Span, MarkValue, toSpans, plainText
@docs covers, markValueAt
@docs valueInRange
@docs empty, fromSpans

-}

import Crdt.Id as Id exposing (Ctx, OpId)
import Crdt.Node as Node exposing (AnchorSide(..), MarkAnchor, MarkOp, Prim(..), RichNode)
import Crdt.Rga as Rga
import Dict exposing (Dict)


{-| The value a mark carries where it is active: a boolean mark that is simply "on"
(`Flag`, e.g. bold), or a value mark carrying a string (`Value`, e.g. a link href or
a color). Cleared marks are never represented — an absent key means "not active".
-}
type MarkValue
    = Flag
    | Value String


{-| A run of text sharing one active mark set. `marks` maps a mark `type_` (e.g.
`"bold"`, `"link"`) to its active `MarkValue`; a key's presence means the mark is on
for this run.
-}
type alias Span =
    { text : String
    , marks : Dict String MarkValue
    }


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
are half-open boundaries between characters (or ±∞ for start/end of text), so a
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


charOf : Node.Node -> Maybe String
charOf node =
    case Node.asPrim node of
        Just (PString s) ->
            Just s

        _ ->
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
                                                Rga.put (Rga.element id parent Rga.Right (Node.reg (PString (String.fromChar ch)) id) False) r
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


lastId : Rga.Rga Node.Node -> Maybe OpId
lastId rga =
    Rga.toElementsInOrder rga |> List.reverse |> List.head |> Maybe.map .id


firstAndLast : List OpId -> Maybe ( OpId, OpId )
firstAndLast ids =
    case ( List.head ids, List.reverse ids |> List.head ) of
        ( Just f, Just l ) ->
            Just ( f, l )

        _ ->
            Nothing
