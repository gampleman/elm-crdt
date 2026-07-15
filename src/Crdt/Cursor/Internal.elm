module Crdt.Cursor.Internal exposing
    ( Cursor, Anchor(..)
    , fromParts, steps, anchor, element, sameContainer
    , Range, range, rangeAnchor, rangeFocus
    , encode, decoder, encodeRange, rangeDecoder
    )

{-| Stable positions in a collaborative document.

A `Cursor` names a position by **identity**, not by offset: a stable id-based
path to a sequence/text container (`Crdt.OpLog.Target`, the same id-based path the
op log addresses edits with) plus an `Anchor` for the spot _within_ that
container. Because it is anchored to element `OpId`s — which are immutable — a
cursor stays meaningful as other replicas concurrently insert, delete, or
reorder around it. Resolve one back to a current offset with
`Crdt.Doc.cursorOffset`.

Cursors are **ephemeral presence data**, not document state: you broadcast them
on the `Crdt.Presence` channel and each viewer resolves them locally against its
own converged document. There is no merge — convergence is just `cursorOffset`
being a pure function of the converged order.

A `Range` (a selection) is simply an ordered pair of cursors — an `anchor` (where
the selection started) and a `focus` (where it currently ends). Each endpoint
resolves independently, so a selection survives concurrent edits the same way a
single caret does.

@docs Cursor, Anchor
@docs fromParts, steps, anchor, element, sameContainer
@docs Range, range, rangeAnchor, rangeFocus
@docs encode, decoder, encodeRange, rangeDecoder

-}

import Crdt.Id exposing (OpId)
import Crdt.Json as Json
import Crdt.OpLog exposing (Target, TargetStep(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


{-| Where a cursor sits within its container's sequence.

  - `Start` — before the first element (offset 0).
  - `After id` — immediately after the element `id` (so the caret is at the
    offset following it, or, read as item-identity, "on" that element).

-}
type Anchor
    = Start
    | After OpId


{-| A stable position: an id-based path to a sequence/text container, plus the
anchor within it. Opaque; build with `Crdt.Doc.cursorAt`.
-}
type Cursor
    = Cursor Target Anchor


{-| Construct a cursor from a container target and an anchor. (Normally you'd use
`Crdt.Doc.cursorAt`, which derives both from a visible-index path + offset.)
-}
fromParts : Target -> Anchor -> Cursor
fromParts =
    Cursor


{-| The anchor (position within the container) of a cursor.
-}
anchor : Cursor -> Anchor
anchor (Cursor _ a) =
    a


{-| The container target steps of a cursor (used by `Crdt.Doc` to resolve it).
-}
steps : Cursor -> Target
steps (Cursor t _) =
    t


{-| Do two cursors point into the **same container** (the same text field or list),
regardless of where within it? Useful when rendering per-field carets: keep only the
peers whose cursor is in the field you are drawing.
-}
sameContainer : Cursor -> Cursor -> Bool
sameContainer (Cursor t1 _) (Cursor t2 _) =
    t1 == t2


{-| The element a cursor is anchored to, if any (`Nothing` for `Start`). Use this
for item-identity presence — "which list item is this peer on" — independent of
the item's current index.
-}
element : Cursor -> Maybe OpId
element (Cursor _ a) =
    case a of
        Start ->
            Nothing

        After id ->
            Just id



-- RANGE ----------------------------------------------------------------------


{-| A selection: an ordered pair of cursors. `anchor` is where the selection
began (the fixed end), `focus` is where it currently extends to (the moving end).
They may be in either document order — a backwards selection has `focus` before
`anchor`.
-}
type Range
    = Range Cursor Cursor


{-| Build a range from its anchor (fixed) and focus (moving) cursors.
-}
range : Cursor -> Cursor -> Range
range =
    Range


{-| The fixed end of a selection.
-}
rangeAnchor : Range -> Cursor
rangeAnchor (Range a _) =
    a


{-| The moving end of a selection.
-}
rangeFocus : Range -> Cursor
rangeFocus (Range _ f) =
    f



-- JSON -----------------------------------------------------------------------


{-| Serialize a cursor for the presence channel.
-}
encode : Cursor -> JE.Value
encode (Cursor t a) =
    JE.object
        [ ( "t", JE.list encodeStep t )
        , ( "a", encodeAnchor a )
        ]


{-| Decode a cursor received from a peer.
-}
decoder : Decoder Cursor
decoder =
    JD.map2 Cursor
        (JD.field "t" (JD.list stepDecoder))
        (JD.field "a" anchorDecoder)


{-| Serialize a range.
-}
encodeRange : Range -> JE.Value
encodeRange (Range a f) =
    JE.object
        [ ( "anchor", encode a )
        , ( "focus", encode f )
        ]


{-| Decode a range.
-}
rangeDecoder : Decoder Range
rangeDecoder =
    JD.map2 Range
        (JD.field "anchor" decoder)
        (JD.field "focus" decoder)


encodeAnchor : Anchor -> JE.Value
encodeAnchor a =
    case a of
        Start ->
            JE.object [ ( "k", JE.string "start" ) ]

        After id ->
            JE.object [ ( "k", JE.string "after" ), ( "id", Json.encodeOpId id ) ]


anchorDecoder : Decoder Anchor
anchorDecoder =
    JD.field "k" JD.string
        |> JD.andThen
            (\k ->
                case k of
                    "start" ->
                        JD.succeed Start

                    "after" ->
                        JD.map After (JD.field "id" Json.opIdDecoder)

                    other ->
                        JD.fail ("unknown anchor kind: " ++ other)
            )


encodeStep : TargetStep -> JE.Value
encodeStep step =
    case step of
        IntoKey key ->
            JE.object [ ( "key", JE.string key ) ]

        IntoElem id ->
            JE.object [ ( "elem", Json.encodeOpId id ) ]


stepDecoder : Decoder TargetStep
stepDecoder =
    JD.oneOf
        [ JD.map IntoKey (JD.field "key" JD.string)
        , JD.map IntoElem (JD.field "elem" Json.opIdDecoder)
        ]
