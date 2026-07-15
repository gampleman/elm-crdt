module Crdt.Cursor exposing
    ( Cursor, element, sameContainer
    , Range, range, rangeAnchor, rangeFocus
    , encode, decoder, encodeRange, rangeDecoder
    )

{-| A stable position in a collaborative document — where someone's caret or selection
sits, so you can show remote collaborators' cursors.

A `Cursor` remembers a position by the **identity** of the characters around it, not by
a numeric offset. That is what makes it survive concurrent editing: if a collaborator's
caret is between characters 3 and 4 and someone else inserts text earlier in the line,
the caret still sits between the same two characters after the edits merge — it does not
drift. You create one with `Crdt.cursorAt` (from a field and an offset) and turn it
back into a current offset, against your own converged document, with
`Crdt.cursorOffset`.

Cursors are **presence data, not part of the document**. You broadcast them on the
`Crdt.Presence` channel and every viewer resolves them locally; there is nothing to
merge. A typical setup carries each peer's caret as an optional presence field:

    peerCodec =
        Presence.codec Peer
            |> Presence.optional "caret" .caret Presence.cursor
            |> Presence.buildCodec

A `Range` is a text selection: the pair of cursors marking its two ends — the **anchor**
where the selection started and the **focus** where the caret now reaches (see `Range`
for why the two ends aren't interchangeable). Each end resolves independently, so a
selection stretches and shifts correctly as the text changes underneath it.


# Cursors

@docs Cursor, element, sameContainer


# Selections

@docs Range, range, rangeAnchor, rangeFocus


# Sending cursors over the wire

@docs encode, decoder, encodeRange, rangeDecoder

-}

import Crdt.Cursor.Internal as I
import Crdt.Id exposing (OpId)
import Json.Decode exposing (Decoder)
import Json.Encode as JE


{-| A stable position within a text or list field. Build one with `Crdt.cursorAt`
and resolve it back to a current offset with `Crdt.cursorOffset`. Opaque.
-}
type alias Cursor =
    I.Cursor


{-| The identity of the element a cursor sits on, if any (`Nothing` when it is at the
very start). Use this for item-level presence — "which list item is this peer on" —
which stays correct no matter how the list is reordered.
-}
element : Cursor -> Maybe OpId
element =
    I.element


{-| Do two cursors point into the **same container** (the same text field or list),
regardless of where within it? Useful when rendering per-field carets: keep only the
peers whose cursor is in the field you are currently drawing.
-}
sameContainer : Cursor -> Cursor -> Bool
sameContainer =
    I.sameContainer


{-| A text selection — the pair of cursors marking its two ends.

Think of selecting text by dragging: the point where you first pressed down stays put,
and the other end follows your pointer. Those are the two ends of a `Range`:

  - the **anchor** is the end that stays put — where the selection started;
  - the **focus** is the end that moves — where it reaches to right now (where the
    caret is).

Keeping them apart, rather than just storing "leftmost" and "rightmost", matters for two
reasons. It records the selection's **direction**: dragging left-to-right puts the focus
_after_ the anchor, dragging right-to-left puts it _before_ — a "backwards" selection is
simply one whose focus sits before its anchor. And it is what lets you **grow or shrink**
a selection from the moving end (shift-clicking, or holding shift and pressing an arrow
key) while the anchor holds fast.

If all you want is the selected span itself, resolve both ends to offsets with
`Crdt.cursorOffset` and take the smaller and larger of the two.

-}
type alias Range =
    I.Range


{-| Build a `Range` from its two ends: the `anchor` (the end that stays put, where the
selection started) and the `focus` (the moving end, where the caret now is).
-}
range : Cursor -> Cursor -> Range
range =
    I.range


{-| The end of the selection that stays put — where it started.
-}
rangeAnchor : Range -> Cursor
rangeAnchor =
    I.rangeAnchor


{-| The moving end of the selection — where the caret currently is.
-}
rangeFocus : Range -> Cursor
rangeFocus =
    I.rangeFocus


{-| Serialize a cursor to JSON, to send on the presence channel.
-}
encode : Cursor -> JE.Value
encode =
    I.encode


{-| Decode a cursor received from a peer.
-}
decoder : Decoder Cursor
decoder =
    I.decoder


{-| Serialize a selection to JSON.
-}
encodeRange : Range -> JE.Value
encodeRange =
    I.encodeRange


{-| Decode a selection received from a peer.
-}
rangeDecoder : Decoder Range
rangeDecoder =
    I.rangeDecoder
