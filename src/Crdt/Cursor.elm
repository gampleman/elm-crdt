module Crdt.Cursor exposing
    ( Cursor, cursorAt, cursorAtRich, cursorOffset
    , element, sameContainer
    , Range, range, rangeAnchor, rangeFocus
    , encode, decoder, encodeRange, rangeDecoder
    )

{-| A stable position in a collaborative document — where someone's caret or selection
sits, so you can show remote collaborators' cursors.

A `Cursor` remembers a position by the **identity** of the characters around it, not by
a numeric offset. That is what makes it survive concurrent editing: if a collaborator's
caret is between characters 3 and 4 and someone else inserts text earlier in the line,
the caret still sits between the same two characters after the edits merge — it does not
drift. You create one with `cursorAt` (from a text ref and an offset) and turn it
back into a current offset, against your own converged document, with `cursorOffset`.

Cursors are **presence data, not part of the document**. You broadcast them on the
`Crdt.Presence` channel and every viewer resolves them locally; there is nothing to
merge. A typical setup carries each peer's caret as an optional presence field:

    peerCodec =
        Presence.codec Peer
            |> Presence.optional "caret" .caret Presence.cursor
            |> Presence.buildCodec

A `Range` is a text selection: the pair of cursors marking its two ends — the **anchor**
where the selection started and the **focus** where the caret now reaches (see
[`Range`](#Range) for why the two ends aren't interchangeable). Each end resolves
independently, so a
selection stretches and shifts correctly as the text changes underneath it.


# Cursors

@docs Cursor, cursorAt, cursorAtRich, cursorOffset
@docs element, sameContainer


# Selections

@docs Range, range, rangeAnchor, rangeFocus


# Sending cursors over the wire

@docs encode, decoder, encodeRange, rangeDecoder

-}

import Crdt.Cursor.Internal as I
import Crdt.Doc exposing (Doc)
import Crdt.Doc.Internal as DocI
import Crdt.Id exposing (OpId)
import Crdt.Ref.Internal exposing (Ref(..))
import Crdt.RichText exposing (Span)
import Crdt.Schema.Internal as SI
import Json.Decode exposing (Decoder)
import Json.Encode as JE


{-| A stable position within a text field. Build one with `cursorAt` and resolve it
back to a current offset with `cursorOffset`. Opaque.
-}
type alias Cursor =
    I.Cursor


{-| Make a stable cursor at character `offset` in a text field. Because the cursor is
anchored to the characters around it (not to the number), it keeps pointing at the same
spot as the text is edited concurrently. `Nothing` if the ref doesn't resolve to a text
field in this document.

Making a cursor doesn't change the document — it's a read against the current text — so
broadcast it on the [`Crdt.Presence`](Crdt-Presence) channel to show collaborators' carets.

-}
cursorAt : Ref r SI.Settable String -> Int -> Doc doc -> Maybe Cursor
cursorAt (Ref r) offset doc =
    DocI.cursorAt r.path offset doc
        |> Result.toMaybe


{-| Like `cursorAt`, but for a **rich-text** field (a `Crdt.richText` ref). The `offset`
is a document-wide character offset over the flattened text (block boundaries don't count
as characters), matching the offsets `Crdt.Edit.mark` and the span stream use. The
resulting cursor resolves and merges exactly like a plain-text one.
-}
cursorAtRich : Ref r SI.RichK (List Span) -> Int -> Doc doc -> Maybe Cursor
cursorAtRich (Ref r) offset doc =
    DocI.cursorAt r.path offset doc
        |> Result.toMaybe


{-| Resolve a cursor back to its current character offset in this document, or `Nothing`
if it no longer points anywhere (its text is gone). The inverse of `cursorAt`.
-}
cursorOffset : Cursor -> Doc doc -> Maybe Int
cursorOffset =
    DocI.cursorOffset


{-| The identity of the character a cursor sits just after, if any (`Nothing` when it is
at the very start). Because it is an identity rather than an offset, it keeps pointing at
the same character as the text is edited around it.
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
`cursorOffset` and take the smaller and larger of the two.

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
