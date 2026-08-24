module Crdt.Text exposing (fromString, read)

{-| The collaborative-text layer over `Crdt.Rga`.

Text is an `Rga String` — **one character per element**. It works at the `Char` level:
combining characters and multi-codepoint emoji are split; grapheme-cluster segmentation is a
future refinement, and is why the content is a `String` rather than a `Char` (a cluster is a
string, so that change stays inside this type).

The single-character invariant is enforced where untrusted data enters, by
`Crdt.Json.charDecoder`, so nothing downstream has to re-check it — which is the point of the
content being typed at all (`design-docs/16-typed-sequence-content.md`). Both functions here
are therefore total.

This module is just the two ends of that: `fromString` builds a fresh text RGA (how a text
field is seeded) and `read` reads one back (how it is decoded). The heart of collaborative
editing lives in the op layer: `Crdt.Doc.Internal.applyTextDiff` diffs the current value
against the desired new value and emits the minimal run of insert/delete ops, rather than
overwriting the text (which would clobber a peer's concurrent edits). Concurrent edits from
two replicas then merge character-wise.

@docs fromString, read

-}

import Crdt.Id.Internal exposing (Ctx)
import Crdt.Rga as Rga exposing (Rga)


{-| Build a fresh text RGA from a string, chaining each character after the
previous one.

Unlike the other containers this consumes no ids from the context beyond the element ids
themselves: a character has no stamp, because it is never written in place (an edit is an
insert plus a delete) and ordering comes from the element id.

-}
fromString : Ctx -> String -> ( Rga String, Ctx )
fromString ctx str =
    String.toList str
        |> List.foldl
            (\char ( rga, c, origin ) ->
                let
                    ( rga1, c1 ) =
                        Rga.insertAfter c origin (String.fromChar char) rga
                in
                ( rga1, c1, Rga.lastVisibleId rga1 )
            )
            ( Rga.empty, ctx, Nothing )
        |> (\( rga, c, _ ) -> ( rga, c ))


{-| Read the text RGA back into a string: the visible characters, in order.
-}
read : Rga String -> String
read rga =
    Rga.toList rga |> String.concat
