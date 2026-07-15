module Crdt.Text exposing (fromString, toString)

{-| The collaborative-text layer over `Crdt.Rga`.

Text is an RGA whose elements are single-character registers (`Reg (PString c)`).
v1 works at the `Char` level — combining characters and multi-codepoint emoji are
split; grapheme-cluster segmentation is a documented future refinement.

`applyString` is the heart of collaborative editing: rather than overwriting the
text (which would clobber a peer's concurrent edits), it diffs the current value
against the desired new value and applies the minimal run of RGA insert/delete
operations. Concurrent edits from two replicas then merge character-wise.

@docs fromString, toString

-}

import Crdt.Id.Internal as Id exposing (Ctx)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga exposing (Rga)


{-| Build a fresh text RGA from a string, chaining each character after the
previous one.
-}
fromString : Ctx -> String -> ( Rga Node, Ctx )
fromString ctx str =
    String.toList str
        |> List.foldl
            (\char ( rga, c, origin ) ->
                let
                    ( node, c1 ) =
                        charNode char c

                    ( rga1, c2 ) =
                        Rga.insertAfter c1 origin node rga

                    newId =
                        Rga.lastVisibleId rga1
                in
                ( rga1, c2, newId )
            )
            ( Rga.empty, ctx, Nothing )
        |> (\( rga, c, _ ) -> ( rga, c ))


{-| Build a single-character register, minting a fresh stamp from the context.
The stamp does not drive ordering (RGA uses element ids for that), but it must
still be globally unique so the document never contains duplicate OpIds.
-}
charNode : Char -> Ctx -> ( Node, Ctx )
charNode char ctx =
    let
        ( stamp, ctx1 ) =
            Id.nextId ctx
    in
    ( Node.reg (PString (String.fromChar char)) stamp, ctx1 )


{-| Read the text RGA back into a string.
-}
toString : Rga Node -> String
toString rga =
    Rga.toList rga
        |> List.filterMap
            (\node ->
                case Node.asPrim node of
                    Just (PString s) ->
                        Just s

                    _ ->
                        Nothing
            )
        |> String.concat
