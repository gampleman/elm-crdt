module Crdt.Text exposing
    ( fromString, toString
    , applyString
    )

{-| The collaborative-text layer over `Crdt.Rga`.

Text is an RGA whose elements are single-character registers (`Reg (PString c)`).
v1 works at the `Char` level — combining characters and multi-codepoint emoji are
split; grapheme-cluster segmentation is a documented future refinement.

`applyString` is the heart of collaborative editing: rather than overwriting the
text (which would clobber a peer's concurrent edits), it diffs the current value
against the desired new value and applies the minimal run of RGA insert/delete
operations. Concurrent edits from two replicas then merge character-wise.

@docs fromString, toString
@docs applyString

-}

import Crdt.Id as Id exposing (Ctx, OpId)
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


{-| Transform the text so it reads as `target`, applying the minimal insert/
delete run derived from a common-prefix / common-suffix diff. Returns the
updated RGA and advanced context.

This keeps untouched characters (and their identities) stable, so a remote
replica editing a different region merges cleanly.

-}
applyString : Ctx -> String -> Rga Node -> ( Rga Node, Ctx )
applyString ctx target rga =
    let
        current =
            toString rga

        currentChars =
            String.toList current

        targetChars =
            String.toList target

        prefixLen =
            commonPrefix currentChars targetChars 0

        -- common suffix length, not overlapping the shared prefix
        maxSuffix =
            min (List.length currentChars - prefixLen) (List.length targetChars - prefixLen)

        suffixLen =
            commonSuffix (List.reverse currentChars) (List.reverse targetChars) 0 maxSuffix

        -- delete the differing middle of the current text, back to front so
        -- earlier indices stay valid
        deleteFrom =
            prefixLen

        deleteCount =
            List.length currentChars - prefixLen - suffixLen

        afterDelete =
            List.range deleteFrom (deleteFrom + deleteCount - 1)
                |> List.reverse
                |> List.foldl
                    (\i acc ->
                        case Rga.idAtVisibleIndex i acc of
                            Just id ->
                                Rga.delete id acc

                            Nothing ->
                                acc
                    )
                    rga

        -- insert the differing middle of the target text at the prefix boundary
        insertChars =
            targetChars
                |> List.drop prefixLen
                |> List.take (List.length targetChars - prefixLen - suffixLen)

        startOrigin =
            if prefixLen <= 0 then
                Nothing

            else
                Rga.idAtVisibleIndex (prefixLen - 1) afterDelete
    in
    insertChars
        |> List.foldl
            (\char ( acc, c, origin ) ->
                let
                    ( node, c1 ) =
                        charNode char c

                    ( acc1, c2 ) =
                        Rga.insertAfter c1 origin node acc

                    newId =
                        idJustInsertedAfter origin acc1
                in
                ( acc1, c2, newId )
            )
            ( afterDelete, ctx, startOrigin )
        |> (\( acc, c, _ ) -> ( acc, c ))


{-| Find the id of the element inserted immediately after `origin` — it is the
visible element right after origin's position (or index 0 when origin is head).
-}
idJustInsertedAfter : Maybe OpId -> Rga Node -> Maybe OpId
idJustInsertedAfter origin rga =
    case origin of
        Nothing ->
            Rga.idAtVisibleIndex 0 rga

        Just o ->
            visibleIndexOf o rga
                |> Maybe.andThen (\i -> Rga.idAtVisibleIndex (i + 1) rga)


visibleIndexOf : OpId -> Rga Node -> Maybe Int
visibleIndexOf target rga =
    let
        ids =
            Rga.toElementsInOrder rga
                |> List.filter (not << .deleted)
                |> List.map .id
    in
    indexOfHelp 0 target ids


indexOfHelp : Int -> OpId -> List OpId -> Maybe Int
indexOfHelp i target ids =
    case ids of
        [] ->
            Nothing

        x :: rest ->
            if Id.compareOpId x target == EQ then
                Just i

            else
                indexOfHelp (i + 1) target rest


commonPrefix : List Char -> List Char -> Int -> Int
commonPrefix a b acc =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            if x == y then
                commonPrefix xs ys (acc + 1)

            else
                acc

        _ ->
            acc


commonSuffix : List Char -> List Char -> Int -> Int -> Int
commonSuffix ra rb acc cap =
    if acc >= cap then
        acc

    else
        case ( ra, rb ) of
            ( x :: xs, y :: ys ) ->
                if x == y then
                    commonSuffix xs ys (acc + 1) cap

                else
                    acc

            _ ->
                acc
