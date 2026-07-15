module BlockCoreTests exposing (suite)

{-| Unit tests for the pure block read model (`Crdt.RichText.toBlocks`), building
`Node.RichNode`s by hand — no ops or wire yet. Elements are placed explicitly (chars,
markers, nest tokens, each with an id) so we can construct arbitrary block layouts and
pin: implicit leading block, split into two blocks, block type from the marker's
`block` mark, indent depth from nest-token count, and that tombstoned markers/tokens
(merge / outdent) are skipped.
-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.Node as Node exposing (AnchorSide(..), MarkOp, Prim(..))
import Crdt.Rga as Rga
import Crdt.RichText.Internal as RichText
import Dict
import Expect
import Test exposing (Test, describe, test)


rid : Int -> OpId
rid n =
    Id.opId n (Id.replica "r")


{-| One element to place in the sequence, with its 1-based id `n`.
-}
type Elem
    = Char Int Char
    | Marker Int
    | Token Int
    | Dead Elem -- tombstoned


{-| Build a rich node from an ordered element list (chained left-to-right by id) and
a list of mark ops. Ids come from each element; parent = the previous element's id.
-}
build : List Elem -> List MarkOp -> Node.RichNode
build elems marks =
    let
        idOf e =
            case e of
                Char n _ ->
                    n

                Marker n ->
                    n

                Token n ->
                    n

                Dead inner ->
                    idOf inner

        contentOf e =
            case e of
                Char n c ->
                    Node.reg (PString (String.fromChar c)) (rid n)

                Marker _ ->
                    RichText.markerNode

                Token _ ->
                    RichText.nestTokenNode

                Dead inner ->
                    contentOf inner

        isDead e =
            case e of
                Dead _ ->
                    True

                _ ->
                    False

        ( text, _ ) =
            List.foldl
                (\e ( rga, prev ) ->
                    let
                        n =
                            idOf e

                        el =
                            Rga.element (rid n) prev Rga.Right (contentOf e) (isDead e)
                    in
                    ( Rga.put el rga, Just (rid n) )
                )
                ( Rga.empty, Nothing )
                elems
    in
    { text = text
    , marks = marks |> List.map (\m -> ( Id.opIdToString m.id, m )) |> Dict.fromList
    }


{-| A `block`-type mark over the single marker element with id `markerN`: start
`Before` it, end `After` it, so it covers exactly that element.
-}
blockMark : Int -> Int -> String -> MarkOp
blockMark idN markerN type_ =
    { id = rid idN
    , type_ = RichText.blockTypeMark
    , value = PString type_
    , start = { ref = Just (rid markerN), side = Before }
    , end = { ref = Just (rid markerN), side = After }
    }


{-| Render blocks compactly: `type:depth"text"` per block, joined by `|`.
-}
render : Node.RichNode -> String
render node =
    RichText.toBlocks node
        |> List.map
            (\b ->
                let
                    txt =
                        b.spans |> List.map .text |> String.concat
                in
                b.type_ ++ ":" ++ String.fromInt b.depth ++ "\"" ++ txt ++ "\""
            )
        |> String.join " | "


suite : Test
suite =
    describe "RichText.toBlocks (pure block read)"
        [ test "no markers → one implicit leading block" <|
            \_ ->
                render (build [ Char 1 'h', Char 2 'i' ] [])
                    |> Expect.equal ":0\"hi\""
        , test "empty node → one empty default block" <|
            \_ ->
                render (build [] [])
                    |> Expect.equal ":0\"\""
        , test "a marker splits into two blocks" <|
            \_ ->
                -- "ab" | marker | "cd"
                render (build [ Char 1 'a', Char 2 'b', Marker 3, Char 4 'c', Char 5 'd' ] [])
                    |> Expect.equal ":0\"ab\" | :0\"cd\""
        , test "marker's block mark types the FOLLOWING block" <|
            \_ ->
                render
                    (build [ Char 1 'a', Marker 2, Char 3 'b' ]
                        [ blockMark 100 2 "h1" ]
                    )
                    |> Expect.equal ":0\"a\" | h1:0\"b\""
        , test "nest tokens after a marker set that block's depth" <|
            \_ ->
                -- "a" | marker | token | token | "b"  → second block depth 2
                render (build [ Char 1 'a', Marker 2, Token 3, Token 4, Char 5 'b' ] [])
                    |> Expect.equal ":0\"a\" | :2\"b\""
        , test "tokens before the first marker indent the leading block" <|
            \_ ->
                render (build [ Token 1, Char 2 'a' ] [])
                    |> Expect.equal ":1\"a\""
        , test "a tombstoned marker is skipped (merged block)" <|
            \_ ->
                render (build [ Char 1 'a', Dead (Marker 2), Char 3 'b' ] [])
                    |> Expect.equal ":0\"ab\""
        , test "a tombstoned nest token lowers depth (outdent)" <|
            \_ ->
                render (build [ Char 1 'a', Marker 2, Token 3, Dead (Token 4), Char 5 'b' ] [])
                    |> Expect.equal ":0\"a\" | :1\"b\""
        , test "type + depth + inline text together" <|
            \_ ->
                render
                    (build [ Char 1 'x', Marker 2, Token 3, Char 4 'y', Char 5 'z' ]
                        [ blockMark 100 2 "ul" ]
                    )
                    |> Expect.equal ":0\"x\" | ul:1\"yz\""
        , test "inline marks still flatten within a block" <|
            \_ ->
                -- bold the 'b' in the second block
                let
                    bold =
                        { id = rid 100
                        , type_ = "bold"
                        , value = PBool True
                        , start = { ref = Just (rid 3), side = Before }
                        , end = { ref = Just (rid 3), side = After }
                        }
                in
                RichText.toBlocks (build [ Char 1 'a', Marker 2, Char 3 'b' ] [ bold ])
                    |> List.map (\b -> List.map (\s -> ( s.text, Dict.keys s.marks )) b.spans)
                    |> Expect.equal [ [ ( "a", [] ) ], [ ( "b", [ "bold" ] ) ] ]
        ]
