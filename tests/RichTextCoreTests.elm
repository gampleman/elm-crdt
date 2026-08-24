module RichTextCoreTests exposing (suite)

{-| Unit tests for the pure rich-text read model (`Crdt.RichText.toSpans` + the mark
cover logic), building `Node.RichNode`s by hand — no ops or wire yet. These pin the
Peritext flatten: per-character LWW by op id, PNull clears, run grouping, and that a
mark covers characters strictly between its anchor boundaries.
-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.Node as Node exposing (AnchorSide(..), MarkAnchor, MarkOp, Prim(..))
import Crdt.Rga as Rga
import Crdt.RichText as RichText
import Crdt.RichText.Internal as RTI
import Dict
import Expect
import Test exposing (Test, describe, test)


rid : Int -> OpId
rid n =
    Id.opId n (Id.replica "r")


{-| Build a rich node from a plain string (chars chained left-to-right, ids 1..n) and
a list of mark ops. Char `i` (1-based) has id `rid i`, so anchors can reference chars
by position for test brevity.
-}
richFrom : String -> List MarkOp -> Node.RichNode
richFrom str marks =
    let
        chars =
            String.toList str

        text =
            List.indexedMap Tuple.pair chars
                |> List.foldl
                    (\( i, ch ) rga ->
                        let
                            id =
                                rid (i + 1)

                            parent =
                                if i == 0 then
                                    Nothing

                                else
                                    Just (rid i)
                        in
                        Rga.put (Rga.element id parent Rga.Right (Node.TextChar (String.fromChar ch)) False) rga
                    )
                    Rga.empty
    in
    { text = text
    , marks = marks |> List.map (\m -> ( Id.opIdToString m.id, m )) |> Dict.fromList
    }


{-| A mark op: id counter, type, value, and start/end anchors given as (char-index,
side) — index 0 with Before/After means start/end of text.
-}
mark : Int -> String -> Prim -> ( Int, AnchorSide ) -> ( Int, AnchorSide ) -> MarkOp
mark idN type_ value ( sI, sSide ) ( eI, eSide ) =
    { id = rid idN
    , type_ = type_
    , value = value
    , start = anchorAt sI sSide
    , end = anchorAt eI eSide
    }


anchorAt : Int -> AnchorSide -> MarkAnchor
anchorAt i side =
    { ref =
        if i == 0 then
            Nothing

        else
            Just (rid i)
    , side = side
    }


{-| Render spans compactly: each span as `text{marks}` joined by `|`, marks sorted.
E.g. `he|llo{bold}` = "he" unmarked, "llo" bold.
-}
render : Node.RichNode -> String
render node =
    RTI.toSpans node
        |> List.map
            (\s ->
                let
                    ms =
                        Dict.toList s.marks
                            |> List.map (\( k, v ) -> k ++ primStr v)
                            |> String.join ","
                in
                if ms == "" then
                    s.text

                else
                    s.text ++ "{" ++ ms ++ "}"
            )
        |> String.join "|"


primStr : RichText.MarkValue -> String
primStr v =
    case v of
        RichText.Flag ->
            ""

        RichText.Value s ->
            "=" ++ s


suite : Test
suite =
    describe "RichText.toSpans (pure flatten)"
        [ test "no marks → one plain span" <|
            \_ ->
                render (richFrom "hello" []) |> Expect.equal "hello"
        , test "plainText ignores marks" <|
            \_ ->
                RTI.plainText (richFrom "hello" [ mark 100 "bold" (PBool True) ( 0, Before ) ( 0, After ) ])
                    |> Expect.equal "hello"
        , test "bold a middle range splits into three spans" <|
            \_ ->
                -- "hello", bold chars 2..4 ("ell"): start Before char2, end After char4
                render (richFrom "hello" [ mark 100 "bold" (PBool True) ( 2, Before ) ( 4, After ) ])
                    |> Expect.equal "h|ell{bold}|o"
        , test "a mark over the whole text via start/end sentinels" <|
            \_ ->
                render (richFrom "hi" [ mark 100 "bold" (PBool True) ( 0, Before ) ( 0, After ) ])
                    |> Expect.equal "hi{bold}"
        , test "later op with higher id wins (unbold a sub-range)" <|
            \_ ->
                -- bold all of "hello" (id 100), then unbold "ll" (chars 3..4, id 200)
                render
                    (richFrom "hello"
                        [ mark 100 "bold" (PBool True) ( 0, Before ) ( 0, After )
                        , mark 200 "bold" PNull ( 3, Before ) ( 4, After )
                        ]
                    )
                    |> Expect.equal "he{bold}|ll|o{bold}"
        , test "lower id cannot override a higher id on the overlap" <|
            \_ ->
                -- unbold "ll" with id 200 first, then a LOWER-id bold-all (id 100):
                -- on chars 3..4 the higher id (200, the clear) still wins.
                render
                    (richFrom "hello"
                        [ mark 200 "bold" PNull ( 3, Before ) ( 4, After )
                        , mark 100 "bold" (PBool True) ( 0, Before ) ( 0, After )
                        ]
                    )
                    |> Expect.equal "he{bold}|ll|o{bold}"
        , test "two different mark types stack on a span" <|
            \_ ->
                render
                    (richFrom "hi"
                        [ mark 100 "bold" (PBool True) ( 0, Before ) ( 0, After )
                        , mark 101 "italic" (PBool True) ( 0, Before ) ( 0, After )
                        ]
                    )
                    |> Expect.equal "hi{bold,italic}"
        , test "value mark carries its string (link)" <|
            \_ ->
                render (richFrom "hi" [ mark 100 "link" (PString "x.com") ( 0, Before ) ( 0, After ) ])
                    |> Expect.equal "hi{link=x.com}"
        , test "value mark LWW: higher-id link wins on overlap" <|
            \_ ->
                render
                    (richFrom "hi"
                        [ mark 100 "link" (PString "old") ( 0, Before ) ( 0, After )
                        , mark 200 "link" (PString "new") ( 0, Before ) ( 0, After )
                        ]
                    )
                    |> Expect.equal "hi{link=new}"
        , test "a mark whose anchor char is absent covers nothing" <|
            \_ ->
                -- reference char id 99 (not present) → boundary unresolved → no cover
                render
                    (richFrom "hi"
                        [ { id = rid 100
                          , type_ = "bold"
                          , value = PBool True
                          , start = { ref = Just (rid 99), side = Before }
                          , end = { ref = Nothing, side = After }
                          }
                        ]
                    )
                    |> Expect.equal "hi"
        ]
