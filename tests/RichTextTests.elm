module RichTextTests exposing (suite)

{-| Rich text through the **public API**: a `S.richText` field edited via `Crdt.Ref`
(`setRich` + `mark`/`unmark`), read back as `RichText.Span`s, and synced through the
op-log wire. Covers boolean + value marks, per-character LWW resolution, that a mark
follows concurrent text edits (identity anchors), convergence, wire round-trip, and
undo of a mark.
-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText exposing (MarkValue(..), Span)
import Crdt.Schema as S
import Dict
import Expect
import Test exposing (Test, describe, test)


type alias Doc =
    { body : List Span }


type alias DocRefs =
    { body : Ref Doc S.RichK (List Span) }


docDoc : Ref.RecordRefs Doc DocRefs
docDoc =
    Ref.record Doc DocRefs
        |> Ref.field "body" .body S.richText
        |> Ref.build


refs : DocRefs
refs =
    docDoc.refs


init : String -> OpDoc Doc
init name =
    OpDoc.init (Id.replica name) docDoc.schema


ok : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
ok fb =
    Result.withDefault fb


spans : OpDoc Doc -> List Span
spans doc =
    OpDoc.read doc |> Result.map .body |> Result.withDefault []


{-| Compact render: `text{mark,mark=val}` per span, joined by `|`.
-}
render : OpDoc Doc -> String
render doc =
    spans doc
        |> List.map
            (\s ->
                let
                    ms =
                        s.marks
                            |> Dict.toList
                            |> markPairs
                            |> String.join ","
                in
                if ms == "" then
                    s.text

                else
                    s.text ++ "{" ++ ms ++ "}"
            )
        |> String.join "|"


markPairs : List ( String, MarkValue ) -> List String
markPairs =
    List.map
        (\( k, v ) ->
            case v of
                Flag ->
                    k

                Value s ->
                    k ++ "=" ++ s
        )


{-| Merge `from` fully into `to` (full-state exchange).
-}
mergeIn : OpDoc Doc -> OpDoc Doc -> OpDoc Doc
mergeIn from to =
    OpDoc.decodeInto (OpDoc.encode from) to |> Result.withDefault to


{-| A doc seeded with `s` as plain text (one unformatted span).
-}
withText : String -> OpDoc Doc
withText s =
    Ref.setRich refs.body s (init "a") |> ok (init "a")


suite : Test
suite =
    describe "Rich text — public API (S.richText + Crdt.Ref)"
        [ describe "boolean marks"
            [ test "bold a middle range splits into three spans" <|
                \_ ->
                    let
                        d =
                            withText "hello"
                                |> (\doc -> Ref.mark refs.body 1 4 "bold" Flag doc |> ok doc)
                    in
                    render d |> Expect.equal "h|ell{bold}|o"
            , test "unmark a sub-range (LWW: the later clear wins)" <|
                \_ ->
                    let
                        d =
                            withText "hello"
                                |> (\doc -> Ref.mark refs.body 0 5 "bold" Flag doc |> ok doc)
                                |> (\doc -> Ref.unmark refs.body 2 4 "bold" doc |> ok doc)
                    in
                    render d |> Expect.equal "he{bold}|ll|o{bold}"
            , test "two mark types stack" <|
                \_ ->
                    let
                        d =
                            withText "hi"
                                |> (\doc -> Ref.mark refs.body 0 2 "bold" Flag doc |> ok doc)
                                |> (\doc -> Ref.mark refs.body 0 2 "italic" Flag doc |> ok doc)
                    in
                    render d |> Expect.equal "hi{bold,italic}"
            ]
        , describe "value marks"
            [ test "a link carries its href" <|
                \_ ->
                    let
                        d =
                            withText "click"
                                |> (\doc -> Ref.mark refs.body 0 5 "link" (Value "x.com") doc |> ok doc)
                    in
                    render d |> Expect.equal "click{link=x.com}"
            , test "concurrent different links converge to the higher-id one" <|
                \_ ->
                    let
                        base =
                            withText "link"

                        alice =
                            mergeIn base (init "alice")
                                |> (\doc -> Ref.mark refs.body 0 4 "link" (Value "alice.com") doc |> ok doc)

                        bob =
                            mergeIn base (init "bob")
                                |> (\doc -> Ref.mark refs.body 0 4 "link" (Value "bob.com") doc |> ok doc)

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (render ab) (render ba)

                        -- exactly one link wins over the whole range (not split)
                        , \_ ->
                            Expect.equal True
                                (render ab == "link{link=alice.com}" || render ab == "link{link=bob.com}")
                        ]
                        ()
            ]
        , describe "marks follow concurrent text edits (identity anchors)"
            [ test "text inserted inside a bold range is also bold" <|
                \_ ->
                    -- bold all of "ac", then a peer inserts "b" in the middle → "abc"
                    -- all bold, because the mark anchors to the surviving chars.
                    let
                        base =
                            withText "ac"
                                |> (\doc -> Ref.mark refs.body 0 2 "bold" Flag doc |> ok doc)

                        peer =
                            mergeIn base (init "peer")
                                |> (\doc -> Ref.setRich refs.body "abc" doc |> ok doc)

                        merged =
                            mergeIn peer base
                    in
                    render merged |> Expect.equal "abc{bold}"
            , test "deleting a boundary char keeps the mark resolving" <|
                \_ ->
                    -- bold "ell" in "hello", then delete "h": mark still bolds "ell".
                    let
                        d =
                            withText "hello"
                                |> (\doc -> Ref.mark refs.body 1 4 "bold" Flag doc |> ok doc)
                                |> (\doc -> Ref.setRich refs.body "ello" doc |> ok doc)
                    in
                    render d |> Expect.equal "ell{bold}|o"
            ]
        , describe "sync + wire"
            [ test "full-state exchange converges" <|
                \_ ->
                    let
                        alice =
                            withText "hello"
                                |> (\doc -> Ref.mark refs.body 0 5 "bold" Flag doc |> ok doc)

                        bob =
                            OpDoc.decodeInto (OpDoc.encode alice) (init "bob") |> Result.withDefault (init "bob")
                    in
                    Expect.equal (render alice) (render bob)
            , test "a mark delta reaches a peer" <|
                \_ ->
                    let
                        base =
                            withText "hello"

                        before =
                            OpDoc.version base

                        alice =
                            Ref.mark refs.body 0 5 "italic" Flag base |> ok base

                        bob =
                            OpDoc.decodeInto (OpDoc.encodeSince before alice) (mergeIn base (init "bob"))
                                |> Result.withDefault (init "bob")
                    in
                    render bob |> Expect.equal "hello{italic}"
            ]
        , describe "undo"
            [ test "undo a mark removes it" <|
                \_ ->
                    let
                        base =
                            withText "hello"

                        marked =
                            OpDoc.recordEdit (OpDoc.version base)
                                (Ref.mark refs.body 0 5 "bold" Flag base |> ok base)
                    in
                    Expect.all
                        [ \_ -> Expect.equal "hello{bold}" (render marked)
                        , \_ -> Expect.equal "hello" (render (OpDoc.undo marked))
                        ]
                        ()
            , test "undo a mark then redo re-applies it" <|
                \_ ->
                    let
                        base =
                            withText "hello"

                        marked =
                            OpDoc.recordEdit (OpDoc.version base)
                                (Ref.mark refs.body 0 5 "bold" Flag base |> ok base)

                        cycled =
                            marked |> OpDoc.undo |> OpDoc.redo
                    in
                    Expect.equal "hello{bold}" (render cycled)
            , test "type a character, undo, then redo restores it" <|
                \_ ->
                    -- the demo's editor path: setRich changes the text; undo removes
                    -- the char, redo must re-insert it (a Rich-node text element).
                    let
                        typed =
                            OpDoc.recordEdit (OpDoc.version (init "a"))
                                (Ref.setRich refs.body "f" (init "a") |> ok (init "a"))

                        undone =
                            OpDoc.undo typed

                        redone =
                            OpDoc.redo undone
                    in
                    Expect.all
                        [ \_ -> Expect.equal "f" (render typed)
                        , \_ -> Expect.equal "" (render undone)
                        , \_ -> Expect.equal "f" (render redone)
                        ]
                        ()
            , test "type several chars, undo, redo restores all in order" <|
                \_ ->
                    let
                        typed =
                            OpDoc.recordEdit (OpDoc.version (init "a"))
                                (Ref.setRich refs.body "hello" (init "a") |> ok (init "a"))

                        cycled =
                            typed |> OpDoc.undo |> OpDoc.redo
                    in
                    Expect.all
                        [ \_ -> Expect.equal "" (render (OpDoc.undo typed))
                        , \_ -> Expect.equal "hello" (render cycled)
                        ]
                        ()
            ]
        ]
