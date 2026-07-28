module BlockTests exposing (suite)

{-| Block structure end-to-end through the public API: a `C.richText` field edited
via `Crdt.Ref` block edits (`splitBlock`/`mergeBlock`/`setBlockType`/`indentBlock`/
`outdentBlock`) and read back as `RichText.Block`s via `Doc.readBlocks`, synced
through the op-log wire. Pins the properties from `docs/11`: split/merge preserve
char identity, type-change converges by LWW, marks span a split, concurrent split +
edit converge, indent/outdent (incl. concurrency), and wire + undo round-trips.
-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.RichText exposing (Block, MarkValue(..), Span)
import Dict
import Expect
import Test exposing (Test, describe, test)


type alias Sample =
    { body : List Span }


type alias DocDoc =
    { body : Ref Sample C.RichK (List Span)
    , schema : C.Schema C.Nested Sample
    }


docDoc : DocDoc
docDoc =
    C.record Sample DocDoc
        |> C.field "body" .body C.richText
        |> C.build


refs : DocDoc
refs =
    docDoc


bodyPath : Path
bodyPath =
    Path.root |> Path.field "body"


init : String -> Doc Sample
init name =
    Doc.init (Id.replica name) docDoc.schema


ok : Doc Sample -> Result Edit.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


blocks : Doc Sample -> List Block
blocks doc =
    Doc.readBlocks bodyPath doc |> Result.withDefault []


{-| Compact render: `type:depth"text"` per block, joined by `|` (matching
BlockCoreTests). Marks within a block are ignored here; a dedicated test checks them.
-}
render : Doc Sample -> String
render doc =
    blocks doc
        |> List.map
            (\b ->
                let
                    txt =
                        b.spans |> List.map .text |> String.concat
                in
                b.type_ ++ ":" ++ String.fromInt b.depth ++ "\"" ++ txt ++ "\""
            )
        |> String.join " | "


withText : String -> Doc Sample
withText s =
    Edit.setRich refs.body s (init "a") |> ok (init "a")


mergeIn : Doc Sample -> Doc Sample -> Doc Sample
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to


suite : Test
suite =
    describe "Rich-text block structure (C.richText + Ref block edits)"
        [ describe "split / merge"
            [ test "split a paragraph into two blocks" <|
                \_ ->
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                    in
                    render d |> Expect.equal ":0\"ab\" | :0\"cd\""
            , test "split preserves character identity (a mark still resolves across it)" <|
                \_ ->
                    -- bold "bc" (offsets 1..3), then split at 2 (between b and c);
                    -- both halves keep the bold because it anchors to char ids.
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.mark refs.body 1 3 "bold" Flag doc |> ok doc)
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        boldTexts =
                            blocks d
                                |> List.concatMap .spans
                                |> List.filter (\s -> Dict.member "bold" s.marks)
                                |> List.map .text
                    in
                    boldTexts |> Expect.equal [ "b", "c" ]
            , test "mark offsets are char-only: a mark in a later block skips the marker" <|
                \_ ->
                    -- regression: `mark` mapped offsets through marker-inclusive ids, so
                    -- with a block marker in the sequence a mark landed one char early
                    -- per preceding marker. Editor sends char-only offsets; bold [3,5)
                    -- of "abcdef" split into "ab"|"cdef" must be "de", not "cd".
                    let
                        d =
                            withText "abcdef"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                                |> (\doc -> Edit.mark refs.body 3 5 "bold" Flag doc |> ok doc)

                        boldTexts =
                            blocks d
                                |> List.concatMap .spans
                                |> List.filter (\s -> Dict.member "bold" s.marks)
                                |> List.map .text
                                |> String.concat
                    in
                    boldTexts |> Expect.equal "de"
            , test "merge removes the marker and rejoins the blocks" <|
                \_ ->
                    let
                        split =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        merged =
                            Edit.mergeBlock refs.body 1 split |> ok split
                    in
                    render merged |> Expect.equal ":0\"abcd\""
            , test "editing text after a split still targets the right chars (marker in sequence)" <|
                \_ ->
                    -- split "abcd" into "ab"|"cd", then retype the whole text with the
                    -- second block extended: setRich must diff over CHARS ONLY, not be
                    -- thrown off by the marker element sitting in the sequence.
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                                |> (\doc -> Edit.setRich refs.body "abcdef" doc |> ok doc)
                    in
                    render d |> Expect.equal ":0\"ab\" | :0\"cdef\""
            , test "typing into a later block via setBlockText lands there, not block 0 (He\\nd)" <|
                \_ ->
                    -- reproduces the reported bug: type "He", Enter (split at 2),
                    -- then type "d" into the now-empty second block. A whole-document
                    -- diff would place "d" in block 0 ("Hed"); block-scoped editing
                    -- must place it in block 1.
                    let
                        d =
                            withText "He"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                                |> (\doc -> Edit.setBlockText refs.body 1 "d" doc |> ok doc)
                    in
                    render d |> Expect.equal ":0\"He\" | :0\"d\""
            , test "typing into an EMPTY block bounded by a following block lands there (not the next block)" <|
                \_ ->
                    -- found by BlockPropertyTests: split at offset 0 leaves an empty
                    -- first block followed by another; typing into it must land between
                    -- the two markers, not float past the next marker into block 1.
                    let
                        d =
                            withText "hello"
                                |> (\doc -> Edit.splitBlock refs.body 0 0 doc |> ok doc)
                                |> (\doc -> Edit.setBlockText refs.body 0 "X" doc |> ok doc)
                    in
                    render d |> Expect.equal ":0\"X\" | :0\"hello\""
            , test "setBlockText edits only its block, leaving others intact" <|
                \_ ->
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                                |> (\doc -> Edit.setBlockText refs.body 0 "abZ" doc |> ok doc)
                    in
                    render d |> Expect.equal ":0\"abZ\" | :0\"cd\""
            ]
        , describe "block type"
            [ test "set a block's type" <|
                \_ ->
                    let
                        split =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        typed =
                            Edit.setBlockType refs.body 1 (Just "h1") split |> ok split
                    in
                    render typed |> Expect.equal ":0\"ab\" | h1:0\"cd\""
            , test "the first block is formattable (leading marker created lazily)" <|
                \_ ->
                    -- reproduces the reported bug: block 0 has no separator marker, so
                    -- setBlockType must lazily create a leading marker to attach the type.
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.setBlockType refs.body 0 (Just "h1") doc |> ok doc)
                    in
                    render d |> Expect.equal "h1:0\"abcd\""
            , test "splitting after block 0 is formatted places the marker correctly (leading marker not a separator)" <|
                \_ ->
                    -- reproduces the reported bug: format block 0 (creates the leading
                    -- marker), split into two blocks, then split block 1 at its start.
                    -- The leading marker must NOT shift block indices, else the split
                    -- lands before block 0's text. And the split inherits the ul type.
                    let
                        d =
                            withText "ferfefrefre"
                                |> (\doc -> Edit.splitBlock refs.body 0 5 doc |> ok doc)
                                |> (\doc -> Edit.setBlockType refs.body 0 (Just "ul") doc |> ok doc)
                                |> (\doc -> Edit.setBlockType refs.body 1 (Just "ul") doc |> ok doc)
                                |> (\doc -> Edit.splitBlock refs.body 1 0 doc |> ok doc)
                    in
                    render d |> Expect.equal "ul:0\"ferfe\" | ul:0\"\" | ul:0\"frefre\""
            , test "split inherits the block's type and depth" <|
                \_ ->
                    let
                        d =
                            withText "abcd"
                                |> (\doc -> Edit.setBlockType refs.body 0 (Just "ul") doc |> ok doc)
                                |> (\doc -> Edit.indentBlock refs.body 0 doc |> ok doc)
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                    in
                    render d |> Expect.equal "ul:1\"ab\" | ul:1\"cd\""
            , test "concurrent first-block formatting dedupes via the well-known leading id" <|
                \_ ->
                    -- two peers format block 0 concurrently; the leading marker has a
                    -- constant id so both creations converge to ONE block, not two.
                    let
                        base =
                            withText "abcd"

                        alice =
                            Edit.setBlockType refs.body 0 (Just "h1") (mergeIn base (init "alice")) |> ok base

                        bob =
                            Edit.setBlockType refs.body 0 (Just "blockquote") (mergeIn base (init "bob")) |> ok base

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (render ab) (render ba)
                        , \_ -> Expect.equal 1 (List.length (blocks ab))
                        , \_ ->
                            Expect.equal True
                                (render ab == "h1:0\"abcd\"" || render ab == "blockquote:0\"abcd\"")
                        ]
                        ()
            , test "concurrent different types converge by LWW" <|
                \_ ->
                    let
                        base =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        setType peer t =
                            Edit.setBlockType refs.body 1 (Just t) (mergeIn base (init peer))
                                |> ok base

                        alice =
                            setType "alice" "h1"

                        bob =
                            setType "bob" "blockquote"

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (render ab) (render ba)
                        , \_ ->
                            Expect.equal True
                                (render ab == ":0\"ab\" | h1:0\"cd\"" || render ab == ":0\"ab\" | blockquote:0\"cd\"")
                        ]
                        ()
            , test "leading marker converges when peers create it from different local states" <|
                \_ ->
                    -- found by BlockConvergenceTests: the leading marker's placement must
                    -- NOT depend on local state, else two peers emit the same well-known
                    -- id with different parents and the merge is order-dependent. Here A
                    -- indents block 0 (block 0 still has text) while B empties block 0
                    -- then indents it (no first char) — both lazily create the leading
                    -- marker; the reads must converge in both merge orders.
                    let
                        base =
                            withText "alpha"

                        a =
                            Edit.indentBlock refs.body 0 (mergeIn base (init "alice")) |> ok base

                        b =
                            mergeIn base (init "bob")
                                |> (\d -> Edit.setBlockText refs.body 0 "" d |> ok d)
                                |> (\d -> Edit.indentBlock refs.body 0 d |> ok d)

                        ab =
                            mergeIn b a

                        ba =
                            mergeIn a b
                    in
                    render ab |> Expect.equal (render ba)
            ]
        , describe "indent / outdent"
            [ test "indent raises depth, outdent lowers it" <|
                \_ ->
                    let
                        split =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        indentedTwice =
                            split
                                |> (\d -> Edit.indentBlock refs.body 1 d |> ok d)
                                |> (\d -> Edit.indentBlock refs.body 1 d |> ok d)

                        outdentedOnce =
                            Edit.outdentBlock refs.body 1 indentedTwice |> ok indentedTwice
                    in
                    Expect.all
                        [ \_ -> Expect.equal ":0\"ab\" | :2\"cd\"" (render indentedTwice)
                        , \_ -> Expect.equal ":0\"ab\" | :1\"cd\"" (render outdentedOnce)
                        ]
                        ()
            , test "outdent at depth 0 is a no-op" <|
                \_ ->
                    let
                        split =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        d =
                            Edit.outdentBlock refs.body 1 split |> ok split
                    in
                    render d |> Expect.equal ":0\"ab\" | :0\"cd\""
            , test "concurrent indent + outdent cancel (net depth unchanged)" <|
                \_ ->
                    -- start at depth 1; A indents (→2), B outdents (→0); merge = 1.
                    let
                        base =
                            withText "abcd"
                                |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)
                                |> (\doc -> Edit.indentBlock refs.body 1 doc |> ok doc)

                        alice =
                            Edit.indentBlock refs.body 1 (mergeIn base (init "alice")) |> ok base

                        bob =
                            Edit.outdentBlock refs.body 1 (mergeIn base (init "bob")) |> ok base

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (render ab) (render ba)
                        , \_ -> Expect.equal ":0\"ab\" | :1\"cd\"" (render ab)
                        ]
                        ()
            ]
        , describe "concurrent split + edit"
            [ test "peer types while another splits; converge with text on the right side" <|
                \_ ->
                    let
                        base =
                            withText "abcd"

                        -- alice splits at 2; bob appends "X" at the end (offset 4)
                        alice =
                            Edit.splitBlock refs.body 0 2 (mergeIn base (init "alice")) |> ok base

                        bob =
                            Edit.setRich refs.body "abcdX" (mergeIn base (init "bob")) |> ok base

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (render ab) (render ba)
                        , \_ -> Expect.equal ":0\"ab\" | :0\"cdX\"" (render ab)
                        ]
                        ()
            ]
        , describe "wire + undo"
            [ test "split syncs to a peer" <|
                \_ ->
                    let
                        alice =
                            withText "abcd" |> (\doc -> Edit.splitBlock refs.body 0 2 doc |> ok doc)

                        bob =
                            Doc.decodeInto (Doc.encode alice) (init "bob") |> Result.withDefault (init "bob")
                    in
                    render bob |> Expect.equal ":0\"ab\" | :0\"cd\""
            , test "undo a split rejoins the blocks; redo re-splits" <|
                \_ ->
                    let
                        base =
                            withText "abcd"

                        split =
                            Doc.recordEdit (Doc.version base)
                                (Edit.splitBlock refs.body 0 2 base |> ok base)
                    in
                    Expect.all
                        [ \_ -> Expect.equal ":0\"ab\" | :0\"cd\"" (render split)
                        , \_ -> Expect.equal ":0\"abcd\"" (render (Doc.undo split))
                        , \_ -> Expect.equal ":0\"ab\" | :0\"cd\"" (render (split |> Doc.undo |> Doc.redo))
                        ]
                        ()
            ]
        ]
