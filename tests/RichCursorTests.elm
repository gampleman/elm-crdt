module RichCursorTests exposing (suite)

{-| Rich-text cursor offsets live in **character** space, not raw element space.

A `Rich` sequence interleaves characters with block markers and nest tokens. Every offset a
caller hands the cursor API is a character offset — that's what the editor reports, what the
span stream reads as, and what `Crdt.Edit.mark` marks over (`markRange` filters to char ids
for exactly this reason). Counting markers instead drifted the caret one position per
preceding block boundary, which is invisible in a single-block document — the only shape the
presence-caret e2e test covered — and so went unnoticed.

These tests pin the two halves: the offset SPACE matches the character count regardless of
how many blocks there are, and a caret round-trips through a multi-block document.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.RichText exposing (Span)
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


bodyPath : Path
bodyPath =
    Path.root |> Path.field "body"


init : Doc Sample
init =
    Doc.init (Id.replica "a") docDoc.schema


ok : Doc Sample -> Result Doc.Error (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


{-| The document's flat character stream — the space offsets are expressed in.
-}
flatText : Doc Sample -> String
flatText d =
    Doc.read d
        |> Result.map (.body >> List.map .text >> String.concat)
        |> Result.withDefault "<err>"


{-| One block holding "AB".
-}
oneBlock : Doc Sample
oneBlock =
    init |> (\d -> Doc.setRichText bodyPath "AB" d |> ok d)


{-| "A" / "B" — same two characters, one block boundary between them.
-}
twoBlocks : Doc Sample
twoBlocks =
    oneBlock |> (\d -> Doc.splitBlock bodyPath 0 1 d |> ok d)


{-| "A" / "" / "B" — same two characters, two block boundaries.
-}
threeBlocks : Doc Sample
threeBlocks =
    twoBlocks |> (\d -> Doc.splitBlock bodyPath 1 0 d |> ok d)


{-| The largest offset any cursor in `d` resolves to — i.e. the size of the offset space.
-}
maxOffset : Doc Sample -> Int
maxOffset d =
    List.range 0 12
        |> List.filterMap
            (\k ->
                Doc.cursorAt bodyPath k d
                    |> Result.toMaybe
                    |> Maybe.andThen (\c -> Doc.cursorOffset c d)
            )
        |> List.maximum
        |> Maybe.withDefault -1


roundTrip : Int -> Doc Sample -> Maybe Int
roundTrip k d =
    Doc.cursorAt bodyPath k d
        |> Result.toMaybe
        |> Maybe.andThen (\c -> Doc.cursorOffset c d)


suite : Test
suite =
    describe "rich-text cursor offsets are character offsets"
        [ test "the offset space is the CHARACTER count, whatever the block count" <|
            \_ ->
                -- all three documents hold exactly the two characters "AB"; only the
                -- number of block boundaries differs. Before the fix these reported
                -- 2 / 3 / 4 — inflated by one per marker.
                Expect.all
                    [ \_ -> Expect.equal "AB" (flatText oneBlock)
                    , \_ -> Expect.equal "AB" (flatText twoBlocks)
                    , \_ -> Expect.equal "AB" (flatText threeBlocks)
                    , \_ -> Expect.equal 2 (maxOffset oneBlock)
                    , \_ -> Expect.equal 2 (maxOffset twoBlocks)
                    , \_ -> Expect.equal 2 (maxOffset threeBlocks)
                    ]
                    ()
        , test "every character offset round-trips in a multi-block document" <|
            \_ ->
                Expect.all
                    [ \_ -> Expect.equal (Just 0) (roundTrip 0 threeBlocks)
                    , \_ -> Expect.equal (Just 1) (roundTrip 1 threeBlocks)
                    , \_ -> Expect.equal (Just 2) (roundTrip 2 threeBlocks)
                    ]
                    ()
        , test "an offset past the end clamps to the last character, not past a marker" <|
            \_ ->
                Expect.equal (Just 2) (roundTrip 9 threeBlocks)
        , test "a caret survives an edit in an EARLIER block, shifting by that edit only" <|
            \_ ->
                -- caret after "B" (offset 2). Typing "xy" into block 0 makes the text
                -- "AxyB", so the same caret is now at offset 4 — it tracked the character
                -- it was anchored to, and the intervening marker didn't inflate the count.
                let
                    caret =
                        Doc.cursorAt bodyPath 2 twoBlocks |> Result.toMaybe

                    edited =
                        Doc.setBlockText bodyPath 0 "Axy" twoBlocks |> ok twoBlocks
                in
                case caret of
                    Just c ->
                        Expect.all
                            [ \_ -> Expect.equal "AxyB" (flatText edited)
                            , \_ -> Expect.equal (Just 4) (Doc.cursorOffset c edited)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "expected a cursor"
        , test "plain (non-rich) text is unaffected: offsets still span every character" <|
            \_ ->
                let
                    d =
                        init |> (\x -> Doc.setRichText bodyPath "hello" x |> ok x)
                in
                Expect.all
                    [ \_ -> Expect.equal 5 (maxOffset d)
                    , \_ -> Expect.equal (Just 3) (roundTrip 3 d)
                    ]
                    ()
        ]
