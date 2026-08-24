module FugueTests exposing (suite)

{-| The Fugue ordering guarantee: **concurrent runs inserted at the same position do
not interleave.** This is the merge-quality property RGA lacks and the whole reason
for `design-docs/09-fugue.md`. Convergence (agreement) held under RGA too; what Fugue adds
is that each replica's run stays a contiguous block.

We drive it end-to-end through the public `Crdt.Doc` text API (`setText`) and the
real sync path (`encode`/`decodeInto`), plus a couple of unit-level ordering checks
on `Crdt.Rga` directly for the `side = Left` case that RGA could not express.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id.Internal as Id
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Path as Path exposing (Path)
import Crdt.Rga as Rga
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, test)



-- SCHEMA: a single text field ------------------------------------------------


type alias Sample =
    { body : String }


schema : Crdt S.Nested Sample
schema =
    S.record Sample
        |> S.field "body" .body S.text
        |> S.build


bodyPath : Path
bodyPath =
    Path.root |> Path.field "body"


initDoc : String -> Doc Sample
initDoc name =
    Doc.init (Id.replica name) schema


setBody : String -> Doc Sample -> Doc Sample
setBody s doc =
    Doc.setText bodyPath s doc |> Result.withDefault doc


body : Doc Sample -> String
body doc =
    Doc.read doc |> Result.map .body |> Result.withDefault "<err>"


{-| Merge `from` fully into `to` (both directions makes convergence checks trivial).
-}
mergeIn : Doc Sample -> Doc Sample -> Doc Sample
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to



-- RUN INTERLEAVING HELPERS ---------------------------------------------------


{-| Is `s` free of interleaving between two runs made of the chars `xs` and `ys`?
i.e. does it consist of one whole run entirely before the other — `xs ++ ys` or
`ys ++ xs` (allowing for a shared prefix/suffix, which we don't have here)? We test
the exact block orderings, since with distinct alphabets that's the definition.
-}
isBlockOrdered : String -> String -> String -> Bool
isBlockOrdered xs ys result =
    result == (xs ++ ys) || result == (ys ++ xs)


{-| The number of maximal same-run blocks in `result`, classifying each char as
belonging to run A (in `xs`) or run B (in `ys`). Interleaving shows up as > 2
blocks. Contiguous runs give exactly 2 (or 1 if a run is empty).
-}
blockCount : String -> String -> String -> Int
blockCount xs ys result =
    let
        classify c =
            if String.any ((==) c) xs then
                "A"

            else if String.any ((==) c) ys then
                "B"

            else
                "?"

        step c ( count, prev ) =
            let
                cls =
                    classify c
            in
            if cls == prev then
                ( count, prev )

            else
                ( count + 1, cls )
    in
    String.toList result
        |> List.foldl step ( 0, "" )
        |> Tuple.first



-- SUITE ----------------------------------------------------------------------


suite : Test
suite =
    describe "Fugue ordering (no same-position interleaving)"
        [ describe "the headline property — concurrent runs stay contiguous"
            [ test "two peers type distinct runs into an empty doc; merge both ways" <|
                \_ ->
                    let
                        base =
                            initDoc "seed"

                        -- alice and bob are genuinely separate replicas sharing `base`
                        alice =
                            mergeIn base (initDoc "alice") |> setBody "aaa"

                        bob =
                            mergeIn base (initDoc "bob") |> setBody "bbb"

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ -- convergence
                          \_ -> Expect.equal (body ab) (body ba)

                        -- contiguity: one whole run before the other, never interleaved
                        , \_ ->
                            Expect.equal True
                                (isBlockOrdered "aaa" "bbb" (body ab))
                        , \_ -> Expect.equal 2 (blockCount "aaa" "bbb" (body ab))
                        ]
                        ()
            , test "concurrent runs inserted in the MIDDLE of shared text stay contiguous" <|
                \_ ->
                    let
                        -- shared "XY"; both insert a run between X and Y
                        base =
                            initDoc "seed" |> setBody "XY"

                        alice =
                            mergeIn base (initDoc "alice") |> setBody "XaaaY"

                        bob =
                            mergeIn base (initDoc "bob") |> setBody "XbbbY"

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (body ab) (body ba)

                        -- must be X, then one whole run, then the other, then Y
                        , \_ ->
                            Expect.equal True
                                (body ab == "XaaabbbY" || body ab == "XbbbaaaY")
                        ]
                        ()
            , test "a peer editing INSIDE another's run anchors at a derived char id (run-length safety)" <|
                \_ ->
                    -- alice types "abcde" as ONE run-length op; its chars have derived ids
                    -- start..start+4. Bob receives it, then inserts inside it (between c
                    -- and d). Bob's insert anchors at the id of 'c' = alice's start+2, an
                    -- id the run explosion materialised. It must land correctly and
                    -- converge — the crux of storing runs but addressing chars.
                    let
                        alice =
                            initDoc "alice" |> setBody "abcde"

                        bob =
                            mergeIn alice (initDoc "bob") |> setBody "abcXde"

                        -- alice also edits concurrently elsewhere (prepend), to make the
                        -- mid-run anchor survive a genuine concurrent merge
                        alice2 =
                            setBody "Zabcde" alice

                        ab =
                            mergeIn bob alice2

                        ba =
                            mergeIn alice2 bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (body ab) (body ba)
                        , \_ -> Expect.equal "ZabcXde" (body ab)
                        ]
                        ()
            , test "concurrent inserts at the SAME mid-run char converge without interleaving" <|
                \_ ->
                    -- both peers receive alice's run "abcde", then both insert a distinct
                    -- run right after 'c' (same derived anchor id). Contiguity + convergence.
                    let
                        seed =
                            initDoc "alice" |> setBody "abcde"

                        p1 =
                            mergeIn seed (initDoc "p1") |> setBody "abcXXde"

                        p2 =
                            mergeIn seed (initDoc "p2") |> setBody "abcYYde"

                        ab =
                            mergeIn p2 p1

                        ba =
                            mergeIn p1 p2
                    in
                    Expect.all
                        [ \_ -> Expect.equal (body ab) (body ba)
                        , \_ -> Expect.equal True (body ab == "abcXXYYde" || body ab == "abcYYXXde")
                        ]
                        ()
            , fuzz2 runFuzz runFuzz "fuzzed: any two concurrent runs converge and never interleave" <|
                \runA runB ->
                    let
                        -- distinct alphabets so classification is unambiguous
                        a =
                            String.map (\_ -> 'a') runA |> String.left 6

                        b =
                            String.map (\_ -> 'b') runB |> String.left 6

                        base =
                            initDoc "seed"

                        alice =
                            mergeIn base (initDoc "alice") |> setBody a

                        bob =
                            mergeIn base (initDoc "bob") |> setBody b

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ \_ -> Expect.equal (body ab) (body ba)
                        , \_ ->
                            -- at most 2 blocks: the two runs never split each other up
                            Expect.atMost 2 (blockCount a b (body ab))
                        ]
                        ()
            ]
        , describe "sequential edits behave like plain text"
            [ test "append builds left-to-right" <|
                \_ ->
                    Expect.equal "hello" (body (setBody "hello" (initDoc "a")))
            , test "insert in the middle lands in the right place" <|
                \_ ->
                    let
                        d =
                            initDoc "a" |> setBody "ac" |> setBody "abc"
                    in
                    Expect.equal "abc" (body d)
            , test "a run typed by one replica is contiguous with a later concurrent run" <|
                \_ ->
                    -- non-concurrent baseline: alice types, bob (after syncing) types
                    let
                        alice =
                            initDoc "alice" |> setBody "hello"

                        bob =
                            mergeIn alice (initDoc "bob") |> setBody "hello world"
                    in
                    Expect.equal "hello world" (body (mergeIn bob alice))
            ]
        , describe "the mechanism actually distinguishes (would interleave without Left)"
            [ test "two runs as right-children of the SAME parent interleave (the RGA situation)" <|
                \_ ->
                    -- Contrast: if both concurrent runs anchored right-of X with a
                    -- single left anchor (classic RGA), they interleave. This is the
                    -- exact situation `applyTextDiff` avoids by attaching the second
                    -- run's first char as a `Left` child of the right neighbor — so
                    -- this test pins that the non-interleaving above is earned by the
                    -- Left/Right placement, not an accident of the traversal.
                    let
                        x =
                            Id.opId 1 (Id.replica "x")

                        a1 =
                            Id.opId 2 (Id.replica "alice")

                        a2 =
                            Id.opId 4 (Id.replica "alice")

                        b1 =
                            Id.opId 3 (Id.replica "bob")

                        b2 =
                            Id.opId 5 (Id.replica "bob")

                        rga =
                            Rga.empty
                                |> Rga.put (charEl x Nothing Rga.Right 'X')
                                |> Rga.put (charEl a1 (Just x) Rga.Right 'a')
                                |> Rga.put (charEl a2 (Just a1) Rga.Right 'a')
                                |> Rga.put (charEl b1 (Just x) Rga.Right 'b')
                                |> Rga.put (charEl b2 (Just b1) Rga.Right 'b')
                    in
                    Expect.greaterThan 2 (blockCount "aa" "bb" (textOf rga))
            ]
        , describe "Rga ordering unit checks (the side = Left case)"
            [ test "a left-child renders immediately before its parent" <|
                \_ ->
                    -- parent P (a root), then a left-child L of P → order is L, P
                    let
                        p =
                            Id.opId 1 (Id.replica "a")

                        l =
                            Id.opId 2 (Id.replica "a")

                        rga =
                            Rga.empty
                                |> Rga.put (charEl p Nothing Rga.Right 'P')
                                |> Rga.put (charEl l (Just p) Rga.Left 'L')
                    in
                    Expect.equal "LP" (textOf rga)
            , test "a right-child renders immediately after its parent" <|
                \_ ->
                    let
                        p =
                            Id.opId 1 (Id.replica "a")

                        r =
                            Id.opId 2 (Id.replica "a")

                        rga =
                            Rga.empty
                                |> Rga.put (charEl p Nothing Rga.Right 'P')
                                |> Rga.put (charEl r (Just p) Rga.Right 'R')
                    in
                    Expect.equal "PR" (textOf rga)
            , test "left-child and right-child straddle the parent" <|
                \_ ->
                    let
                        p =
                            Id.opId 1 (Id.replica "a")

                        l =
                            Id.opId 2 (Id.replica "a")

                        r =
                            Id.opId 3 (Id.replica "a")

                        rga =
                            Rga.empty
                                |> Rga.put (charEl p Nothing Rga.Right 'P')
                                |> Rga.put (charEl l (Just p) Rga.Left 'L')
                                |> Rga.put (charEl r (Just p) Rga.Right 'R')
                    in
                    Expect.equal "LPR" (textOf rga)
            ]
        ]



-- RGA UNIT HELPERS -----------------------------------------------------------


charEl : Id.OpId -> Maybe Id.OpId -> Rga.Side -> Char -> Rga.Element Node
charEl id parent side c =
    Rga.element id parent side (Node.reg (PString (String.fromChar c)) id) False


textOf : Rga.Rga Node -> String
textOf rga =
    Rga.toList rga
        |> List.filterMap
            (\n ->
                case Node.asPrim n of
                    Just (PString s) ->
                        Just s

                    _ ->
                        Nothing
            )
        |> String.concat


runFuzz : Fuzzer String
runFuzz =
    Fuzz.intRange 1 6 |> Fuzz.map (\n -> String.repeat n "z")
