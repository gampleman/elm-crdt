module FracTests exposing (suite)

{-| The fractional-index allocator (`Crdt.Frac`), fuzzed on its one invariant:
`between` always returns a key strictly between its bounds, so siblings can be
ordered densely without limit. If this holds, `Crdt.Tree`'s sibling ordering is
sound.
-}

import Crdt.Frac as Frac exposing (Frac)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, fuzz2, test)


lt : Frac -> Frac -> Bool
lt a b =
    Frac.compare a b == LT


{-| A fuzzed **valid** `Frac`: a value strictly in the open interval (0, 1). We
append a `1` digit so the value is never exactly 0 (`[0]`, `[0,0]`, … all equal 0,
the low open boundary, which is not a representable key — nothing sorts below it).
`between` never _produces_ a value-0 key from valid bounds; this keeps the fuzzed
_inputs_ valid too.
-}
fracFuzz : Fuzzer Frac
fracFuzz =
    Fuzz.listOfLengthBetween 0 5 (Fuzz.intRange 0 9)
        |> Fuzz.map (\ds -> Frac.fromList (ds ++ [ 1 ]))


{-| An ordered pair of _distinct_ fracs `(lo, hi)` with `lo < hi`, for the
between-two-bounds tests.
-}
orderedPair : Fuzzer ( Frac, Frac )
orderedPair =
    Fuzz.pair fracFuzz fracFuzz
        |> Fuzz.map
            (\( a, b ) ->
                case Frac.compare a b of
                    LT ->
                        ( a, b )

                    GT ->
                        ( b, a )

                    EQ ->
                        -- collapse equal draws to a known-distinct pair
                        ( Frac.fromList [ 1 ], Frac.fromList [ 8 ] )
            )


suite : Test
suite =
    describe "Crdt.Frac — fractional index"
        [ describe "between is strictly between its bounds"
            [ fuzz orderedPair "between two distinct keys lands strictly between" <|
                \( lo, hi ) ->
                    let
                        m =
                            Frac.between (Just lo) (Just hi)
                    in
                    Expect.equal ( True, True ) ( lt lo m, lt m hi )
            , fuzz fracFuzz "between Nothing and a key is strictly below it" <|
                \hi ->
                    Frac.between Nothing (Just hi)
                        |> (\m -> lt m hi)
                        |> Expect.equal True
            , fuzz fracFuzz "between a key and Nothing is strictly above it" <|
                \lo ->
                    Frac.between (Just lo) Nothing
                        |> (\m -> lt lo m)
                        |> Expect.equal True
            ]
        , describe "density: you can always keep inserting between"
            [ fuzz2 orderedPair (Fuzz.intRange 1 40) "N repeated left-inserts stay ordered and distinct" <|
                \( lo, hi ) n ->
                    -- repeatedly insert between lo and the last inserted key;
                    -- every step must stay strictly > lo and < previous hi
                    let
                        step _ ( prevHi, okSoFar ) =
                            let
                                m =
                                    Frac.between (Just lo) (Just prevHi)
                            in
                            ( m, okSoFar && lt lo m && lt m prevHi )
                    in
                    List.range 1 n
                        |> List.foldl step ( hi, True )
                        |> Tuple.second
                        |> Expect.equal True
            , fuzz2 orderedPair (Fuzz.intRange 1 40) "N repeated right-inserts stay ordered and distinct" <|
                \( lo, hi ) n ->
                    let
                        step _ ( prevLo, okSoFar ) =
                            let
                                m =
                                    Frac.between (Just prevLo) (Just hi)
                            in
                            ( m, okSoFar && lt prevLo m && lt m hi )
                    in
                    List.range 1 n
                        |> List.foldl step ( lo, True )
                        |> Tuple.second
                        |> Expect.equal True
            ]
        , describe "determinism + order basics"
            [ fuzz orderedPair "between is deterministic (same bounds → same key)" <|
                \( lo, hi ) ->
                    Expect.equal
                        (Frac.between (Just lo) (Just hi) |> Frac.toList)
                        (Frac.between (Just lo) (Just hi) |> Frac.toList)
            , test "compare orders by numeric value, not length" <|
                \_ ->
                    -- 0.5 > 0.49 (shorter isn't smaller)
                    Frac.compare (Frac.fromList [ 5 ]) (Frac.fromList [ 4, 9 ])
                        |> Expect.equal GT
            , fuzz fracFuzz "compare is reflexive (EQ with itself)" <|
                \a ->
                    Frac.compare a a |> Expect.equal EQ
            ]
        ]
