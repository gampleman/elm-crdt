module Crdt.Frac exposing
    ( Frac
    , between, compare, toList, fromList
    )

{-| A **fractional index**: a dense, totally-ordered key you can always allocate a
new value _between_ two existing ones (or before/after the ends). Used by
`Crdt.Tree` to order siblings under a parent, so a node can be reordered without
referencing its neighbours (a reference-based "after sibling X" scheme would let
concurrent reorders form a cycle, the very thing `Crdt.MoveList` avoids — a
position _value_ cannot).

A `Frac` is a non-empty list of digits in base `base`, read as the fraction
`0.d1 d2 …` in `(0, 1)`. Order is numeric, which equals lexicographic order on the
digit lists (a shorter list is padded with the minimum digit). `between` returns a
key strictly between its bounds; concurrent allocations against the _same_ bounds
produce the _same_ key, so callers break ties by a stable id (e.g. the node's
`OpId`) — the standard fractional-index trade-off.

This module is package-internal (not exposed). Its one invariant —
`a < between (Just a) (Just b) < b` — is fuzz-tested in `tests/FracTests.elm`.

@docs Frac
@docs between, compare, toList, fromList

-}


{-| The digit base. 10 keeps keys short and human-readable in JSON.
-}
base : Int
base =
    10


{-| A fractional index: digits of `0.d1 d2 …` (each in `[0, base)`), non-empty.
-}
type Frac
    = Frac (List Int)


{-| Build from raw digits (used by JSON decode). Empties become the midpoint.
-}
fromList : List Int -> Frac
fromList digits =
    case digits of
        [] ->
            Frac [ base // 2 ]

        _ ->
            Frac digits


{-| The raw digits (for JSON encode).
-}
toList : Frac -> List Int
toList (Frac digits) =
    digits


{-| Numeric order (== lexicographic on digits, shorter padded with 0).
-}
compare : Frac -> Frac -> Order
compare (Frac a) (Frac b) =
    compareDigits a b


compareDigits : List Int -> List Int -> Order
compareDigits a b =
    case ( a, b ) of
        ( [], [] ) ->
            EQ

        ( [], y :: ys ) ->
            -- a exhausted: treat as trailing zeros (0.x00… vs 0.x…)
            case Basics.compare 0 y of
                EQ ->
                    compareDigits [] ys

                other ->
                    other

        ( x :: xs, [] ) ->
            case Basics.compare x 0 of
                EQ ->
                    compareDigits xs []

                other ->
                    other

        ( x :: xs, y :: ys ) ->
            case Basics.compare x y of
                EQ ->
                    compareDigits xs ys

                other ->
                    other


{-| A key strictly between the two bounds. `Nothing` on a side means "the open
end" — `between Nothing (Just b)` is some key in `(0, b)`, `between (Just a)
Nothing` is some key in `(a, 1)`, `between Nothing Nothing` is the midpoint.

Deterministic: identical bounds → identical result (callers tiebreak by id).

-}
between : Maybe Frac -> Maybe Frac -> Frac
between lo hi =
    Frac (mid (Maybe.map toList lo) (Maybe.map toList hi))


{-| The digit-by-digit midpoint. `lo` absent = the constant 0 stream (the low open
end); `hi` absent = the constant `base` stream (the high open end).
-}
mid : Maybe (List Int) -> Maybe (List Int) -> List Int
mid lo hi =
    let
        ( l, _ ) =
            uncons lo 0

        ( h, _ ) =
            uncons hi base
    in
    if l == h then
        -- same leading digit: keep it and recurse into the remaining fractions
        l :: mid (Just (rest lo)) (Just (rest hi))

    else if l + 1 < h then
        -- room between the digits: take their midpoint and stop
        [ (l + h) // 2 ]

    else
        -- l + 1 == h: no gap here, descend on the low side toward `base`
        -- (i.e. pick a digit above `l`'s remaining fraction)
        l :: mid (Just (rest lo)) Nothing


{-| Head of the stream, using `deflt` when the list is empty/absent.
-}
uncons : Maybe (List Int) -> Int -> ( Int, List Int )
uncons m deflt =
    case m of
        Just (x :: xs) ->
            ( x, xs )

        _ ->
            ( deflt, [] )


rest : Maybe (List Int) -> List Int
rest m =
    case m of
        Just (_ :: xs) ->
            xs

        _ ->
            []
