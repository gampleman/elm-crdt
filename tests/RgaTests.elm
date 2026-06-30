module RgaTests exposing (suite)

{-| RGA-specific correctness: deterministic ordering independent of merge order,
tombstone semantics, and the classic concurrent-insert-at-same-origin case.
-}

import Crdt.Id as Id exposing (OpId)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Expect
import Test exposing (Test, describe, test)


alice : String -> Int -> OpId
alice _ n =
    Id.opId n (Id.replica "alice")


bob : String -> Int -> OpId
bob _ n =
    Id.opId n (Id.replica "bob")


{-| A single-character text element keyed by an OpId, no origin (head insert).
-}
charEl : OpId -> Maybe OpId -> Char -> Rga.Element Node
charEl id origin c =
    Rga.element id origin (Node.reg (PString (String.fromChar c)) id) False


{-| Merge two node-RGAs, recursing into element content via Node.merge.
-}
merge : Rga.Rga Node -> Rga.Rga Node -> Rga.Rga Node
merge =
    Rga.merge Node.merge


textOf : Rga.Rga Node -> String
textOf rga =
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


suite : Test
suite =
    describe "Rga"
        [ test "merge order does not affect resulting order" <|
            \_ ->
                let
                    -- alice types "Hi", bob types "Yo", both at the head, concurrently
                    aliceRga =
                        Rga.fromElements
                            [ charEl (alice "" 1) Nothing 'H'
                            , charEl (alice "" 2) (Just (alice "" 1)) 'i'
                            ]

                    bobRga =
                        Rga.fromElements
                            [ charEl (bob "" 1) Nothing 'Y'
                            , charEl (bob "" 2) (Just (bob "" 1)) 'o'
                            ]
                in
                Expect.equal
                    (textOf (merge aliceRga bobRga))
                    (textOf (merge bobRga aliceRga))
        , test "deleted elements are tombstoned, not resurrected by merge" <|
            \_ ->
                let
                    full =
                        Rga.fromElements [ charEl (alice "" 1) Nothing 'A' ]

                    deleted =
                        Rga.delete (alice "" 1) full
                in
                -- merging a replica that still sees 'A' with one that deleted it
                -- must keep it deleted (tombstone wins), in both directions
                Expect.all
                    [ \_ -> Expect.equal "" (textOf (merge full deleted))
                    , \_ -> Expect.equal "" (textOf (merge deleted full))
                    ]
                    ()
        , test "concurrent inserts at the same origin converge to a deterministic order" <|
            \_ ->
                let
                    -- both insert a char right after X, concurrently
                    aliceIns =
                        Rga.fromElements
                            [ charEl (alice "" 1) Nothing 'X'
                            , charEl (alice "" 5) (Just (alice "" 1)) 'a'
                            ]

                    bobIns =
                        Rga.fromElements
                            [ charEl (alice "" 1) Nothing 'X'
                            , charEl (bob "" 5) (Just (alice "" 1)) 'b'
                            ]
                in
                Expect.equal
                    (textOf (merge aliceIns bobIns))
                    (textOf (merge bobIns aliceIns))
        , test "idempotent: merging an Rga with itself is identity" <|
            \_ ->
                let
                    rga =
                        Rga.fromElements
                            [ charEl (alice "" 1) Nothing 'A'
                            , charEl (alice "" 2) (Just (alice "" 1)) 'B'
                            ]
                in
                Expect.equal rga (merge rga rga)
        , test "long origin-chain orders without stack overflow (regression)" <|
            \_ ->
                -- A list built by appending forms a linear origin-chain of depth
                -- N. The ordering walk must be iterative, not N-deep recursion —
                -- this used to overflow the stack around a few thousand elements.
                let
                    n =
                        20000

                    chain =
                        List.range 1 n
                            |> List.map
                                (\i ->
                                    charEl (alice "" i)
                                        (if i == 1 then
                                            Nothing

                                         else
                                            Just (alice "" (i - 1))
                                        )
                                        'x'
                                )
                in
                Rga.fromElements chain
                    |> Rga.toList
                    |> List.length
                    |> Expect.equal n
        ]
