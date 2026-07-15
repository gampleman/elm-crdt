module MoveListTests exposing (suite)

{-| Correctness core for movable lists (`Crdt.MoveList`), tested in isolation with
`Int` content — no schema, no op-log — so the move/merge semantics stand on their
own before anything is wired to them.

The properties that matter:

  - a move reorders and **preserves identity** (the value's content travels);
  - **no cycles / no loss** — the failure mode that killed the origin approach;
  - concurrent moves of the _same_ value converge (max-cell-id wins);
  - concurrent moves of _different_ values both apply;
  - merge is commutative & idempotent on the read.

-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as ML exposing (MoveList)
import Expect
import Test exposing (Test, describe, test)


a : Int -> OpId
a n =
    Id.opId n (Id.replica "alice")


b : Int -> OpId
b n =
    Id.opId n (Id.replica "bob")


{-| Build a list a1=>"A", a2=>"B", a3=>"C" appended in order (Int content via
char codes would be noisy; use the counter as the value).
-}
abc : MoveList String
abc =
    ML.empty
        |> ML.insert (a 1) Nothing "A"
        |> ML.insert (a 2) (Just (a 1)) "B"
        |> ML.insert (a 3) (Just (a 2)) "C"


suite : Test
suite =
    describe "MoveList"
        [ test "appended values read in order" <|
            \_ ->
                ML.toList abc |> Expect.equal [ "A", "B", "C" ]
        , test "move the head to the end reorders correctly (the case that cycled before)" <|
            \_ ->
                -- move A (value a1) to after C (cell a3)
                let
                    moved =
                        ML.move (a 4) (a 1) (Just (a 3)) abc
                in
                ML.toList moved |> Expect.equal [ "B", "C", "A" ]
        , test "move preserves identity: a value edited then moved keeps its content" <|
            \_ ->
                let
                    edited =
                        abc |> ML.updateValue (a 1) (\_ -> "A*")

                    movedThenRead =
                        ML.move (a 4) (a 1) (Just (a 3)) edited |> ML.toList
                in
                movedThenRead |> Expect.equal [ "B", "C", "A*" ]
        , test "move to the head" <|
            \_ ->
                -- move C (value a3) to the head (after Nothing)
                ML.move (a 4) (a 3) Nothing abc
                    |> ML.toList
                    |> Expect.equal [ "C", "A", "B" ]
        , test "no cycle / no loss: every value appears exactly once after a move" <|
            \_ ->
                let
                    moved =
                        ML.move (a 4) (a 1) (Just (a 3)) abc
                in
                ML.toList moved |> List.length |> Expect.equal 3
        , test "a second move of the same value supersedes the first (max cell wins)" <|
            \_ ->
                -- move A to end (cell a4), then move A to head (cell a5); a5 wins
                let
                    twice =
                        abc
                            |> ML.move (a 4) (a 1) (Just (a 3))
                            |> ML.move (a 5) (a 1) Nothing
                in
                ML.toList twice |> Expect.equal [ "A", "B", "C" ]
        , test "delete removes a value from the order" <|
            \_ ->
                ML.delete (a 2) abc |> ML.toList |> Expect.equal [ "A", "C" ]
        , describe "merge"
            [ test "idempotent on the read" <|
                \_ ->
                    Expect.equal (ML.toList abc) (ML.toList (ML.merge always abc abc))
            , test "concurrent move of the SAME value converges (max move-cell id wins, both orders equal)" <|
                \_ ->
                    let
                        -- alice moves A to end (a4); bob moves A to head (b9)
                        aliceMove =
                            ML.move (a 4) (a 1) (Just (a 3)) abc

                        bobMove =
                            ML.move (b 9) (a 1) Nothing abc

                        ab =
                            ML.merge always aliceMove bobMove

                        ba =
                            ML.merge always bobMove aliceMove
                    in
                    Expect.all
                        [ \_ -> Expect.equal (ML.toList ab) (ML.toList ba)
                        , -- b9 > a4, so bob's "A to head" wins
                          \_ -> Expect.equal [ "A", "B", "C" ] (ML.toList ab)
                        ]
                        ()
            , test "concurrent move of DIFFERENT values: both apply, order is deterministic" <|
                \_ ->
                    let
                        -- alice moves A to end; bob moves C to head
                        aliceMove =
                            ML.move (a 4) (a 1) (Just (a 3)) abc

                        bobMove =
                            ML.move (b 9) (a 3) Nothing abc
                    in
                    Expect.equal
                        (ML.toList (ML.merge always aliceMove bobMove))
                        (ML.toList (ML.merge always bobMove aliceMove))
            , test "move + concurrent insert converge" <|
                \_ ->
                    let
                        aliceMove =
                            ML.move (a 4) (a 1) (Just (a 3)) abc

                        bobInsert =
                            ML.insert (b 9) (Just (a 2)) "X" abc
                    in
                    Expect.equal
                        (ML.toList (ML.merge always aliceMove bobInsert))
                        (ML.toList (ML.merge always bobInsert aliceMove))
            , test "delete-wins over a concurrent move" <|
                \_ ->
                    let
                        aliceDelete =
                            ML.delete (a 1) abc

                        bobMove =
                            ML.move (b 9) (a 1) Nothing abc
                    in
                    -- A is deleted on one side, moved on the other: delete wins, both orders agree
                    Expect.all
                        [ \_ -> Expect.equal [ "B", "C" ] (ML.toList (ML.merge always aliceDelete bobMove))
                        , \_ -> Expect.equal [ "B", "C" ] (ML.toList (ML.merge always bobMove aliceDelete))
                        ]
                        ()
            ]
        ]
