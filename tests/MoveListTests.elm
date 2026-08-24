module MoveListTests exposing (suite)

{-| Correctness core for movable lists (`Crdt.MoveList`), tested in isolation with
`Int` content — no schema, no op-log — so the move semantics stand on their own
before anything is wired to them.

The properties that matter:

  - a move reorders and **preserves identity** (the value's content travels);
  - **no cycles / no loss** — the failure mode that killed the origin approach;
  - concurrent moves of the _same_ value converge (max-cell-id wins);
  - concurrent moves of _different_ values both apply;
  - application order and re-delivery don't affect the read.

Concurrency is expressed by applying both replicas' operations to the shared
starting list, in each order. That _is_ the merge: cells, values and tombstones are
each grow-only, so either order yields the identical state the removed structural
`MoveList.merge` used to compute — which is also exactly what the op log does when
it replays two peers' ops.

-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as ML exposing (MoveList)
import Crdt.Rga as Rga
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


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


{-| One edit, as the op-log fold would apply it. `Int`s index a small pool of ids so
that moves and deletes routinely name values that exist, and sometimes ones that don't.
-}
type Edit
    = Insert Int Int
    | Move Int Int Int
    | Delete Int
    | Update Int


fuzzEdits : Fuzzer (List Edit)
fuzzEdits =
    Fuzz.listOfLengthBetween 0 12 <|
        Fuzz.oneOf
            [ Fuzz.map2 Insert idIx idIx
            , Fuzz.map3 Move idIx idIx idIx
            , Fuzz.map Delete idIx
            , Fuzz.map Update idIx
            ]


idIx : Fuzzer Int
idIx =
    Fuzz.intRange 0 5


{-| Apply the script. Cell/move ids come from the edit's own index into the pool, so
concurrent-looking id collisions and out-of-order moves both occur.
-}
applyEdits : List Edit -> MoveList String
applyEdits =
    List.foldl
        (\edit ->
            case edit of
                Insert v afterIx ->
                    ML.insert (a v) (anchor afterIx) ("v" ++ String.fromInt v)

                Move cell v afterIx ->
                    ML.move (b cell) (a v) (anchor afterIx)

                Delete v ->
                    ML.delete (a v)

                Update v ->
                    ML.updateValue (a v) (\s -> s ++ "*")
        )
        ML.empty


{-| Index 0 anchors at the head; the rest name a cell that may or may not exist.
-}
anchor : Int -> Maybe OpId
anchor ix =
    if ix == 0 then
        Nothing

    else
        Just (a ix)


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
        , describe "concurrency + convergence laws"
            [ test "re-applying the same move is idempotent" <|
                \_ ->
                    -- ops are re-delivered freely, so a repeat must change nothing
                    let
                        moved =
                            ML.move (a 4) (a 1) (Just (a 3)) abc
                    in
                    Expect.equal moved (ML.move (a 4) (a 1) (Just (a 3)) moved)
            , test "concurrent move of the SAME value converges (max move-cell id wins, both orders equal)" <|
                \_ ->
                    let
                        -- alice moves A to end (a4); bob moves A to head (b9)
                        aliceMove =
                            ML.move (a 4) (a 1) (Just (a 3))

                        bobMove =
                            ML.move (b 9) (a 1) Nothing

                        ab =
                            abc |> aliceMove |> bobMove

                        ba =
                            abc |> bobMove |> aliceMove
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
                            ML.move (a 4) (a 1) (Just (a 3))

                        bobMove =
                            ML.move (b 9) (a 3) Nothing
                    in
                    Expect.equal
                        (ML.toList (abc |> aliceMove |> bobMove))
                        (ML.toList (abc |> bobMove |> aliceMove))
            , test "move + concurrent insert converge" <|
                \_ ->
                    let
                        aliceMove =
                            ML.move (a 4) (a 1) (Just (a 3))

                        bobInsert =
                            ML.insert (b 9) (Just (a 2)) "X"
                    in
                    Expect.equal
                        (ML.toList (abc |> aliceMove |> bobInsert))
                        (ML.toList (abc |> bobInsert |> aliceMove))
            , test "delete-wins over a concurrent move" <|
                \_ ->
                    let
                        aliceDelete =
                            ML.delete (a 1)

                        bobMove =
                            ML.move (b 9) (a 1) Nothing
                    in
                    -- A is deleted on one side, moved on the other: delete wins, both orders agree
                    Expect.all
                        [ \_ -> Expect.equal [ "B", "C" ] (ML.toList (abc |> aliceDelete |> bobMove))
                        , \_ -> Expect.equal [ "B", "C" ] (ML.toList (abc |> bobMove |> aliceDelete))
                        ]
                        ()
            ]
        , fuzz fuzzEdits "every cell is a live right-child, whatever the edits" <|
            \edits ->
                -- The invariant `Crdt.Json` spends on the wire: cells omit `side` and
                -- `deleted` because they are constants, so if any edit sequence can
                -- produce a `Left` or tombstoned cell then encode/decode silently stops
                -- being the identity and values move or vanish across the network.
                -- `Rga.putRight` is what makes it true; this checks it stays true, and
                -- `tests/NodeFuzzTests.elm` covers the other half (that cells shaped
                -- this way do round-trip).
                applyEdits edits
                    |> ML.cells
                    |> Rga.elements
                    |> List.filter (\cell -> cell.side /= Rga.Right || cell.deleted)
                    |> Expect.equalLists []
        ]
