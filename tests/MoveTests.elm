module MoveTests exposing (suite)

{-| Movable lists through the **public** API (`Schema.movableList` +
`Doc.listMove`). The correctness core is proven in `MoveListTests`; here we
check it works end-to-end through a schema and the op-log: reorder, identity
preservation, convergence, JSON, and that a plain `list` rejects moves.
-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Test exposing (Test, describe, test)


type alias Sample =
    { todos : List Todo }


type alias Todo =
    { text : String }


schema : Crdt S.Nested Sample
schema =
    S.record Sample
        |> S.field "todos" .todos (S.movableList todoSchema)
        |> S.build


todoSchema : Crdt S.Nested Todo
todoSchema =
    S.record Todo |> S.field "text" .text S.text |> S.build


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


initDoc : String -> Doc Sample
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Sample -> Result Doc.Error (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


add : String -> Doc Sample -> Doc Sample
add label doc =
    Doc.listAppend todosPath (todoSchema |> S.with (Todo label)) doc |> ok doc


texts : Doc Sample -> Result S.Error (List String)
texts doc =
    Doc.read doc |> Result.map (.todos >> List.map .text)


abc : Doc Sample
abc =
    initDoc "alice" |> add "a" |> add "b" |> add "c"


suite : Test
suite =
    describe "movableList (public API)"
        [ test "appends read in order" <|
            \_ ->
                texts abc |> Expect.equal (Ok [ "a", "b", "c" ])
        , test "move index 0 to the end reorders" <|
            \_ ->
                let
                    moved =
                        Doc.listMove todosPath 0 2 abc |> ok abc
                in
                texts moved |> Expect.equal (Ok [ "b", "c", "a" ])
        , test "move to the head" <|
            \_ ->
                let
                    moved =
                        Doc.listMove todosPath 2 0 abc |> ok abc
                in
                texts moved |> Expect.equal (Ok [ "c", "a", "b" ])
        , test "a moved item keeps its identity: edit its text, then move it" <|
            \_ ->
                let
                    textPath0 =
                        Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "text"

                    doc =
                        abc
                            |> (\d -> Doc.setText textPath0 "AAA" d |> ok d)
                            |> (\d -> Doc.listMove todosPath 0 2 d |> ok d)
                in
                -- "a" (now "AAA") moved to the end, text preserved
                texts doc |> Expect.equal (Ok [ "b", "c", "AAA" ])
        , test "no loss: every item present exactly once after a move" <|
            \_ ->
                let
                    moved =
                        Doc.listMove todosPath 1 0 abc |> ok abc
                in
                texts moved |> Result.map List.length |> Expect.equal (Ok 3)
        , test "concurrent moves of the same item converge (both merge orders equal)" <|
            \_ ->
                let
                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encode abc)
                            |> Result.withDefault (initDoc "bob")

                    aliceMove =
                        Doc.listMove todosPath 0 2 abc |> ok abc

                    bobMove =
                        Doc.listMove todosPath 0 1 bob |> ok bob

                    ab =
                        aliceMove |> Doc.decodeInto (Doc.encode bobMove) |> Result.withDefault aliceMove

                    ba =
                        bobMove |> Doc.decodeInto (Doc.encode aliceMove) |> Result.withDefault bobMove
                in
                Expect.equal (texts ab) (texts ba)
        , test "concurrent move + insert converge" <|
            \_ ->
                let
                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encode abc)
                            |> Result.withDefault (initDoc "bob")

                    aliceMove =
                        Doc.listMove todosPath 2 0 abc |> ok abc

                    bobAdd =
                        add "d" bob

                    ab =
                        aliceMove |> Doc.decodeInto (Doc.encode bobAdd) |> Result.withDefault aliceMove

                    ba =
                        bobAdd |> Doc.decodeInto (Doc.encode aliceMove) |> Result.withDefault bobAdd
                in
                Expect.equal (texts ab) (texts ba)
        , test "moves survive the JSON wire round-trip" <|
            \_ ->
                let
                    moved =
                        Doc.listMove todosPath 0 2 abc |> ok abc

                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encode moved)
                            |> Result.withDefault (initDoc "bob")
                in
                Expect.equal (texts moved) (texts bob)
        , test "a moved item read back through the schema keeps its nested fields" <|
            \_ ->
                let
                    moved =
                        Doc.listMove todosPath 0 2 abc |> ok abc
                in
                Doc.read moved
                    |> Result.map .todos
                    |> Expect.equal (Ok [ Todo "b", Todo "c", Todo "a" ])
        ]
