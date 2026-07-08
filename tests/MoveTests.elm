module MoveTests exposing (suite)

{-| Movable lists through the **public** API (`Schema.movableList` +
`OpDoc.listMove`). The correctness core is proven in `MoveListTests`; here we
check it works end-to-end through a schema and the op-log: reorder, identity
preservation, convergence, JSON, and that a plain `list` rejects moves.
-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Test exposing (Test, describe, test)


type alias Doc =
    { todos : List Todo }


type alias Todo =
    { text : String }


schema : Crdt S.Nested Doc
schema =
    S.record Doc
        |> S.field "todos" .todos (S.movableList todoSchema)
        |> S.build


todoSchema : Crdt S.Nested Todo
todoSchema =
    S.record Todo |> S.field "text" .text S.text |> S.build


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


initDoc : String -> OpDoc Doc
initDoc name =
    OpDoc.init (Id.replica name) schema


ok : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
ok fb =
    Result.withDefault fb


add : String -> OpDoc Doc -> OpDoc Doc
add label doc =
    OpDoc.listAppend todosPath (todoSchema |> S.with (Todo label)) doc |> ok doc


texts : OpDoc Doc -> Result S.Error (List String)
texts doc =
    OpDoc.read doc |> Result.map (.todos >> List.map .text)


abc : OpDoc Doc
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
                        OpDoc.listMove todosPath 0 2 abc |> ok abc
                in
                texts moved |> Expect.equal (Ok [ "b", "c", "a" ])
        , test "move to the head" <|
            \_ ->
                let
                    moved =
                        OpDoc.listMove todosPath 2 0 abc |> ok abc
                in
                texts moved |> Expect.equal (Ok [ "c", "a", "b" ])
        , test "a moved item keeps its identity: edit its text, then move it" <|
            \_ ->
                let
                    textPath0 =
                        Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "text"

                    doc =
                        abc
                            |> (\d -> OpDoc.setText textPath0 "AAA" d |> ok d)
                            |> (\d -> OpDoc.listMove todosPath 0 2 d |> ok d)
                in
                -- "a" (now "AAA") moved to the end, text preserved
                texts doc |> Expect.equal (Ok [ "b", "c", "AAA" ])
        , test "no loss: every item present exactly once after a move" <|
            \_ ->
                let
                    moved =
                        OpDoc.listMove todosPath 1 0 abc |> ok abc
                in
                texts moved |> Result.map List.length |> Expect.equal (Ok 3)
        , test "concurrent moves of the same item converge (both merge orders equal)" <|
            \_ ->
                let
                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode abc)
                            |> Result.withDefault (initDoc "bob")

                    aliceMove =
                        OpDoc.listMove todosPath 0 2 abc |> ok abc

                    bobMove =
                        OpDoc.listMove todosPath 0 1 bob |> ok bob

                    ab =
                        aliceMove |> OpDoc.decodeInto (OpDoc.encode bobMove) |> Result.withDefault aliceMove

                    ba =
                        bobMove |> OpDoc.decodeInto (OpDoc.encode aliceMove) |> Result.withDefault bobMove
                in
                Expect.equal (texts ab) (texts ba)
        , test "concurrent move + insert converge" <|
            \_ ->
                let
                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode abc)
                            |> Result.withDefault (initDoc "bob")

                    aliceMove =
                        OpDoc.listMove todosPath 2 0 abc |> ok abc

                    bobAdd =
                        add "d" bob

                    ab =
                        aliceMove |> OpDoc.decodeInto (OpDoc.encode bobAdd) |> Result.withDefault aliceMove

                    ba =
                        bobAdd |> OpDoc.decodeInto (OpDoc.encode aliceMove) |> Result.withDefault bobAdd
                in
                Expect.equal (texts ab) (texts ba)
        , test "moves survive the JSON wire round-trip" <|
            \_ ->
                let
                    moved =
                        OpDoc.listMove todosPath 0 2 abc |> ok abc

                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode moved)
                            |> Result.withDefault (initDoc "bob")
                in
                Expect.equal (texts moved) (texts bob)
        , test "a moved item read back through the schema keeps its nested fields" <|
            \_ ->
                let
                    moved =
                        OpDoc.listMove todosPath 0 2 abc |> ok abc
                in
                OpDoc.read moved
                    |> Result.map .todos
                    |> Expect.equal (Ok [ Todo "b", Todo "c", Todo "a" ])
        ]
