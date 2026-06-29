module SchemaTests exposing (suite)

{-| The typed codec layer: building an empty doc reads back the empty typed
value, and edits are reflected through `read`.
-}

import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Path as Path
import Crdt.Schema as S
import Dict
import Expect
import Helpers exposing (Board, Todo, boardSchema, todoSchema)
import Test exposing (Test, describe, test)


base : Doc
base =
    Crdt.init (Id.replica "alice") boardSchema


suite : Test
suite =
    describe "Schema codec"
        [ test "freshly initialized doc reads as the empty typed value" <|
            \_ ->
                Crdt.read boardSchema base
                    |> Expect.equal (Ok (Board "" [] Dict.empty))
        , test "setText is reflected through read" <|
            \_ ->
                case Edit.setText (Path.root |> Path.field "title") "Hello" base of
                    Ok doc ->
                        Crdt.read boardSchema doc
                            |> Result.map .title
                            |> Expect.equal (Ok "Hello")

                    Err _ ->
                        Expect.fail "edit failed"
        , test "dict set/read roundtrips a key" <|
            \_ ->
                let
                    notesPath =
                        Path.root |> Path.field "notes"
                in
                case Edit.setKey notesPath "groceries" (S.text |> S.with "milk") base of
                    Ok doc ->
                        Crdt.read boardSchema doc
                            |> Result.map (.notes >> Dict.get "groceries")
                            |> Expect.equal (Ok (Just "milk"))

                    Err _ ->
                        Expect.fail "setKey failed"
        , test "dict removeKey makes the key absent on read (LWW tombstone)" <|
            \_ ->
                let
                    notesPath =
                        Path.root |> Path.field "notes"

                    withKey =
                        Edit.setKey notesPath "k" (S.text |> S.with "v") base
                            |> Result.andThen (Edit.removeKey notesPath "k")
                in
                case withKey of
                    Ok doc ->
                        Crdt.read boardSchema doc
                            |> Result.map (.notes >> Dict.member "k")
                            |> Expect.equal (Ok False)

                    Err _ ->
                        Expect.fail "set/remove failed"
        , test "nested record field edits read back through the list" <|
            \_ ->
                let
                    todosPath =
                        Path.root |> Path.field "todos"

                    doc =
                        Edit.listAppend todosPath (todoSchema |> S.with (Todo "task" False)) base
                in
                case doc of
                    Ok d ->
                        Crdt.read boardSchema d
                            |> Result.map (.todos >> List.head)
                            |> Expect.equal (Ok (Just (Todo "task" False)))

                    Err _ ->
                        Expect.fail "append failed"
        ]
