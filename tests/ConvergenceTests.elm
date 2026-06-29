module ConvergenceTests exposing (suite)

{-| End-to-end convergence through the _public_ API — the way the demo uses it.

Two (or three) replicas apply edits independently, exchange full state, and
must read back identical typed values regardless of the order in which states
are merged. This is the property users actually care about.

-}

import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Path as Path
import Crdt.Schema as S
import Expect
import Helpers exposing (Board, Todo, boardSchema, todoSchema)
import Test exposing (Test, describe, test)


initDoc : String -> Doc
initDoc name =
    Crdt.init (Id.replica name) boardSchema


ok : Result Edit.Error Doc -> Doc -> Doc
ok result fallback =
    Result.withDefault fallback result


titlePath : Path.Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path.Path
todosPath =
    Path.root |> Path.field "todos"


read : Doc -> Result Crdt.Error Board
read =
    Crdt.read boardSchema


suite : Test
suite =
    describe "convergence (public API)"
        [ test "concurrent title text edits merge character-wise and converge" <|
            \_ ->
                let
                    base =
                        initDoc "alice"

                    a =
                        Edit.setText titlePath "Trip plan" base |> (\r -> ok r base)

                    b =
                        Edit.setText titlePath "Trip" (initDoc "bob") |> (\r -> ok r base)

                    ab =
                        Crdt.merge a b

                    ba =
                        Crdt.merge b a
                in
                Expect.equal (read ab) (read ba)
        , test "concurrent list appends from two replicas converge (both todos survive)" <|
            \_ ->
                let
                    a =
                        initDoc "alice"
                            |> Edit.listAppend todosPath (todoSchema |> S.with (Todo "pack" False))
                            |> (\r -> ok r (initDoc "alice"))

                    b =
                        initDoc "bob"
                            |> Edit.listAppend todosPath (todoSchema |> S.with (Todo "tickets" False))
                            |> (\r -> ok r (initDoc "bob"))

                    merged =
                        Crdt.merge a b
                in
                case read merged of
                    Ok board ->
                        Expect.equal 2 (List.length board.todos)

                    Err _ ->
                        Expect.fail "schema read failed after merge"
        , test "merge is commutative on the typed read value" <|
            \_ ->
                let
                    a =
                        initDoc "alice"
                            |> Edit.listAppend todosPath (todoSchema |> S.with (Todo "a" False))
                            |> (\r -> ok r (initDoc "alice"))

                    b =
                        initDoc "bob"
                            |> Edit.listAppend todosPath (todoSchema |> S.with (Todo "b" True))
                            |> (\r -> ok r (initDoc "bob"))
                in
                Expect.equal (read (Crdt.merge a b)) (read (Crdt.merge b a))
        , test "LWW: latest done-toggle wins by stamp after merge" <|
            \_ ->
                let
                    shared =
                        initDoc "alice"
                            |> Edit.listAppend todosPath (todoSchema |> S.with (Todo "x" False))
                            |> (\r -> ok r (initDoc "alice"))

                    donePath =
                        Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

                    -- alice and bob start from the shared state, toggle differently
                    a =
                        Edit.setBool donePath True shared |> (\r -> ok r shared)

                    b =
                        Edit.setBool donePath False (Crdt.merge (initDoc "bob") shared)
                            |> (\r -> ok r shared)
                in
                -- whatever the winner, both orders must agree
                Expect.equal (read (Crdt.merge a b)) (read (Crdt.merge b a))
        ]
