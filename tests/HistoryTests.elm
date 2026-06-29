module HistoryTests exposing (suite)

{-| History / version control: named checkpoints, checkout of an old version
(read-only snapshot), restore (a new edit that reverts), and undo/redo.

History is derived from the document's own causal metadata — checking out an old
version must NOT lose newer concurrent edits when later merged, since restore is
modeled as a normal edit.

-}

import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.History as History
import Crdt.Id as Id
import Crdt.Path as Path
import Crdt.Schema as S
import Expect
import Helpers exposing (boardSchema)
import Test exposing (Test, describe, test)


titlePath : Path.Path
titlePath =
    Path.root |> Path.field "title"


setTitle : String -> Doc -> Doc
setTitle s doc =
    Edit.setText titlePath s doc |> Result.withDefault doc


base : Doc
base =
    Crdt.init (Id.replica "alice") boardSchema


readTitle : Doc -> Maybe String
readTitle doc =
    Crdt.read boardSchema doc |> Result.toMaybe |> Maybe.map .title


suite : Test
suite =
    describe "History / version control"
        [ test "a checkpoint records a labeled version" <|
            \_ ->
                let
                    doc =
                        base
                            |> setTitle "v1"
                            |> History.commit "first draft"
                in
                History.checkpoints doc
                    |> List.map History.checkpointMessage
                    |> Expect.equal [ "first draft" ]
        , test "checkout returns the document state as of that version" <|
            \_ ->
                let
                    doc =
                        base
                            |> setTitle "early"
                            |> History.commit "cp"

                    v =
                        History.checkpoints doc
                            |> List.head
                            |> Maybe.map History.checkpointVersion

                    later =
                        setTitle "later" doc
                in
                case v of
                    Just version ->
                        History.checkout version later
                            |> Maybe.andThen readTitle
                            |> Expect.equal (Just "early")

                    Nothing ->
                        Expect.fail "no checkpoint version"
        , test "undo reverts the last edit; redo reapplies it" <|
            \_ ->
                let
                    doc =
                        base |> setTitle "one" |> setTitle "two"

                    undone =
                        History.undo doc

                    redone =
                        History.redo undone
                in
                Expect.all
                    [ \_ -> Expect.equal (Just "one") (readTitle undone)
                    , \_ -> Expect.equal (Just "two") (readTitle redone)
                    ]
                    ()
        , test "restore reverts an edit made after the checkpoint (not a no-op)" <|
            \_ ->
                let
                    doc =
                        base |> setTitle "first" |> History.commit "cp"

                    v =
                        History.checkpoints doc
                            |> List.head
                            |> Maybe.map History.checkpointVersion

                    -- edit AFTER the checkpoint, so the current value has a
                    -- higher LWW stamp than the snapshot
                    edited =
                        setTitle "second" doc
                in
                case v |> Maybe.andThen (\ver -> History.checkout ver edited) of
                    Just old ->
                        History.restore old edited
                            |> readTitle
                            |> Expect.equal (Just "first")

                    Nothing ->
                        Expect.fail "checkout failed"
        , test "restore is a new edit, so concurrent edits from a peer are preserved on merge" <|
            \_ ->
                let
                    doc =
                        base |> setTitle "original" |> History.commit "cp"

                    v =
                        History.checkpoints doc
                            |> List.head
                            |> Maybe.map History.checkpointVersion

                    -- alice keeps editing; bob concurrently appends a todo
                    aliceLater =
                        setTitle "alice-edit" doc

                    bob =
                        Edit.listAppend (Path.root |> Path.field "todos")
                            (Helpers.todoSchema |> S.with (Helpers.Todo "bob-todo" False))
                            (Crdt.init (Id.replica "bob") boardSchema)
                            |> Result.withDefault doc
                in
                case v |> Maybe.andThen (\ver -> History.checkout ver aliceLater) of
                    Just old ->
                        let
                            restored =
                                History.restore old aliceLater

                            merged =
                                Crdt.merge restored bob
                        in
                        Expect.all
                            [ -- the revert actually takes effect: title goes
                              -- back to the snapshot value, beating "alice-edit"
                              \_ ->
                                Crdt.read boardSchema merged
                                    |> Result.map .title
                                    |> Expect.equal (Ok "original")

                            -- bob's concurrent todo must survive the restore
                            , \_ ->
                                Crdt.read boardSchema merged
                                    |> Result.map (.todos >> List.length)
                                    |> Expect.equal (Ok 1)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "checkout failed"
        ]
