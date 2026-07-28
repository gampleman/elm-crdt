module SeedElementTests exposing (suite)

{-| Stamp-soundness of `Schema.Internal.seedOneFrom` — the primitive that recovers an
element seeder from a CONTAINER schema (so the edit APIs can drop the element-schema arg
and take the container ref instead). The risk is that a recovered seed could mint stamps
from a throwaway clock and collide with the document's live clock, dropping an element
under concurrent edits. These tests append/set via the recovered seeder on two concurrent
replicas and assert BOTH survive the merge (a duplicate OpId would silently drop one), and
that the seeded element's nested value reads back correctly.
-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node)
import Crdt.Path as Path exposing (Path)
import Crdt.Rga as Rga
import Crdt.Schema.Internal as S exposing (Crdt)
import Dict
import Expect
import Test exposing (Test, describe, test)


type alias Sample =
    { todos : List Todo
    , movTodos : List Todo
    , notes : Dict.Dict String Todo
    }


type alias Todo =
    { text : String }


todoSchema : Crdt S.Nested Todo
todoSchema =
    S.record Todo |> S.field "text" .text S.text |> S.build


listSchema : Crdt (S.ListK S.Fixed S.Nested Todo) (List Todo)
listSchema =
    S.list todoSchema


movListSchema : Crdt (S.ListK S.Movable S.Nested Todo) (List Todo)
movListSchema =
    S.movableList todoSchema


dictSchema : Crdt (S.DictK S.Nested Todo) (Dict.Dict String Todo)
dictSchema =
    S.dict todoSchema


schema : Crdt S.Nested Sample
schema =
    S.record Sample
        |> S.field "todos" .todos listSchema
        |> S.field "movTodos" .movTodos movListSchema
        |> S.field "notes" .notes dictSchema
        |> S.build


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


movTodosPath : Path
movTodosPath =
    Path.root |> Path.field "movTodos"


notesPath : Path
notesPath =
    Path.root |> Path.field "notes"


initDoc : String -> Doc Sample
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Sample -> Result Doc.Error (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


{-| The list element seeder recovered from the CONTAINER (`listSchema`) — seeds a singleton
list `[ todo ]` and extracts its single RGA element's content node.
-}
seedTodoInList : Todo -> S.Seed
seedTodoInList =
    S.seedOneFrom (\t -> [ t ]) firstSeqElement listSchema


{-| The element seeder recovered from a MOVABLE list container (`movListSchema`) — seeds a
singleton movable list and extracts its single cell's content node. A movable list stores a
`Mov` node (not a `Seq` RGA), so the extractor must handle that — the regression this guards
(a `Seq`-only extractor silently fell through and inserted the whole container).
-}
seedTodoInMovList : Todo -> S.Seed
seedTodoInMovList =
    S.seedOneFrom (\t -> [ t ]) firstMovElement movListSchema


{-| The dict value seeder recovered from the CONTAINER (`dictSchema`) — seeds a singleton
dict and extracts its single value node.
-}
seedTodoInDict : Todo -> S.Seed
seedTodoInDict =
    S.seedOneFrom (\t -> Dict.singleton "one" t) firstMapValue dictSchema


firstSeqElement : Node -> Maybe Node
firstSeqElement node =
    Node.asSeq node
        |> Maybe.andThen (Rga.elements >> List.head)
        |> Maybe.map .content


firstMovElement : Node -> Maybe Node
firstMovElement node =
    Node.asMov node
        |> Maybe.andThen (MoveList.toEntries >> List.head)
        |> Maybe.map Tuple.second


firstMapValue : Node -> Maybe Node
firstMapValue node =
    Node.presentEntries node |> List.head |> Maybe.map Tuple.second


texts : Sample -> List String
texts s =
    List.map .text s.todos


read : Doc Sample -> Sample
read d =
    Doc.read d |> Result.withDefault (Sample [] [] Dict.empty)


suite : Test
suite =
    describe "seedOneFrom — element seeder recovered from a container schema"
        [ test "appends read back the seeded nested value" <|
            \_ ->
                let
                    d =
                        initDoc "a"
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "buy milk")) x |> ok x)
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "walk dog")) x |> ok x)
                in
                texts (read d) |> Expect.equal [ "buy milk", "walk dog" ]
        , test "MOVABLE-list appends read back (Mov node, not Seq — regression)" <|
            \_ ->
                -- a movable list stores a `Mov` node; the recovered seeder's extractor must
                -- handle it. A Seq-only extractor silently inserted the whole container,
                -- which then failed to read back ("expected record for field ...").
                let
                    d =
                        initDoc "a"
                            |> (\x -> Doc.listAppend movTodosPath (seedTodoInMovList (Todo "one")) x |> ok x)
                            |> (\x -> Doc.listAppend movTodosPath (seedTodoInMovList (Todo "two")) x |> ok x)
                in
                List.map .text (read d).movTodos |> Expect.equal [ "one", "two" ]
        , test "concurrent appends via the recovered seeder BOTH survive (stamp-sound)" <|
            \_ ->
                let
                    base =
                        initDoc "seed"
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "base")) x |> ok x)

                    alice =
                        Doc.decodeInto (Doc.encode base) (initDoc "alice")
                            |> Result.withDefault (initDoc "alice")
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "from-alice")) x |> ok x)

                    bob =
                        Doc.decodeInto (Doc.encode base) (initDoc "bob")
                            |> Result.withDefault (initDoc "bob")
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "from-bob")) x |> ok x)

                    merged =
                        Doc.merge bob alice
                in
                Expect.all
                    [ \_ -> List.member "from-alice" (texts (read merged)) |> Expect.equal True
                    , \_ -> List.member "from-bob" (texts (read merged)) |> Expect.equal True
                    , \_ -> List.length (read merged).todos |> Expect.equal 3
                    ]
                    ()
        , test "dict value seeder recovered from the container reads back" <|
            \_ ->
                let
                    d =
                        initDoc "a"
                            |> (\x -> Doc.setKey notesPath "k" (seedTodoInDict (Todo "note!")) x |> ok x)
                in
                Dict.get "k" (read d).notes |> Maybe.map .text |> Expect.equal (Just "note!")
        , test "the recovered seeder mints from the LIVE clock (edit after append doesn't collide)" <|
            \_ ->
                -- append via the recovered seeder, then a further append on the same replica
                -- must not reuse an OpId (which would drop one). Two distinct elements remain.
                let
                    d =
                        initDoc "a"
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "one")) x |> ok x)
                            |> (\x -> Doc.listAppend todosPath (seedTodoInList (Todo "two")) x |> ok x)
                in
                Expect.all
                    [ \_ -> texts (read d) |> Expect.equal [ "one", "two" ]
                    , \_ -> Doc.cacheConsistent d |> Expect.equal True
                    ]
                    ()
        ]
