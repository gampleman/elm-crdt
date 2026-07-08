module CursorTests exposing (suite)

{-| Stable cursors (see `docs/03-stable-cursors.md`). A cursor anchors to element
identity, so its resolved offset tracks the _content_ it pointed at as other
replicas concurrently insert, delete, and reorder — and two converged replicas
resolve the same cursor to the same offset.

These tests are pure library (no UI): make a cursor, mutate/merge, resolve.

-}

import Crdt.Cursor as Cursor
import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Json.Decode as Json
import Test exposing (Test, describe, test)



-- FIXTURE --------------------------------------------------------------------


type alias Doc =
    { title : String
    , todos : List Todo
    }


type alias Todo =
    { text : String }


schema : Crdt S.Nested Doc
schema =
    S.record Doc
        |> S.field "title" .title S.text
        |> S.field "todos" .todos (S.list todoSchema)
        |> S.build


todoSchema : Crdt S.Nested Todo
todoSchema =
    S.record Todo |> S.field "text" .text S.text |> S.build


titlePath : Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


initDoc : String -> OpDoc Doc
initDoc name =
    OpDoc.init (Id.replica name) schema


okDoc : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
okDoc fallback =
    Result.withDefault fallback


okC : Result OpDoc.Error a -> Maybe a
okC =
    Result.toMaybe


{-| Set the title to a literal string (text edit).
-}
setTitle : String -> OpDoc Doc -> OpDoc Doc
setTitle s doc =
    OpDoc.setText titlePath s doc |> okDoc doc


addTodo : String -> OpDoc Doc -> OpDoc Doc
addTodo label doc =
    OpDoc.listAppend todosPath (todoSchema |> S.with (Todo label)) doc |> okDoc doc



-- TESTS ----------------------------------------------------------------------


suite : Test
suite =
    describe "stable cursors"
        [ test "round-trip: cursorAt offset then cursorOffset returns the same offset" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "hello"
                in
                -- offset 3 in "hello" (between 'l' and 'l')
                case OpDoc.cursorAt titlePath 3 doc |> okC of
                    Just cur ->
                        OpDoc.cursorOffset cur doc |> Expect.equal (Just 3)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "tracks content when text is inserted BEFORE the cursor" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "world"

                    -- caret at offset 2 (before 'r' in "wo|rld")
                    cur =
                        OpDoc.cursorAt titlePath 2 doc |> okC

                    -- prepend "say " -> "say world"; the caret should now be at 6
                    doc2 =
                        setTitle "say world" doc
                in
                case cur of
                    Just c ->
                        OpDoc.cursorOffset c doc2 |> Expect.equal (Just 6)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "unaffected by text inserted AFTER the cursor" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "ab"

                    -- caret at offset 1 ("a|b")
                    cur =
                        OpDoc.cursorAt titlePath 1 doc |> okC

                    doc2 =
                        setTitle "abXYZ" doc
                in
                case cur of
                    Just c ->
                        OpDoc.cursorOffset c doc2 |> Expect.equal (Just 1)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "survives deletion of the anchored character (lands at nearest live spot)" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "abcd"

                    -- caret after 'b' (offset 2)
                    cur =
                        OpDoc.cursorAt titlePath 2 doc |> okC

                    -- delete 'b' and 'c' -> "ad"; the anchor char ('b') is gone,
                    -- so the caret should land after 'a' (offset 1)
                    doc2 =
                        setTitle "ad" doc
                in
                case cur of
                    Just c ->
                        OpDoc.cursorOffset c doc2 |> Expect.equal (Just 1)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "converges: two replicas resolve the same cursor to the same offset" <|
            \_ ->
                let
                    base =
                        initDoc "alice" |> setTitle "shared"

                    -- a cursor at offset 3, captured on the shared base
                    cur =
                        OpDoc.cursorAt titlePath 3 base |> okC

                    -- bob starts from the same shared state
                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode base)
                            |> Result.withDefault (initDoc "bob")

                    -- alice and bob edit concurrently, then exchange ops both ways
                    aEdited =
                        setTitle "XYshared" base

                    bEdited =
                        setTitle "sharedZZ" bob

                    aMerged =
                        mergeFrom bEdited aEdited

                    bMerged =
                        mergeFrom aEdited bEdited
                in
                case cur of
                    Just c ->
                        -- both replicas converged (same read) AND resolve the
                        -- cursor to the same offset
                        Expect.all
                            [ \_ -> Expect.equal (OpDoc.read aMerged) (OpDoc.read bMerged)
                            , \_ -> Expect.equal (OpDoc.cursorOffset c aMerged) (OpDoc.cursorOffset c bMerged)
                            ]
                            ()

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "nested stability: a cursor into todos[k].text survives a todo inserted before k" <|
            \_ ->
                let
                    -- two todos; cursor into the SECOND todo's text at offset 1
                    doc =
                        initDoc "alice" |> addTodo "first" |> addTodo "second"

                    secondText =
                        Path.root |> Path.field "todos" |> Path.index 1 |> Path.field "text"

                    cur =
                        OpDoc.cursorAt secondText 1 doc |> okC

                    -- a peer inserts a NEW todo at the front; "second" is now index 2
                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode doc)
                            |> Result.withDefault (initDoc "bob")
                            |> (\d -> OpDoc.listAppend todosPath (todoSchema |> S.with (Todo "zero")) d |> okDoc d)

                    merged =
                        OpDoc.merge doc (mergeFrom bob doc)
                in
                case cur of
                    Just c ->
                        -- the cursor still resolves into "second"'s text at offset 1,
                        -- regardless of the outer list reorder
                        OpDoc.cursorOffset c merged |> Expect.equal (Just 1)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "range resolves to a normalized (start, end) offset pair" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "abcdef"

                    -- select offsets 1..4 (focus before anchor to test normalization)
                    a =
                        OpDoc.cursorAt titlePath 4 doc |> okC

                    f =
                        OpDoc.cursorAt titlePath 1 doc |> okC
                in
                case Maybe.map2 Cursor.range a f of
                    Just r ->
                        OpDoc.cursorRange r doc |> Expect.equal (Just ( 1, 4 ))

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "cursor JSON round-trips" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "hello"

                    cur =
                        OpDoc.cursorAt titlePath 3 doc |> okC
                in
                case cur of
                    Just c ->
                        Cursor.encode c
                            |> Json.decodeValue Cursor.decoder
                            |> Result.toMaybe
                            |> Maybe.andThen (\c2 -> OpDoc.cursorOffset c2 doc)
                            |> Expect.equal (Just 3)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        ]


{-| Encode `from`'s ops and decode them into `into`, returning the updated
`into`. (A little sugar so the convergence tests read cleanly.)
-}
mergeFrom : OpDoc Doc -> OpDoc Doc -> OpDoc Doc
mergeFrom from into =
    into
        |> OpDoc.decodeInto (OpDoc.encode from)
        |> Result.withDefault into
