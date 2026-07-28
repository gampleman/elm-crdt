module RefTests exposing (suite)

{-| Type-safe writes via `Crdt.Ref`. These tests exercise the _runtime_ behavior;
the _compile-time_ guarantees (e.g. `increment` only on a counter ref) are asserted
by the fact that this module compiles at all, plus the notes in `docs/07-optics.md`.

Covered: field refs from the builder, `set`/`over`/`increment`, composition with
`at` into a nested record, `switch` on a sum type, and editing a variant's payload
through a builder-provided ref (applies iff active, silent no-op otherwise).

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Test exposing (Test, describe, test)



-- DOMAIN ----------------------------------------------------------------------


type Status
    = Active
    | Snoozed Int
    | Done String


type alias Settings =
    { theme : String, size : Int }


type alias Todo =
    { text : String, done : Bool }


type alias Board =
    { title : String
    , votes : Int
    , status : Status
    , settings : Settings
    , todos : List Todo
    , tags : List String
    }



-- SCHEMAS + REFS --------------------------------------------------------------


{-| Sum type built with the ref-emitting `customR` builder, so we get typed payload
refs back (`status.refs.snoozed`, `status.refs.done`).
-}
type alias StatusDoc =
    { snoozed : Ref Status C.Settable Int
    , done : Ref Status C.Settable String
    , schema : C.Schema (C.Variants Status) Status
    }


status : StatusDoc
status =
    C.custom
        (\active snoozed done v ->
            case v of
                Active ->
                    active

                Snoozed n ->
                    snoozed n

                Done s ->
                    done s
        )
        StatusDoc
        |> C.variant0 "active" Active
        |> C.variant1 "snoozed" Snoozed C.int
        |> C.variant1 "done" Done C.text
        |> C.buildCustom


type alias SettingsDoc =
    { theme : Ref Settings C.Settable String
    , size : Ref Settings C.Settable Int
    , schema : C.Schema C.Nested Settings
    }


settings : SettingsDoc
settings =
    C.record Settings SettingsDoc
        |> C.field "theme" .theme C.text
        |> C.field "size" .size C.int
        |> C.build


type alias TodoDoc =
    { text : Ref Todo C.Settable String
    , done : Ref Todo C.Settable Bool
    , schema : C.Schema C.Nested Todo
    }


todo : TodoDoc
todo =
    C.record Todo TodoDoc
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias BoardRefs =
    { title : Ref Board C.Settable String
    , votes : Ref Board C.Counter Int
    , status : Ref Board (C.Variants Status) Status
    , settings : Ref Board C.Nested Settings
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , tags : Ref Board (C.ListK C.Fixed C.Settable String) (List String)
    , schema : C.Schema C.Nested Board
    }


todosList =
    C.movableList todo


tagsList =
    C.list C.text


board : BoardRefs
board =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "votes" .votes C.counter
        |> C.field "status" .status status
        |> C.field "settings" .settings settings
        |> C.field "todos" .todos todosList
        |> C.field "tags" .tags tagsList
        |> C.build


r : BoardRefs
r =
    board


doc0 : Doc Board
doc0 =
    C.init (Id.replica "alice") board.schema


ok : Doc Board -> Result Edit.EditError (Doc Board) -> Doc Board
ok fallback =
    Result.withDefault fallback


read : Doc Board -> Result Doc.ReadError Board
read =
    Doc.read


{-| Append a todo to `r.todos` (arg order: schema, value, listRef, doc).
-}
addTodo : String -> Doc Board -> Doc Board
addTodo t doc =
    Edit.append r.todos (Todo t False) doc |> ok doc



-- SUITE -----------------------------------------------------------------------


suite : Test
suite =
    describe "Crdt.Ref — type-safe writes"
        [ describe "leaf refs"
            [ test "set a text field" <|
                \_ ->
                    Edit.set r.title "Trip" doc0
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "Trip")
            , test "over transforms the current value" <|
                \_ ->
                    (Edit.set r.title "trip" doc0 |> ok doc0)
                        |> Edit.over r.title String.toUpper
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "TRIP")
            , test "increment a counter ref" <|
                \_ ->
                    (Edit.increment r.votes 3 doc0 |> ok doc0)
                        |> Edit.increment r.votes 4
                        |> ok doc0
                        |> read
                        |> Result.map .votes
                        |> Expect.equal (Ok 7)
            ]
        , describe "composition with at (nested record)"
            [ test "set a field of a nested record" <|
                \_ ->
                    Edit.set (r.settings |> C.at settings.theme) "dark" doc0
                        |> ok doc0
                        |> read
                        |> Result.map (.settings >> .theme)
                        |> Expect.equal (Ok "dark")
            , test "increment is available through composition too (size is Settable, theme text)" <|
                \_ ->
                    Edit.set (r.settings |> C.at settings.size) 12 doc0
                        |> ok doc0
                        |> read
                        |> Result.map (.settings >> .size)
                        |> Expect.equal (Ok 12)
            ]
        , describe "sum types"
            [ test "fresh doc is the default variant" <|
                \_ ->
                    read doc0 |> Result.map .status |> Expect.equal (Ok Active)
            , test "switch changes the active variant" <|
                \_ ->
                    Edit.switch r.status (Done "shipped") doc0
                        |> ok doc0
                        |> read
                        |> Result.map .status
                        |> Expect.equal (Ok (Done "shipped"))
            , test "switch to a payload variant then edit its payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            Edit.switch r.status (Done "draft") doc0 |> ok doc0

                        d2 =
                            Edit.set (r.status |> C.at status.done) "final" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Done "final"))
            , test "editing a variant payload when a DIFFERENT variant is active is a no-op" <|
                \_ ->
                    let
                        d1 =
                            Edit.switch r.status (Snoozed 5) doc0 |> ok doc0

                        -- status is Snoozed, not Done; editing the Done note does nothing
                        d2 =
                            Edit.set (r.status |> C.at status.done) "ignored" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Snoozed 5))
            , test "over the snoozed payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            Edit.switch r.status (Snoozed 10) doc0 |> ok doc0

                        d2 =
                            Edit.over (r.status |> C.at status.snoozed) (\n -> n + 5) d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Snoozed 15))
            ]
        , describe "list refs (append / remove / move / element)"
            [ test "append then read the list" <|
                \_ ->
                    (doc0 |> addTodo "a" |> addTodo "b")
                        |> read
                        |> Result.map (.todos >> List.map .text)
                        |> Expect.equal (Ok [ "a", "b" ])
            , test "edit a nested field of a list element via index + at" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a"

                        d2 =
                            Edit.set (r.todos |> todosList.index 0 |> C.at todo.done) True d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .done) |> Expect.equal (Ok [ True ])
            , test "move reorders a movable list" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b" |> addTodo "c"

                        d2 =
                            Edit.move r.todos 0 2 d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b", "c", "a" ])
            , test "remove drops an element" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b"

                        d2 =
                            Edit.remove r.todos 0 d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b" ])
            , test "set a leaf list element directly (tags is List String)" <|
                \_ ->
                    let
                        d1 =
                            (Edit.append r.tags "urgent" doc0 |> ok doc0)
                                |> Edit.append r.tags "later"
                                |> ok doc0

                        d2 =
                            Edit.set (r.tags |> tagsList.index 1) "soon" d1 |> ok d1
                    in
                    read d2 |> Result.map .tags |> Expect.equal (Ok [ "urgent", "soon" ])
            ]
        ]
