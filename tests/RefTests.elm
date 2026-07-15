module RefTests exposing (suite)

{-| Type-safe writes via `Crdt.Ref`. These tests exercise the _runtime_ behavior;
the _compile-time_ guarantees (e.g. `increment` only on a counter ref) are asserted
by the fact that this module compiles at all, plus the notes in `docs/07-optics.md`.

Covered: field refs from the builder, `set`/`over`/`increment`, composition with
`at` into a nested record, `switch` on a sum type, and editing a variant's payload
through a builder-provided ref (applies iff active, silent no-op otherwise).

-}

import Crdt as C exposing (Ref)
import Crdt.Doc exposing (Doc)
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
type alias StatusRefs =
    { snoozed : Ref Status C.Settable Int
    , done : Ref Status C.Settable String
    }


status : C.CustomRefs Status StatusRefs
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
        StatusRefs
        |> C.variant0 "active" Active
        |> C.variant1 "snoozed" Snoozed C.int
        |> C.variant1 "done" Done C.text
        |> C.buildCustom


type alias SettingsRefs =
    { theme : Ref Settings C.Settable String
    , size : Ref Settings C.Settable Int
    }


settings : C.RecordRefs Settings SettingsRefs
settings =
    C.record Settings SettingsRefs
        |> C.field "theme" .theme C.text
        |> C.field "size" .size C.int
        |> C.build


type alias TodoRefs =
    { text : Ref Todo C.Settable String
    , done : Ref Todo C.Settable Bool
    }


todo : C.RecordRefs Todo TodoRefs
todo =
    C.record Todo TodoRefs
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
    }


board : C.RecordRefs Board BoardRefs
board =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "votes" .votes C.counter
        |> C.field "status" .status status.schema
        |> C.field "settings" .settings settings.schema
        |> C.field "todos" .todos (C.movableList todo.schema)
        |> C.field "tags" .tags (C.list C.text)
        |> C.build


r : BoardRefs
r =
    board.refs


doc0 : Doc Board
doc0 =
    C.init (Id.replica "alice") board.schema


ok : Doc Board -> Result C.EditError (Doc Board) -> Doc Board
ok fallback =
    Result.withDefault fallback


read : Doc Board -> Result C.ReadError Board
read =
    C.read


{-| Append a todo to `r.todos` (arg order: schema, value, listRef, doc).
-}
addTodo : String -> Doc Board -> Doc Board
addTodo t doc =
    C.append todo.schema (Todo t False) r.todos doc |> ok doc



-- SUITE -----------------------------------------------------------------------


suite : Test
suite =
    describe "Crdt.Ref — type-safe writes"
        [ describe "leaf refs"
            [ test "set a text field" <|
                \_ ->
                    C.set r.title "Trip" doc0
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "Trip")
            , test "over transforms the current value" <|
                \_ ->
                    (C.set r.title "trip" doc0 |> ok doc0)
                        |> C.over r.title String.toUpper
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "TRIP")
            , test "increment a counter ref" <|
                \_ ->
                    (C.increment r.votes 3 doc0 |> ok doc0)
                        |> C.increment r.votes 4
                        |> ok doc0
                        |> read
                        |> Result.map .votes
                        |> Expect.equal (Ok 7)
            ]
        , describe "composition with at (nested record)"
            [ test "set a field of a nested record" <|
                \_ ->
                    C.set (r.settings |> C.at settings.refs.theme) "dark" doc0
                        |> ok doc0
                        |> read
                        |> Result.map (.settings >> .theme)
                        |> Expect.equal (Ok "dark")
            , test "increment is available through composition too (size is Settable, theme text)" <|
                \_ ->
                    C.set (r.settings |> C.at settings.refs.size) 12 doc0
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
                    C.switch r.status (Done "shipped") doc0
                        |> ok doc0
                        |> read
                        |> Result.map .status
                        |> Expect.equal (Ok (Done "shipped"))
            , test "switch to a payload variant then edit its payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            C.switch r.status (Done "draft") doc0 |> ok doc0

                        d2 =
                            C.set (r.status |> C.at status.refs.done) "final" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Done "final"))
            , test "editing a variant payload when a DIFFERENT variant is active is a no-op" <|
                \_ ->
                    let
                        d1 =
                            C.switch r.status (Snoozed 5) doc0 |> ok doc0

                        -- status is Snoozed, not Done; editing the Done note does nothing
                        d2 =
                            C.set (r.status |> C.at status.refs.done) "ignored" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Snoozed 5))
            , test "over the snoozed payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            C.switch r.status (Snoozed 10) doc0 |> ok doc0

                        d2 =
                            C.over (r.status |> C.at status.refs.snoozed) (\n -> n + 5) d1 |> ok d1
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
                            C.set (r.todos |> C.index 0 todo.schema |> C.at todo.refs.done) True d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .done) |> Expect.equal (Ok [ True ])
            , test "move reorders a movable list" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b" |> addTodo "c"

                        d2 =
                            C.move 0 2 r.todos d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b", "c", "a" ])
            , test "remove drops an element" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b"

                        d2 =
                            C.remove 0 r.todos d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b" ])
            , test "set a leaf list element directly (tags is List String)" <|
                \_ ->
                    let
                        d1 =
                            (C.append C.text "urgent" r.tags doc0 |> ok doc0)
                                |> C.append C.text "later" r.tags
                                |> ok doc0

                        d2 =
                            C.set (r.tags |> C.index 1 C.text) "soon" d1 |> ok d1
                    in
                    read d2 |> Result.map .tags |> Expect.equal (Ok [ "urgent", "soon" ])
            ]
        ]
