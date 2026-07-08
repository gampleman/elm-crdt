module RefTests exposing (suite)

{-| Type-safe writes via `Crdt.Ref`. These tests exercise the _runtime_ behavior;
the _compile-time_ guarantees (e.g. `increment` only on a counter ref) are asserted
by the fact that this module compiles at all, plus the notes in `docs/07-optics.md`.

Covered: field refs from the builder, `set`/`over`/`increment`, composition with
`at` into a nested record, `switch` on a sum type, and `variantPayload` editing a
variant's payload (applies iff active, silent no-op otherwise).

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Schema as S
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
    { snoozed : Ref Status S.Settable Int
    , done : Ref Status S.Settable String
    }


status : Ref.CustomRefs Status StatusRefs
status =
    Ref.custom
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
        |> Ref.variant0 "active" Active
        |> Ref.variant1 "snoozed" Snoozed S.int
        |> Ref.variant1 "done" Done S.text
        |> Ref.buildCustom


type alias SettingsRefs =
    { theme : Ref Settings S.Settable String
    , size : Ref Settings S.Settable Int
    }


settings : Ref.RecordRefs Settings SettingsRefs
settings =
    Ref.record Settings SettingsRefs
        |> Ref.field "theme" .theme S.text
        |> Ref.field "size" .size S.int
        |> Ref.build


type alias TodoRefs =
    { text : Ref Todo S.Settable String
    , done : Ref Todo S.Settable Bool
    }


todo : Ref.RecordRefs Todo TodoRefs
todo =
    Ref.record Todo TodoRefs
        |> Ref.field "text" .text S.text
        |> Ref.field "done" .done S.bool
        |> Ref.build


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , votes : Ref Board S.Counter Int
    , status : Ref Board (S.Variants Status) Status
    , settings : Ref Board S.Nested Settings
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    , tags : Ref Board (S.ListK S.Fixed S.Settable String) (List String)
    }


board : Ref.RecordRefs Board BoardRefs
board =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "votes" .votes S.counter
        |> Ref.field "status" .status status.schema
        |> Ref.field "settings" .settings settings.schema
        |> Ref.field "todos" .todos (S.movableList todo.schema)
        |> Ref.field "tags" .tags (S.list S.text)
        |> Ref.build


r : BoardRefs
r =
    board.refs


doc0 : OpDoc Board
doc0 =
    OpDoc.init (Id.replica "alice") board.schema


ok : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
ok fallback =
    Result.withDefault fallback


read : OpDoc Board -> Result S.Error Board
read =
    OpDoc.read


{-| Append a todo to `r.todos` (arg order: schema, value, listRef, doc).
-}
addTodo : String -> OpDoc Board -> OpDoc Board
addTodo t doc =
    Ref.append todo.schema (Todo t False) r.todos doc |> ok doc



-- SUITE -----------------------------------------------------------------------


suite : Test
suite =
    describe "Crdt.Ref — type-safe writes"
        [ describe "leaf refs"
            [ test "set a text field" <|
                \_ ->
                    Ref.set r.title "Trip" doc0
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "Trip")
            , test "over transforms the current value" <|
                \_ ->
                    (Ref.set r.title "trip" doc0 |> ok doc0)
                        |> Ref.over r.title String.toUpper
                        |> ok doc0
                        |> read
                        |> Result.map .title
                        |> Expect.equal (Ok "TRIP")
            , test "increment a counter ref" <|
                \_ ->
                    (Ref.increment r.votes 3 doc0 |> ok doc0)
                        |> Ref.increment r.votes 4
                        |> ok doc0
                        |> read
                        |> Result.map .votes
                        |> Expect.equal (Ok 7)
            ]
        , describe "composition with at (nested record)"
            [ test "set a field of a nested record" <|
                \_ ->
                    Ref.set (r.settings |> Ref.at settings.refs.theme) "dark" doc0
                        |> ok doc0
                        |> read
                        |> Result.map (.settings >> .theme)
                        |> Expect.equal (Ok "dark")
            , test "increment is available through composition too (size is Settable, theme text)" <|
                \_ ->
                    Ref.set (r.settings |> Ref.at settings.refs.size) 12 doc0
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
                    Ref.switch r.status (Done "shipped") doc0
                        |> ok doc0
                        |> read
                        |> Result.map .status
                        |> Expect.equal (Ok (Done "shipped"))
            , test "switch to a payload variant then edit its payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            Ref.switch r.status (Done "draft") doc0 |> ok doc0

                        d2 =
                            Ref.set (r.status |> Ref.at status.refs.done) "final" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Done "final"))
            , test "editing a variant payload when a DIFFERENT variant is active is a no-op" <|
                \_ ->
                    let
                        d1 =
                            Ref.switch r.status (Snoozed 5) doc0 |> ok doc0

                        -- status is Snoozed, not Done; editing the Done note does nothing
                        d2 =
                            Ref.set (r.status |> Ref.at status.refs.done) "ignored" d1 |> ok d1
                    in
                    read d2 |> Result.map .status |> Expect.equal (Ok (Snoozed 5))
            , test "over the snoozed payload via the builder's ref" <|
                \_ ->
                    let
                        d1 =
                            Ref.switch r.status (Snoozed 10) doc0 |> ok doc0

                        d2 =
                            Ref.over (r.status |> Ref.at status.refs.snoozed) (\n -> n + 5) d1 |> ok d1
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
                            Ref.set (r.todos |> Ref.index 0 todo.schema |> Ref.at todo.refs.done) True d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .done) |> Expect.equal (Ok [ True ])
            , test "move reorders a movable list" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b" |> addTodo "c"

                        d2 =
                            Ref.move 0 2 r.todos d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b", "c", "a" ])
            , test "remove drops an element" <|
                \_ ->
                    let
                        d1 =
                            doc0 |> addTodo "a" |> addTodo "b"

                        d2 =
                            Ref.remove 0 r.todos d1 |> ok d1
                    in
                    read d2 |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "b" ])
            , test "set a leaf list element directly (tags is List String)" <|
                \_ ->
                    let
                        d1 =
                            (Ref.append S.text "urgent" r.tags doc0 |> ok doc0)
                                |> Ref.append S.text "later" r.tags
                                |> ok doc0

                        d2 =
                            Ref.set (r.tags |> Ref.index 1 S.text) "soon" d1 |> ok d1
                    in
                    read d2 |> Result.map .tags |> Expect.equal (Ok [ "urgent", "soon" ])
            ]
        ]
