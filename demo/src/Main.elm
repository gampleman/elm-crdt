module Main exposing (main)

{-| Collaborative todo + notes demo for `jhampl/elm-crdt`.

This file is written FIRST, as the executable specification of the library's
public API. Every `Crdt.*` call below is a requirement the library must satisfy.

It exercises the full JSON-like schema (record + list + dict + text + LWW),
real WebSocket networking over ports, live **presence** (who's here, their
cursor), and **history / version control** (named checkpoints + checkout +
undo/redo).

-}

import Browser
import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.History as History exposing (Checkpoint, Version)
import Crdt.Id exposing (ReplicaId)
import Crdt.Path as Path exposing (Path)
import Crdt.Presence as Presence exposing (Presence)
import Crdt.Schema as S exposing (Crdt)
import Html exposing (Html, button, div, h1, h2, input, li, span, text, ul)
import Html.Attributes as A exposing (class, placeholder, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput)
import Json.Decode as JD
import Json.Encode as JE
import Ports



-- DOMAIN ---------------------------------------------------------------------


{-| The collaborative document. A plain Elm record; the CRDT machinery is
entirely described by `schema` below.
-}
type alias Board =
    { title : String
    , todos : List Todo
    , notes : Dict String String
    }


type alias Todo =
    { text : String
    , done : Bool
    }


{-| The schema is the single source of truth tying typed Elm values to the
underlying CRDT state. Built combinator-style, like an `elm/json` decoder.
-}
schema : Crdt Board
schema =
    S.record Board
        |> S.field "title" .title S.text
        |> S.field "todos" .todos (S.list todoSchema)
        |> S.field "notes" .notes (S.dict S.text)
        |> S.build


todoSchema : Crdt Todo
todoSchema =
    S.record Todo
        |> S.field "text" .text S.text
        |> S.field "done" .done S.bool
        |> S.build


{-| Per-peer ephemeral state. Never merged into the document — it lives on the
separate presence channel and expires when a peer goes quiet.
-}
type alias Cursor =
    { name : String
    , color : String
    , editing : Maybe String -- which field this peer is focused on
    }


cursorCodec : Presence.Codec Cursor
cursorCodec =
    Presence.codec Cursor
        |> Presence.field "name" .name Presence.string
        |> Presence.field "color" .color Presence.string
        |> Presence.optional "editing" .editing Presence.string
        |> Presence.buildCodec



-- PATHS (typed accessors into the document) ----------------------------------


titlePath : Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


todoTextPath : Int -> Path
todoTextPath i =
    Path.root |> Path.field "todos" |> Path.index i |> Path.field "text"


todoDonePath : Int -> Path
todoDonePath i =
    Path.root |> Path.field "todos" |> Path.index i |> Path.field "done"


notePath : String -> Path
notePath k =
    Path.root |> Path.field "notes" |> Path.key k



-- FIELD IDS ------------------------------------------------------------------
-- Stable identifiers for editable fields, shared between focus reporting (what
-- we broadcast in our presence) and highlighting (whether some *other* peer is
-- editing the field we're rendering). Keeping these in one place is what makes
-- the two sides agree.


titleField : String
titleField =
    "title"


todoField : Int -> String
todoField i =
    "todo:" ++ String.fromInt i


noteField : String -> String
noteField k =
    "note:" ++ k



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { me : ReplicaId
    , doc : Doc
    , presence : Presence Cursor
    , peers : Presence Cursor -- merged view of everyone (incl. self)

    -- transient form state
    , newTodo : String
    , newNoteKey : String

    -- history / version control
    , checkpointMsg : String
    , viewing : Maybe Version -- Just v => previewing an old version (read-only)
    , connected : Bool
    }


type alias Flags =
    { replicaId : String
    , name : String
    , color : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        me =
            Crdt.Id.replica flags.replicaId

        presence =
            Presence.init me cursorCodec
                |> Presence.setLocal
                    { name = flags.name, color = flags.color, editing = Nothing }
    in
    ( { me = me
      , doc = Crdt.init me schema
      , presence = presence
      , peers = presence
      , newTodo = ""
      , newNoteKey = ""
      , checkpointMsg = ""
      , viewing = Nothing
      , connected = False
      }
    , broadcastPresence presence
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = -- title
      TitleChanged String
      -- todos
    | NewTodoChanged String
    | AddTodo
    | TodoTextChanged Int String
    | ToggleTodo Int
    | RemoveTodo Int
      -- notes (dict)
    | NewNoteKeyChanged String
    | AddNote
    | NoteChanged String String
    | RemoveNote String
      -- presence
    | FocusField String
    | BlurField
      -- history / version control
    | CheckpointMsgChanged String
    | SaveCheckpoint
    | PreviewVersion Version
    | RestoreVersion Version
    | LeavePreview
    | Undo
    | Redo
      -- networking
    | GotMessage JD.Value
    | ConnectionChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TitleChanged s ->
            model |> editText titlePath s

        NewTodoChanged s ->
            ( { model | newTodo = s }, Cmd.none )

        AddTodo ->
            if String.isEmpty (String.trim model.newTodo) then
                ( model, Cmd.none )

            else
                let
                    -- append a fresh todo subtree at the end of the list
                    doc1 =
                        model.doc
                            |> Edit.listAppend todosPath
                                (todoSchema |> S.with (Todo model.newTodo False))
                            |> orKeep model.doc
                in
                pushDoc { model | doc = doc1, newTodo = "" }

        TodoTextChanged i s ->
            model |> editText (todoTextPath i) s

        ToggleTodo i ->
            let
                current =
                    Crdt.read schema model.doc
                        |> Result.toMaybe
                        |> Maybe.andThen (\b -> List.drop i b.todos |> List.head)
                        |> Maybe.map .done
                        |> Maybe.withDefault False

                doc1 =
                    model.doc
                        |> Edit.setBool (todoDonePath i) (not current)
                        |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        RemoveTodo i ->
            let
                doc1 =
                    model.doc |> Edit.listRemove todosPath i |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        NewNoteKeyChanged s ->
            ( { model | newNoteKey = s }, Cmd.none )

        AddNote ->
            if String.isEmpty (String.trim model.newNoteKey) then
                ( model, Cmd.none )

            else
                let
                    doc1 =
                        model.doc
                            |> Edit.setKey (Path.root |> Path.field "notes")
                                model.newNoteKey
                                (S.text |> S.with "")
                            |> orKeep model.doc
                in
                pushDoc { model | doc = doc1, newNoteKey = "" }

        NoteChanged k s ->
            model |> editText (notePath k) s

        RemoveNote k ->
            let
                doc1 =
                    model.doc
                        |> Edit.removeKey (Path.root |> Path.field "notes") k
                        |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        FocusField fieldName ->
            setEditing (Just fieldName) model

        BlurField ->
            setEditing Nothing model

        CheckpointMsgChanged s ->
            ( { model | checkpointMsg = s }, Cmd.none )

        SaveCheckpoint ->
            let
                doc1 =
                    History.commit model.checkpointMsg model.doc
            in
            pushDoc { model | doc = doc1, checkpointMsg = "" }

        PreviewVersion v ->
            ( { model | viewing = Just v }, Cmd.none )

        LeavePreview ->
            ( { model | viewing = Nothing }, Cmd.none )

        RestoreVersion v ->
            -- restoring is itself a new edit (a revert), so it merges/syncs normally
            case History.checkout v model.doc of
                Just old ->
                    let
                        doc1 =
                            History.restore old model.doc
                    in
                    pushDoc { model | doc = doc1, viewing = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        Undo ->
            pushDoc { model | doc = History.undo model.doc }

        Redo ->
            pushDoc { model | doc = History.redo model.doc }

        GotMessage raw ->
            case decodeEnvelope raw of
                Ok (DocMsg incomingJson) ->
                    case Crdt.decode model.me incomingJson of
                        Ok incomingDoc ->
                            -- the whole point: merge is commutative & convergent
                            ( { model | doc = Crdt.merge model.doc incomingDoc }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Ok (PresenceMsg incomingJson) ->
                    case Presence.decode cursorCodec incomingJson of
                        Ok incoming ->
                            ( { model | peers = Presence.merge model.peers incoming }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ConnectionChanged isUp ->
            -- on (re)connect, push our full state so a fresh peer catches up
            ( { model | connected = isUp }
            , if isUp then
                Cmd.batch [ broadcastDoc model.doc, broadcastPresence model.presence ]

              else
                Cmd.none
            )



-- EDIT / SYNC HELPERS --------------------------------------------------------


{-| Text fields use the Text CRDT: rather than overwriting, we diff old→new and
apply the minimal insert/delete so concurrent typing merges character-wise.
-}
editText : Path -> String -> Model -> ( Model, Cmd Msg )
editText path newValue model =
    let
        doc1 =
            model.doc
                |> Edit.setText path newValue
                |> orKeep model.doc
    in
    pushDoc { model | doc = doc1 }


orKeep : Doc -> Result Edit.Error Doc -> Doc
orKeep fallback result =
    Result.withDefault fallback result


{-| After any local change, broadcast the new full document state. (Phase-later
optimization: send deltas via `Crdt.encodeSince`.)
-}
pushDoc : Model -> ( Model, Cmd Msg )
pushDoc model =
    ( model, broadcastDoc model.doc )


broadcastDoc : Doc -> Cmd Msg
broadcastDoc doc =
    Ports.outgoing (envelope "doc" (Crdt.encode doc))


broadcastPresence : Presence Cursor -> Cmd Msg
broadcastPresence p =
    Ports.outgoing (envelope "presence" (Presence.encode p))


{-| Update which field we're editing: bump our local presence, fold it into the
merged `peers` view so our own row stays current, and broadcast it to others.
-}
setEditing : Maybe String -> Model -> ( Model, Cmd Msg )
setEditing field model =
    let
        presence1 =
            Presence.updateLocal (\c -> { c | editing = field }) model.presence
    in
    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
    , broadcastPresence presence1
    )



-- WIRE ENVELOPE --------------------------------------------------------------


type Envelope
    = DocMsg JD.Value
    | PresenceMsg JD.Value


envelope : String -> JE.Value -> JE.Value
envelope kind payload =
    JE.object [ ( "kind", JE.string kind ), ( "payload", payload ) ]


decodeEnvelope : JD.Value -> Result JD.Error Envelope
decodeEnvelope =
    JD.decodeValue
        (JD.field "kind" JD.string
            |> JD.andThen
                (\kind ->
                    case kind of
                        "doc" ->
                            JD.map DocMsg (JD.field "payload" JD.value)

                        "presence" ->
                            JD.map PresenceMsg (JD.field "payload" JD.value)

                        _ ->
                            JD.fail ("unknown envelope kind: " ++ kind)
                )
        )



-- VIEW -----------------------------------------------------------------------


{-| Fire `msg` when the Enter key is pressed in an input, so the add-row fields
submit without reaching for the button.
-}
onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    on "keydown"
        (JD.field "key" JD.string
            |> JD.andThen
                (\key ->
                    if key == "Enter" then
                        JD.succeed msg

                    else
                        JD.fail "not Enter"
                )
        )


{-| The color of a _remote_ peer currently editing `fieldId`, if any. Our own
focus never highlights (you know where your cursor is); only other peers do.
-}
peerEditing : Model -> String -> Maybe String
peerEditing model fieldId =
    Presence.peers model.peers
        |> List.filter (\( rid, _ ) -> rid /= model.me)
        |> List.filter (\( _, c ) -> c.editing == Just fieldId)
        |> List.head
        |> Maybe.map (\( _, c ) -> c.color)


{-| Attributes that tint an input with the editing peer's color when one is
present on `fieldId`, plus the focus/blur handlers that report _our_ editing of
it. Apply to every collaborative input so presence lights up both ways.
-}
fieldAttrs : Model -> String -> List (Html.Attribute Msg)
fieldAttrs model fieldId =
    let
        highlight =
            case peerEditing model fieldId of
                Just color ->
                    [ A.style "box-shadow" ("0 0 0 2px " ++ color)
                    , A.style "border-color" color
                    ]

                Nothing ->
                    []
    in
    onFocus (FocusField fieldId)
        :: onBlur BlurField
        :: highlight


view : Model -> Html Msg
view model =
    let
        -- when previewing an old version, render that read-only snapshot
        shownDoc =
            case model.viewing of
                Just v ->
                    History.checkout v model.doc |> Maybe.withDefault model.doc

                Nothing ->
                    model.doc

        readOnly =
            model.viewing /= Nothing
    in
    case Crdt.read schema shownDoc of
        Ok board ->
            div [ class "app" ]
                [ viewHeader model
                , div [ class "columns" ]
                    [ div [ class "board" ] [ viewBoard readOnly model board ]
                    , div [ class "sidebar" ]
                        [ viewPresence model
                        , viewHistory model
                        ]
                    ]
                ]

        Err err ->
            div [ class "error" ] [ text ("schema read error: " ++ Crdt.errorToString err) ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "header" ]
        [ h1 [] [ text "elm-crdt — collaborative board" ]
        , span [ class "replica" ] [ text ("you are " ++ Crdt.Id.toString model.me) ]
        , span
            [ class
                (if model.connected then
                    "status online"

                 else
                    "status offline"
                )
            ]
            [ text
                (if model.connected then
                    "● online"

                 else
                    "○ offline (edits queue & sync on reconnect)"
                )
            ]
        ]


viewBoard : Bool -> Model -> Board -> Html Msg
viewBoard readOnly model board =
    div []
        [ h2 [] [ text "Title" ]
        , input
            ([ value board.title
             , placeholder "Untitled board"
             , onInput TitleChanged
             , A.disabled readOnly
             ]
                ++ fieldAttrs model titleField
            )
            []
        , h2 [] [ text "Todos" ]
        , ul [ class "todos" ] (List.indexedMap (viewTodo readOnly model) board.todos)
        , if readOnly then
            text ""

          else
            div [ class "add-row" ]
                [ input
                    [ value model.newTodo
                    , placeholder "new todo…"
                    , onInput NewTodoChanged
                    , onEnter AddTodo
                    ]
                    []
                , button [ onClick AddTodo ] [ text "add" ]
                ]
        , h2 [] [ text "Notes" ]
        , viewNotes readOnly model board.notes
        ]


viewTodo : Bool -> Model -> Int -> Todo -> Html Msg
viewTodo readOnly model i todo =
    li [ class "todo" ]
        [ input
            [ A.type_ "checkbox"
            , A.checked todo.done
            , onClick (ToggleTodo i)
            , A.disabled readOnly
            ]
            []
        , input
            ([ value todo.text
             , onInput (TodoTextChanged i)
             , A.disabled readOnly
             ]
                ++ fieldAttrs model (todoField i)
            )
            []
        , if readOnly then
            text ""

          else
            button [ onClick (RemoveTodo i) ] [ text "✕" ]
        ]


viewNotes : Bool -> Model -> Dict String String -> Html Msg
viewNotes readOnly model notes =
    div []
        [ ul [ class "notes" ]
            (Crdt.dictToList notes
                |> List.map
                    (\( k, v ) ->
                        li []
                            [ span [ class "note-key" ] [ text k ]
                            , input
                                ([ value v, onInput (NoteChanged k), A.disabled readOnly ]
                                    ++ fieldAttrs model (noteField k)
                                )
                                []
                            , if readOnly then
                                text ""

                              else
                                button [ onClick (RemoveNote k) ] [ text "✕" ]
                            ]
                    )
            )
        , if readOnly then
            text ""

          else
            div [ class "add-row" ]
                [ input
                    [ value model.newNoteKey
                    , placeholder "note key…"
                    , onInput NewNoteKeyChanged
                    , onEnter AddNote
                    ]
                    []
                , button [ onClick AddNote ] [ text "add note" ]
                ]
        ]


viewPresence : Model -> Html Msg
viewPresence model =
    div [ class "presence" ]
        [ h2 [] [ text "Who's here" ]
        , ul []
            (Presence.peers model.peers
                |> List.map
                    (\( rid, cursor ) ->
                        li [ class "peer" ]
                            [ span
                                [ class "dot", A.style "background" cursor.color ]
                                []
                            , text cursor.name
                            , text
                                (if rid == model.me then
                                    " (you)"

                                 else
                                    ""
                                )
                            , case cursor.editing of
                                Just f ->
                                    span [ class "editing" ] [ text (" editing " ++ f) ]

                                Nothing ->
                                    text ""
                            ]
                    )
            )
        ]


viewHistory : Model -> Html Msg
viewHistory model =
    div [ class "history" ]
        [ h2 [] [ text "History" ]
        , div [ class "add-row" ]
            [ input
                [ value model.checkpointMsg
                , placeholder "checkpoint message…"
                , onInput CheckpointMsgChanged
                ]
                []
            , button [ onClick SaveCheckpoint ] [ text "save checkpoint" ]
            ]
        , div [ class "undo-redo" ]
            [ button [ onClick Undo ] [ text "↶ undo" ]
            , button [ onClick Redo ] [ text "↷ redo" ]
            ]
        , ul [ class "checkpoints" ]
            (History.checkpoints model.doc
                |> List.map (viewCheckpoint model.viewing)
            )
        , case model.viewing of
            Just _ ->
                div [ class "preview-banner" ]
                    [ text "previewing an old version — "
                    , button [ onClick LeavePreview ] [ text "back to latest" ]
                    ]

            Nothing ->
                text ""
        ]


viewCheckpoint : Maybe Version -> Checkpoint -> Html Msg
viewCheckpoint viewing cp =
    let
        v =
            History.checkpointVersion cp

        isViewing =
            viewing == Just v
    in
    li
        [ class
            (if isViewing then
                "checkpoint active"

             else
                "checkpoint"
            )
        ]
        [ span [ class "cp-msg" ] [ text (History.checkpointMessage cp) ]
        , span [ class "cp-author" ] [ text (Crdt.Id.toString (History.checkpointAuthor cp)) ]
        , button [ onClick (PreviewVersion v) ] [ text "preview" ]
        , button [ onClick (RestoreVersion v) ] [ text "restore" ]
        ]



-- DICT ALIAS -----------------------------------------------------------------
-- The schema's `dict` reads back as a standard Elm Dict; we re-expose the
-- ordered listing helper from the library for stable rendering.


type alias Dict k v =
    Crdt.Dict k v



-- SUBSCRIPTIONS / MAIN -------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incoming GotMessage
        , Ports.connection ConnectionChanged
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
