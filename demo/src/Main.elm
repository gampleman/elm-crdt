module Main exposing (main)

{-| Collaborative todo + notes demo for `gampleman/elm-crdt`.

Runs on the **op-log** core (`Crdt.OpDoc`): the document is an operation log,
edits emit ops, sync ships ops over a WebSocket, and history is collaborative
time-travel over the op DAG (`OpDoc.version` / `OpDoc.readAt`).

It exercises the full JSON-like schema (record + list + dict + text + LWW), real
WebSocket networking, live **presence** (who's here, their cursor), and
**collaborative history** (named checkpoints + time-travel preview).

-}

import Browser
import Crdt.Cursor as Cursor exposing (Cursor)
import Crdt.Id exposing (ReplicaId)
import Crdt.OpDoc as OpDoc exposing (Checkpoint, OpDoc, Version)
import Crdt.Path as Path exposing (Path)
import Crdt.Presence as Presence exposing (Presence)
import Crdt.Schema as S exposing (Crdt)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, li, span, text, ul)
import Html.Attributes as A exposing (class, placeholder, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, preventDefaultOn)
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
        |> S.field "todos" .todos (S.movableList todoSchema)
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
type alias Peer =
    { name : String
    , color : String
    , editing : Maybe String -- which field this peer is focused on
    , caret : Maybe Cursor -- stable text caret, if editing a text field
    }


peerCodec : Presence.Codec Peer
peerCodec =
    Presence.codec Peer
        |> Presence.field "name" .name Presence.string
        |> Presence.field "color" .color Presence.string
        |> Presence.optional "editing" .editing Presence.string
        |> Presence.optional "caret" .caret (Presence.custom Cursor.encode Cursor.decoder)
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


notesPath : Path
notesPath =
    Path.root |> Path.field "notes"



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
    , doc : OpDoc Board
    , presence : Presence Peer
    , peers : Presence Peer -- merged view of everyone (incl. self)

    -- transient form state
    , newTodo : String
    , newNoteKey : String

    -- drag-and-drop reorder: the visible index of the todo currently being
    -- dragged (local, ephemeral — never replicated)
    , dragging : Maybe Int

    -- history / version control (checkpoints now live in the doc itself)
    , checkpointMsg : String
    , viewing : Maybe Version -- Just v => time-travel preview (read-only)
    , connected : Bool

    -- delta sync: the version up to which peers already have our ops, so each
    -- broadcast ships only `encodeSince lastSent` instead of the whole log.
    , lastSent : Version
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
            Presence.init me peerCodec
                |> Presence.setLocal
                    { name = flags.name, color = flags.color, editing = Nothing, caret = Nothing }

        doc =
            OpDoc.init me schema
    in
    ( { me = me
      , doc = doc
      , presence = presence
      , peers = presence
      , newTodo = ""
      , newNoteKey = ""
      , dragging = Nothing
      , checkpointMsg = ""
      , viewing = Nothing
      , connected = False
      , lastSent = OpDoc.version doc
      }
    , broadcastPresence presence
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = -- any collaborative text field: carries its path + the new value + the real
      -- DOM caret offset, so the broadcast caret is exact for that field
      TextEdited Path String Int
    | CaretMoved Path Int
      -- todos
    | NewTodoChanged String
    | AddTodo
    | ToggleTodo Int
    | RemoveTodo Int
      -- drag-and-drop reorder
    | DragStart Int
    | DragOver Int
    | DragEnd
      -- notes (dict)
    | NewNoteKeyChanged String
    | AddNote
    | RemoveNote String
      -- presence
    | FocusField String
    | BlurField
      -- history / version control
    | CheckpointMsgChanged String
    | SaveCheckpoint
    | PreviewVersion Version
    | LeavePreview
      -- networking
    | GotMessage JD.Value
    | ConnectionChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextEdited path s caretOffset ->
            editText path s caretOffset model

        CaretMoved path caretOffset ->
            -- caret moved without editing (arrow keys / click): re-publish our
            -- caret at the new offset within that field
            publishCaret path caretOffset model

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
                            |> OpDoc.listAppend todosPath
                                (todoSchema |> S.with (Todo model.newTodo False))
                            |> orKeep model.doc
                in
                pushDoc { model | doc = doc1, newTodo = "" }

        ToggleTodo i ->
            let
                current =
                    OpDoc.read model.doc
                        |> Result.toMaybe
                        |> Maybe.andThen (\b -> List.drop i b.todos |> List.head)
                        |> Maybe.map .done
                        |> Maybe.withDefault False

                doc1 =
                    model.doc
                        |> OpDoc.setBool (todoDonePath i) (not current)
                        |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        RemoveTodo i ->
            let
                doc1 =
                    model.doc |> OpDoc.listRemove todosPath i |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        DragStart i ->
            ( { model | dragging = Just i }, Cmd.none )

        DragOver target ->
            case model.dragging of
                Just from ->
                    if from == target then
                        ( model, Cmd.none )

                    else
                        -- reorder live as the row is dragged over a new slot, so
                        -- the move converges through the same op path as any edit
                        let
                            doc1 =
                                model.doc
                                    |> OpDoc.listMove todosPath from target
                                    |> orKeep model.doc
                        in
                        pushDoc { model | doc = doc1, dragging = Just target }

                Nothing ->
                    ( model, Cmd.none )

        DragEnd ->
            ( { model | dragging = Nothing }, Cmd.none )

        NewNoteKeyChanged s ->
            ( { model | newNoteKey = s }, Cmd.none )

        AddNote ->
            if String.isEmpty (String.trim model.newNoteKey) then
                ( model, Cmd.none )

            else
                let
                    doc1 =
                        model.doc
                            |> OpDoc.setKey notesPath model.newNoteKey (S.text |> S.with "")
                            |> orKeep model.doc
                in
                pushDoc { model | doc = doc1, newNoteKey = "" }

        RemoveNote k ->
            let
                doc1 =
                    model.doc |> OpDoc.removeKey notesPath k |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        FocusField fieldName ->
            setEditing (Just fieldName) model

        BlurField ->
            setEditing Nothing model

        CheckpointMsgChanged s ->
            ( { model | checkpointMsg = s }, Cmd.none )

        SaveCheckpoint ->
            -- capturing a version doesn't change the document, so no broadcast
            let
                label =
                    if String.isEmpty (String.trim model.checkpointMsg) then
                        "checkpoint"

                    else
                        model.checkpointMsg
            in
            ( { model | doc = OpDoc.checkpoint label model.doc, checkpointMsg = "" }
            , Cmd.none
            )

        PreviewVersion v ->
            ( { model | viewing = Just v }, Cmd.none )

        LeavePreview ->
            ( { model | viewing = Nothing }, Cmd.none )

        GotMessage raw ->
            case decodeEnvelope raw of
                Ok (DocMsg incomingJson) ->
                    -- decodeInto merges the peer's ops into our log (idempotent).
                    -- These ops were broadcast to everyone, so advance `lastSent`
                    -- to avoid echoing them back on our next delta.
                    case OpDoc.decodeInto incomingJson model.doc of
                        Ok doc1 ->
                            ( { model | doc = doc1, lastSent = OpDoc.version doc1 }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Ok (PresenceMsg incomingJson) ->
                    case Presence.decode peerCodec incomingJson of
                        Ok incoming ->
                            ( { model | peers = Presence.merge model.peers incoming }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Ok HelloMsg ->
                    -- a peer just joined: send our full op set AND our presence so
                    -- they catch up on both the document and who's here / cursors.
                    ( model
                    , Cmd.batch [ broadcastFull model.doc, broadcastPresence model.presence ]
                    )

                Ok (LeftMsg rid) ->
                    -- the relay says this peer's socket closed: drop it from the
                    -- presence view (and its caret with it).
                    ( { model | peers = Presence.remove rid model.peers }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ConnectionChanged isUp ->
            -- On (re)connect, exchange full state both ways: push our entire op
            -- set (so peers get anything we did offline) and `hello` (so they
            -- push theirs). After this, steady-state edits are deltas again.
            ( { model | connected = isUp, lastSent = OpDoc.version model.doc }
            , if isUp then
                Cmd.batch
                    [ broadcastFull model.doc
                    , sayHello
                    , broadcastPresence model.presence
                    ]

              else
                Cmd.none
            )



-- EDIT / SYNC HELPERS --------------------------------------------------------


{-| Edit any collaborative text field + publish the caret in one step: apply the
text change (the Text CRDT diffs old→new into minimal insert/delete ops so
concurrent typing merges character-wise), then publish a **stable caret** at the
real DOM offset within `path`. Because the caret is a `Crdt.Cursor`, it tracks the
right character on every viewer even as they make their own concurrent edits.
-}
editText : Path -> String -> Int -> Model -> ( Model, Cmd Msg )
editText path newValue caretOffset model =
    let
        doc1 =
            model.doc
                |> OpDoc.setText path newValue
                |> orKeep model.doc

        caret =
            OpDoc.cursorAt path caretOffset doc1 |> Result.toMaybe

        presence1 =
            Presence.updateLocal (\c -> { c | caret = caret }) model.presence

        ( model1, docCmd ) =
            pushDoc
                { model
                    | doc = doc1
                    , presence = presence1
                    , peers = Presence.merge model.peers presence1
                }
    in
    ( model1, Cmd.batch [ docCmd, broadcastPresence presence1 ] )


{-| Re-publish our caret at a new offset within `path` (caret moved, no edit).
-}
publishCaret : Path -> Int -> Model -> ( Model, Cmd Msg )
publishCaret path caretOffset model =
    let
        caret =
            OpDoc.cursorAt path caretOffset model.doc |> Result.toMaybe

        presence1 =
            Presence.updateLocal (\c -> { c | caret = caret }) model.presence
    in
    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
    , broadcastPresence presence1
    )


orKeep : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
orKeep fallback result =
    Result.withDefault fallback result


{-| After any local change, broadcast only the **delta** since our last
broadcast, then advance `lastSent`. While connected every peer sees every
broadcast, so this keeps everyone in sync at edit-size bandwidth; fresh peers are
caught up by the full-state exchange on connect (see `ConnectionChanged` /
`hello`).
-}
pushDoc : Model -> ( Model, Cmd Msg )
pushDoc model =
    let
        now =
            OpDoc.version model.doc
    in
    ( { model | lastSent = now }
    , Ports.outgoing (envelope "doc" (OpDoc.encodeSince model.lastSent model.doc))
    )


{-| Broadcast our entire op set — used for catch-up (connect / answering a
`hello`), the one place full state is needed.
-}
broadcastFull : OpDoc Board -> Cmd Msg
broadcastFull doc =
    Ports.outgoing (envelope "doc" (OpDoc.encode doc))


{-| Announce ourselves so already-present peers send us their full state.
-}
sayHello : Cmd Msg
sayHello =
    Ports.outgoing (envelope "hello" (JE.object []))


broadcastPresence : Presence Peer -> Cmd Msg
broadcastPresence p =
    Ports.outgoing (envelope "presence" (Presence.encode p))


{-| Update which field we're editing: bump our local presence, fold it into the
merged `peers` view so our own row stays current, and broadcast it to others.
-}
setEditing : Maybe String -> Model -> ( Model, Cmd Msg )
setEditing field model =
    let
        -- the input's own mouseup/keyup publishes the caret at the real offset;
        -- here we just track focus and clear the caret on blur.
        presence1 =
            Presence.updateLocal
                (\c ->
                    { c
                        | editing = field
                        , caret =
                            if field == Nothing then
                                Nothing

                            else
                                c.caret
                    }
                )
                model.presence
    in
    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
    , broadcastPresence presence1
    )



-- WIRE ENVELOPE --------------------------------------------------------------


type Envelope
    = DocMsg JD.Value -- a batch of ops (delta or full)
    | PresenceMsg JD.Value
    | HelloMsg -- "I just joined — send me your full state"
    | LeftMsg ReplicaId -- the relay says this peer's socket closed


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

                        "hello" ->
                            JD.succeed HelloMsg

                        "left" ->
                            JD.map (Crdt.Id.replica >> LeftMsg)
                                (JD.at [ "payload", "replica" ] JD.string)

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


{-| The collaborative-text attributes for an input editing `path`: report edits
(value + real caret offset) and caret moves (keyup/mouseup) so we broadcast an
exact caret, plus the focus/blur presence handlers.
-}
textFieldAttrs : Path -> List (Html.Attribute Msg)
textFieldAttrs path =
    [ on "input"
        (JD.map2 (TextEdited path)
            (JD.at [ "target", "value" ] JD.string)
            (JD.at [ "target", "selectionStart" ] JD.int)
        )
    , on "keyup" (JD.map (CaretMoved path) (JD.at [ "target", "selectionStart" ] JD.int))
    , on "mouseup" (JD.map (CaretMoved path) (JD.at [ "target", "selectionStart" ] JD.int))
    ]


{-| Render each _remote_ peer's caret **for the field at `path`** as a thin
colored bar, positioned by the caret's resolved offset. We resolve each peer's
stable `Cursor` against **our own** document, so it lands at the right character
even if our local edits shifted offsets — the point of stable cursors. A peer's
caret only shows in the field it actually points into (compared by `Cursor`'s
target path), so each text input shows only the carets that belong to it.

Horizontal placement uses `ch` units; with the monospace input font 1ch is one
glyph, so the bar lands exactly on the character.

-}
viewFieldCarets : Path -> Model -> List (Html Msg)
viewFieldCarets path model =
    let
        pathTarget =
            OpDoc.cursorAt path 0 model.doc
                |> Result.toMaybe
                |> Maybe.map Cursor.steps
    in
    Presence.peers model.peers
        |> List.filterMap
            (\( rid, peer ) ->
                if rid == model.me then
                    Nothing

                else
                    peer.caret
                        |> Maybe.andThen
                            (\c ->
                                -- only this field's carets: the cursor's target
                                -- path must match the field we're rendering
                                if Just (Cursor.steps c) == pathTarget then
                                    OpDoc.cursorOffset c model.doc
                                        |> Maybe.map (\offset -> ( peer, offset ))

                                else
                                    Nothing
                            )
            )
        |> List.map
            (\( peer, offset ) ->
                -- positioned over the input: 0.5rem left padding + 1px border,
                -- then `offset` character-widths in. Vertical: just inside the
                -- input's top/bottom padding. `ch`-approximation (see docs/03).
                span
                    [ class "remote-caret"
                    , A.style "position" "absolute"
                    , A.style "left" ("calc(0.5rem + 1px + " ++ String.fromInt offset ++ "ch)")
                    , A.style "top" "0.3rem"
                    , A.style "background" peer.color
                    , A.style "width" "2px"
                    , A.style "height" "1.2rem"
                    , A.title (peer.name ++ "'s cursor")
                    ]
                    []
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


{-| Read the board to render: the live document, or a past version when previewing.
-}
readShown : Model -> Result S.Error Board
readShown model =
    case model.viewing of
        Just v ->
            OpDoc.readAt v model.doc

        Nothing ->
            OpDoc.read model.doc


view : Model -> Html Msg
view model =
    let
        readOnly =
            model.viewing /= Nothing
    in
    case readShown model of
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
            div [ class "error" ] [ text ("schema read error: " ++ S.errorToString err) ]


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


{-| A collaborative text input: the field's value, the edit/caret event handlers,
the presence highlight + focus reporting, and any remote peers' carets overlaid
on top — all keyed to this field's `path` and `fieldId`.
-}
viewTextInput : Bool -> Model -> Path -> String -> String -> String -> Html Msg
viewTextInput readOnly model path fieldId currentValue placeholderText =
    div [ class "field-wrap", A.style "position" "relative" ]
        (input
            ([ value currentValue
             , placeholder placeholderText
             , A.disabled readOnly
             ]
                ++ textFieldAttrs path
                ++ fieldAttrs model fieldId
            )
            []
            :: viewFieldCarets path model
        )


viewBoard : Bool -> Model -> Board -> Html Msg
viewBoard readOnly model board =
    div []
        [ h2 [] [ text "Title" ]
        , viewTextInput readOnly model titlePath titleField board.title "Untitled board"
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
    let
        dragging =
            model.dragging == Just i

        -- the row is a drop target while a drag is in progress: hovering over it
        -- reorders live. `preventDefault` on dragover is what makes a drop legal.
        dropAttrs =
            if readOnly then
                []

            else
                [ preventDefaultOn "dragover" (JD.succeed ( DragOver i, True ))
                , A.classList [ ( "dragging", dragging ) ]
                ]
    in
    li (class "todo" :: dropAttrs)
        [ if readOnly then
            text ""

          else
            span
                [ class "drag-handle"
                , A.draggable "true"
                , A.title "drag to reorder"
                , on "dragstart" (JD.succeed (DragStart i))
                , on "dragend" (JD.succeed DragEnd)
                ]
                [ text "⠿" ]
        , input
            [ A.type_ "checkbox"
            , A.checked todo.done
            , onClick (ToggleTodo i)
            , A.disabled readOnly
            ]
            []
        , viewTextInput readOnly model (todoTextPath i) (todoField i) todo.text ""
        , if readOnly then
            text ""

          else
            button [ onClick (RemoveTodo i) ] [ text "✕" ]
        ]


viewNotes : Bool -> Model -> Dict String String -> Html Msg
viewNotes readOnly model notes =
    div []
        [ ul [ class "notes" ]
            (Dict.toList notes
                |> List.map
                    (\( k, v ) ->
                        li []
                            [ span [ class "note-key" ] [ text k ]
                            , viewTextInput readOnly model (notePath k) (noteField k) v ""
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
                , onEnter SaveCheckpoint
                ]
                []
            , button [ onClick SaveCheckpoint ] [ text "save checkpoint" ]
            ]
        , ul [ class "checkpoints" ]
            (List.map (viewCheckpoint model.viewing) (OpDoc.checkpoints model.doc))
        , case model.viewing of
            Just _ ->
                div [ class "preview-banner" ]
                    [ text "time-travelling to an old version — "
                    , button [ onClick LeavePreview ] [ text "back to latest" ]
                    ]

            Nothing ->
                text ""
        ]


viewCheckpoint : Maybe Version -> Checkpoint -> Html Msg
viewCheckpoint viewing cp =
    let
        cpVersion =
            OpDoc.checkpointVersion cp

        isViewing =
            viewing == Just cpVersion
    in
    li
        [ class
            (if isViewing then
                "checkpoint active"

             else
                "checkpoint"
            )
        ]
        [ span [ class "cp-msg" ] [ text (OpDoc.checkpointMessage cp) ]
        , span [ class "cp-author" ] [ text (Crdt.Id.toString (OpDoc.checkpointAuthor cp)) ]
        , button [ onClick (PreviewVersion cpVersion) ] [ text "preview" ]
        ]



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
