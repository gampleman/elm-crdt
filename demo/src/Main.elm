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
import Crdt.Presence as Presence exposing (Presence)
import Crdt.Ref as Ref exposing (Ref)
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
    , status : Status
    , todos : List Todo
    , notes : Dict String String
    }


{-| A board-level lifecycle status — a **sum type**, showcasing `S.custom`. The
`Archived` variant carries a reason (collaborative text), so concurrent edits to
the reason merge character-wise while the active variant itself is LWW.
-}
type Status
    = Planning
    | Active
    | Archived String


type alias Todo =
    { text : String
    , done : Bool
    }


{-| The schema and its **typed refs**, built together with the ref-emitting
builders. `boardDoc.schema` is the `Crdt` for `OpDoc.init`; `boardDoc.refs` holds a
compile-checked `Ref` per field — every edit in `update` goes through these instead
of a stringly-typed `Path`.
-}
type alias BoardRefs =
    { title : Ref Board S.Settable String
    , status : Ref Board (S.Variants Status) Status
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    , notes : Ref Board (S.DictK S.Settable String) (Dict String String)
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "status" .status statusDoc.schema
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.field "notes" .notes (S.dict S.text)
        |> Ref.build


schema : Crdt S.Nested Board
schema =
    boardDoc.schema


refs : BoardRefs
refs =
    boardDoc.refs


type alias StatusRefs =
    { archived : Ref Status S.Settable String }


statusDoc : Ref.CustomRefs Status StatusRefs
statusDoc =
    Ref.custom
        (\planning active archived value ->
            case value of
                Planning ->
                    planning

                Active ->
                    active

                Archived reason ->
                    archived reason
        )
        StatusRefs
        |> Ref.variant0 "planning" Planning
        |> Ref.variant0 "active" Active
        |> Ref.variant1 "archived" Archived S.text
        |> Ref.buildCustom


{-| The reason text inside the `Archived` variant. Editing it applies only while the
board is actually `Archived` (silent no-op otherwise).
-}
archivedReasonRef : Ref Board S.Settable String
archivedReasonRef =
    refs.status |> Ref.at statusDoc.refs.archived


type alias TodoRefs =
    { text : Ref Todo S.Settable String
    , done : Ref Todo S.Settable Bool
    }


todoDoc : Ref.RecordRefs Todo TodoRefs
todoDoc =
    Ref.record Todo TodoRefs
        |> Ref.field "text" .text S.text
        |> Ref.field "done" .done S.bool
        |> Ref.build


{-| A text ref into todo `i`'s `text` field — `todos[i].text`, composed from the
list ref, an element ref, and the field ref. Drives that todo's text input + caret.
-}
todoTextRef : Int -> Ref Board S.Settable String
todoTextRef i =
    refs.todos |> Ref.index i todoDoc.schema |> Ref.at todoDoc.refs.text


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



-- REFS (typed accessors into the document) -----------------------------------
-- Composed from the schema-emitted `refs`; every edit goes through these, so a
-- wrong field name or wrong-kind op is a compile error, not a runtime one.


todoDoneRef : Int -> Ref Board S.Settable Bool
todoDoneRef i =
    refs.todos |> Ref.index i todoDoc.schema |> Ref.at todoDoc.refs.done


{-| A text ref into note `k`'s value.
-}
noteRef : String -> Ref Board S.Settable String
noteRef k =
    refs.notes |> Ref.key k S.text



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

    -- the document version captured at the start of a drag, so the whole reorder
    -- (which emits a move op per slot crossed) records as ONE undo step on drop
    , dragStartVersion : Maybe Version

    -- version captured when a text field gained focus, so a whole typing session
    -- (many keystroke ops) collapses into ONE undo step when the field blurs
    , editStartVersion : Maybe Version

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
      , dragStartVersion = Nothing
      , editStartVersion = Nothing
      , checkpointMsg = ""
      , viewing = Nothing
      , connected = False
      , lastSent = OpDoc.version doc
      }
    , broadcastPresence presence
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = -- any collaborative text field: carries a typed text `Ref` + the new value +
      -- the real DOM caret offset, so the broadcast caret is exact for that field
      TextEdited (Ref Board S.Settable String) String Int
    | CaretMoved (Ref Board S.Settable String) Int
      -- todos
    | NewTodoChanged String
    | AddTodo
    | ToggleTodo Int
    | RemoveTodo Int
      -- board status (sum type)
    | SetStatus Status
    | ArchivedReasonEdited String Int
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
    | Scrub Int
    | RestoreHere
    | Undo
    | Redo
      -- networking
    | GotMessage JD.Value
    | ConnectionChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextEdited textRef s caretOffset ->
            editText textRef s caretOffset model

        CaretMoved textRef caretOffset ->
            -- caret moved without editing (arrow keys / click): re-publish our
            -- caret at the new offset within that field
            publishCaret textRef caretOffset model

        NewTodoChanged s ->
            ( { model | newTodo = s }, Cmd.none )

        AddTodo ->
            if String.isEmpty (String.trim model.newTodo) then
                ( model, Cmd.none )

            else
                let
                    before =
                        OpDoc.version model.doc

                    -- append a fresh todo at the end of the list
                    doc1 =
                        Ref.append todoDoc.schema (Todo model.newTodo False) refs.todos model.doc
                            |> orKeep model.doc
                in
                recordPush before { model | doc = doc1, newTodo = "" }

        ToggleTodo i ->
            let
                before =
                    OpDoc.version model.doc

                current =
                    OpDoc.read model.doc
                        |> Result.toMaybe
                        |> Maybe.andThen (\b -> List.drop i b.todos |> List.head)
                        |> Maybe.map .done
                        |> Maybe.withDefault False

                doc1 =
                    Ref.set (todoDoneRef i) (not current) model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        RemoveTodo i ->
            let
                before =
                    OpDoc.version model.doc

                doc1 =
                    Ref.remove i refs.todos model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        SetStatus newStatus ->
            -- switch the board's status variant through the typed Ref API
            let
                before =
                    OpDoc.version model.doc

                doc1 =
                    Ref.switch refs.status newStatus model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        ArchivedReasonEdited s _ ->
            -- edit the Archived reason text; applies only while status is Archived.
            -- (Character-wise merge is preserved because it's a text leaf.)
            let
                doc1 =
                    Ref.set archivedReasonRef s model.doc |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        DragStart i ->
            -- capture the version now so the whole drag undoes as one step
            ( { model | dragging = Just i, dragStartVersion = Just (OpDoc.version model.doc) }, Cmd.none )

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
                                Ref.move from target refs.todos model.doc
                                    |> orKeep model.doc
                        in
                        pushDoc { model | doc = doc1, dragging = Just target }

                Nothing ->
                    ( model, Cmd.none )

        DragEnd ->
            -- record the entire reorder (all the per-slot moves) as one undo step
            case model.dragStartVersion of
                Just before ->
                    ( { model
                        | dragging = Nothing
                        , dragStartVersion = Nothing
                        , doc = OpDoc.recordEdit before model.doc
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | dragging = Nothing }, Cmd.none )

        NewNoteKeyChanged s ->
            ( { model | newNoteKey = s }, Cmd.none )

        AddNote ->
            if String.isEmpty (String.trim model.newNoteKey) then
                ( model, Cmd.none )

            else
                let
                    before =
                        OpDoc.version model.doc

                    doc1 =
                        Ref.setKey S.text model.newNoteKey "" refs.notes model.doc
                            |> orKeep model.doc
                in
                recordPush before { model | doc = doc1, newNoteKey = "" }

        RemoveNote k ->
            let
                before =
                    OpDoc.version model.doc

                doc1 =
                    Ref.removeKey k refs.notes model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        FocusField fieldName ->
            -- mark the start of a typing session so it undoes as one step
            setEditing (Just fieldName) { model | editStartVersion = Just (OpDoc.version model.doc) }

        BlurField ->
            -- close the typing session: record everything typed since focus as one
            -- undo step (no-op if nothing changed)
            let
                model1 =
                    case model.editStartVersion of
                        Just before ->
                            { model | doc = OpDoc.recordEdit before model.doc, editStartVersion = Nothing }

                        Nothing ->
                            model
            in
            setEditing Nothing model1

        Undo ->
            if OpDoc.canUndo model.doc then
                pushDoc { model | doc = OpDoc.undo model.doc, viewing = Nothing }

            else
                ( model, Cmd.none )

        Redo ->
            if OpDoc.canRedo model.doc then
                pushDoc { model | doc = OpDoc.redo model.doc, viewing = Nothing }

            else
                ( model, Cmd.none )

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

        Scrub n ->
            -- scrubbing previews the version after the first `n` ops; dragging to
            -- the end (n == historyLength) drops back to the live document
            if n >= OpDoc.historyLength model.doc then
                ( { model | viewing = Nothing }, Cmd.none )

            else
                ( { model | viewing = Just (OpDoc.versionAt n model.doc) }, Cmd.none )

        RestoreHere ->
            case model.viewing of
                Just v ->
                    -- restore emits fresh winning ops, so it syncs like any edit;
                    -- leave the preview and broadcast the revert
                    pushDoc { model | doc = OpDoc.restoreTo v model.doc, viewing = Nothing }

                Nothing ->
                    ( model, Cmd.none )

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
text change (`Ref.set` on a text ref diffs old→new into minimal insert/delete ops so
concurrent typing merges character-wise), then publish a **stable caret** at the
real DOM offset within that ref. Because the caret is a `Crdt.Cursor`, it tracks the
right character on every viewer even as they make their own concurrent edits.
-}
editText : Ref Board S.Settable String -> String -> Int -> Model -> ( Model, Cmd Msg )
editText textRef newValue caretOffset model =
    let
        doc1 =
            Ref.set textRef newValue model.doc
                |> orKeep model.doc

        caret =
            Ref.cursorAt textRef caretOffset doc1 |> Result.toMaybe

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


{-| Re-publish our caret at a new offset within a text ref (caret moved, no edit).
-}
publishCaret : Ref Board S.Settable String -> Int -> Model -> ( Model, Cmd Msg )
publishCaret textRef caretOffset model =
    let
        caret =
            Ref.cursorAt textRef caretOffset model.doc |> Result.toMaybe

        presence1 =
            Presence.updateLocal (\c -> { c | caret = caret }) model.presence
    in
    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
    , broadcastPresence presence1
    )


orKeep : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
orKeep fallback result =
    Result.withDefault fallback result


{-| Record the just-made edit on the local undo stack (bracketing it with the
`before` version), then broadcast it. `model.doc` already holds the post-edit
document; `before` is the version captured before the edit. Used by every discrete
todo/note action so each is one Ctrl-Z step.
-}
recordPush : Version -> Model -> ( Model, Cmd Msg )
recordPush before model =
    pushDoc { model | doc = OpDoc.recordEdit before model.doc }


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


{-| The collaborative-text attributes for an input editing a text `Ref`: report
edits (value + real caret offset) and caret moves (keyup/mouseup) so we broadcast an
exact caret, plus the focus/blur presence handlers.
-}
textFieldAttrs : Ref Board S.Settable String -> List (Html.Attribute Msg)
textFieldAttrs textRef =
    [ on "input"
        (JD.map2 (TextEdited textRef)
            (JD.at [ "target", "value" ] JD.string)
            (JD.at [ "target", "selectionStart" ] JD.int)
        )
    , on "keyup" (JD.map (CaretMoved textRef) (JD.at [ "target", "selectionStart" ] JD.int))
    , on "mouseup" (JD.map (CaretMoved textRef) (JD.at [ "target", "selectionStart" ] JD.int))
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
viewFieldCarets : Ref Board S.Settable String -> Model -> List (Html Msg)
viewFieldCarets textRef model =
    let
        pathTarget =
            Ref.cursorAt textRef 0 model.doc
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
on top — all keyed to this field's text `Ref` and `fieldId`.
-}
viewTextInput : Bool -> Model -> Ref Board S.Settable String -> String -> String -> String -> Html Msg
viewTextInput readOnly model textRef fieldId currentValue placeholderText =
    div [ class "field-wrap", A.style "position" "relative" ]
        (input
            ([ value currentValue
             , placeholder placeholderText
             , A.disabled readOnly
             ]
                ++ textFieldAttrs textRef
                ++ fieldAttrs model fieldId
            )
            []
            :: viewFieldCarets textRef model
        )


viewBoard : Bool -> Model -> Board -> Html Msg
viewBoard readOnly model board =
    div []
        [ h2 [] [ text "Title" ]
        , viewTextInput readOnly model refs.title titleField board.title "Untitled board"
        , h2 [] [ text "Status" ]
        , viewStatus readOnly model board.status
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


{-| The board status: a sum type rendered as three mutually-exclusive buttons
(switching the active variant), plus a reason input shown only for `Archived`.
Demonstrates `S.custom` + the typed `Crdt.Ref` write API end to end.
-}
viewStatus : Bool -> Model -> Status -> Html Msg
viewStatus readOnly model status =
    let
        pill : String -> Status -> Html Msg
        pill label variant =
            button
                [ class
                    (if sameVariant status variant then
                        "status-pill active"

                     else
                        "status-pill"
                    )
                , A.disabled readOnly
                , onClick (SetStatus variant)
                ]
                [ text label ]
    in
    div []
        [ div [ class "status-pills" ]
            [ pill "Planning" Planning
            , pill "Active" Active
            , pill "Archived" (Archived "")
            ]
        , case status of
            Archived reason ->
                div [ class "field-wrap", A.style "position" "relative", A.style "margin-top" "0.5rem" ]
                    (input
                        ([ value reason
                         , placeholder "why archived?"
                         , A.disabled readOnly
                         , on "input"
                            (JD.map2 ArchivedReasonEdited
                                (JD.at [ "target", "value" ] JD.string)
                                (JD.at [ "target", "selectionStart" ] JD.int)
                            )
                         ]
                            ++ fieldAttrs model archivedField
                        )
                        []
                        :: []
                    )

            _ ->
                text ""
        ]


{-| Whether two `Status` values are the same _variant_ (ignoring payload), so the
"Archived" pill highlights regardless of the current reason text.
-}
sameVariant : Status -> Status -> Bool
sameVariant a b =
    case ( a, b ) of
        ( Planning, Planning ) ->
            True

        ( Active, Active ) ->
            True

        ( Archived _, Archived _ ) ->
            True

        _ ->
            False


archivedField : String
archivedField =
    "status:archived"


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
        , viewTextInput readOnly model (todoTextRef i) (todoField i) todo.text ""
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
                            , viewTextInput readOnly model (noteRef k) (noteField k) v ""
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
        , div [ class "undo-row" ]
            [ button
                [ onClick Undo, A.disabled (not (OpDoc.canUndo model.doc)), A.title "undo your last edit" ]
                [ text "↶ undo" ]
            , button
                [ onClick Redo, A.disabled (not (OpDoc.canRedo model.doc)), A.title "redo" ]
                [ text "↷ redo" ]
            ]
        , viewScrubber model
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


{-| A slider over the document's linear op history. Dragging it previews the state
at that step (`OpDoc.versionAt`); the slider sits at the far right (live) when not
previewing. While parked in the past, a "restore to here" button rewinds the live
document to that point — as new ops, so the revert syncs to every peer.
-}
viewScrubber : Model -> Html Msg
viewScrubber model =
    let
        len =
            OpDoc.historyLength model.doc

        pos =
            scrubPosition model

        previewing =
            model.viewing /= Nothing
    in
    if len == 0 then
        div [ class "scrubber empty" ] [ text "no history yet — make an edit" ]

    else
        div [ class "scrubber" ]
            [ input
                [ A.type_ "range"
                , A.min "0"
                , A.max (String.fromInt len)
                , value (String.fromInt pos)
                , A.step "1"
                , class "scrub-range"
                , on "input"
                    (JD.at [ "target", "value" ] JD.string
                        |> JD.map (\s -> Scrub (String.toInt s |> Maybe.withDefault len))
                    )
                ]
                []
            , div [ class "scrub-row" ]
                [ span [ class "scrub-label" ]
                    [ text
                        (if previewing then
                            "step " ++ String.fromInt pos ++ " of " ++ String.fromInt len

                         else
                            "live (" ++ String.fromInt len ++ " edits)"
                        )
                    ]
                , if previewing then
                    button [ class "restore-btn", onClick RestoreHere ] [ text "restore to here" ]

                  else
                    text ""
                ]
            ]


{-| The slider's current step: the live end unless we're previewing a version
reachable by scrubbing, in which case the matching step. (Checkpoint previews that
don't line up with a scrub step just leave the thumb at the live end.)
-}
scrubPosition : Model -> Int
scrubPosition model =
    let
        len =
            OpDoc.historyLength model.doc
    in
    case model.viewing of
        Nothing ->
            len

        Just v ->
            List.range 0 len
                |> List.filter (\n -> OpDoc.versionAt n model.doc == v)
                |> List.head
                |> Maybe.withDefault len


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
