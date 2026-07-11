module Main exposing (main)

{-| Collaborative workspace demo for `gampleman/elm-crdt`, organized into four tabs:
**Todos** (movable list), **Files** (a dict of rich-text docs edited in TipTap),
**Outline** (movable tree), and **Settings** (title + status sum-type + likes
counter). The active tab is local view state but is broadcast on the presence
channel, so peers can see where everyone is.

Runs on the **op-log** core (`Crdt.OpDoc`): the document is an operation log,
edits emit ops, sync ships ops over a WebSocket, and history is collaborative
time-travel over the op DAG (`OpDoc.version` / `OpDoc.readAt`).

It exercises the full JSON-like schema (record + movable list + dict + text + rich
text + tree + sum type + counter + LWW), real WebSocket networking, live
**presence** (who's here, their tab + cursor), and **collaborative history** (named
checkpoints + time-travel preview).

-}

import Browser
import Crdt.Cursor as Cursor exposing (Cursor)
import Crdt.Id exposing (OpId, ReplicaId)
import Crdt.OpDoc as OpDoc exposing (Checkpoint, OpDoc, Version)
import Crdt.Presence as Presence exposing (Presence)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText as RichText exposing (Block, Span)
import Crdt.Schema as S exposing (Crdt)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, li, span, text, ul)
import Html.Attributes as A exposing (class, placeholder, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, preventDefaultOn)
import Html.Keyed
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
    , files : Dict String (List Span)
    , outline : Tree.Forest Node
    , likes : Int
    }


{-| A node in the collaborative **outline** — a movable tree (`S.tree`). Each node
carries editable text; nodes can be nested under one another and re-parented, and
concurrent re-parents that would form a cycle converge safely.
-}
type alias Node =
    { text : String }


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
    , files : Ref Board (S.DictK S.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (S.TreeK S.Nested Node) (Tree.Forest Node)
    , likes : Ref Board S.Counter Int
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "status" .status statusDoc.schema
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.field "files" .files (S.dict S.richText)
        |> Ref.field "outline" .outline (S.tree nodeDoc.schema)
        |> Ref.field "likes" .likes S.counter
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


type alias NodeRefs =
    { text : Ref Node S.Settable String }


nodeDoc : Ref.RecordRefs Node NodeRefs
nodeDoc =
    Ref.record Node NodeRefs
        |> Ref.field "text" .text S.text
        |> Ref.build


{-| A text ref into outline node `id`'s `text` field, composed from the tree ref, a
node ref (by stable id), and the field ref. Drives that node's text input + caret.
-}
outlineTextRef : OpId -> Ref Board S.Settable String
outlineTextRef id =
    refs.outline |> Ref.treeNode id nodeDoc.schema |> Ref.at nodeDoc.refs.text


{-| Per-peer ephemeral state. Never merged into the document — it lives on the
separate presence channel and expires when a peer goes quiet. `tab` is the tab the
peer is currently viewing (shared awareness, so the peer list can show who is where);
which tab _you_ see is still your own local choice, it's just also broadcast.
-}
type alias Peer =
    { name : String
    , color : String
    , tab : String -- the tab this peer is viewing (see `tabId`)
    , editing : Maybe String -- which field this peer is focused on
    , caret : Maybe Cursor -- stable text caret, if editing a text field
    }


peerCodec : Presence.Codec Peer
peerCodec =
    Presence.codec Peer
        |> Presence.field "name" .name Presence.string
        |> Presence.field "color" .color Presence.string
        |> Presence.field "tab" .tab Presence.string
        |> Presence.optional "editing" .editing Presence.string
        |> Presence.optional "caret" .caret (Presence.custom Cursor.encode Cursor.decoder)
        |> Presence.buildCodec



-- REFS (typed accessors into the document) -----------------------------------
-- Composed from the schema-emitted `refs`; every edit goes through these, so a
-- wrong field name or wrong-kind op is a compile error, not a runtime one.


todoDoneRef : Int -> Ref Board S.Settable Bool
todoDoneRef i =
    refs.todos |> Ref.index i todoDoc.schema |> Ref.at todoDoc.refs.done


{-| A rich-text ref into file `k`'s contents (a dict of rich text). Drives the
TipTap editor and mark commands for the open file.
-}
fileRef : String -> Ref Board S.RichK (List Span)
fileRef k =
    refs.files |> Ref.key k S.richText



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


fileField : String -> String
fileField k =
    "file:" ++ k


outlineField : OpId -> String
outlineField id =
    "outline:" ++ Crdt.Id.opIdToString id



-- MODEL ----------------------------------------------------------------------


type alias Model =
    { me : ReplicaId
    , doc : OpDoc Board
    , presence : Presence Peer
    , peers : Presence Peer -- merged view of everyone (incl. self)

    -- which tab is showing. This is the viewer's own choice (local view state — all
    -- tabs' documents keep converging behind whichever is hidden), but it is also
    -- broadcast on the presence channel so peers can see where everyone is.
    , tab : Tab

    -- Files tab: which file is open in the editor (local view state — a stored
    -- selection, not part of the doc). `Nothing` = no file open.
    , selectedFile : Maybe String

    -- transient form state
    , newTodo : String
    , newFileName : String

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


{-| The demo surfaces, selected by tabs. Which tab _you_ see is your own choice, but
it is also broadcast on the presence channel so peers can see where everyone is.
-}
type Tab
    = TodosTab
    | FilesTab
    | OutlineTab
    | SettingsTab


allTabs : List Tab
allTabs =
    [ TodosTab, FilesTab, OutlineTab, SettingsTab ]


{-| A stable id for a tab, used both as the presence payload and to render peers'
locations. Paired with `tabFromId` for decoding presence.
-}
tabId : Tab -> String
tabId tab =
    case tab of
        TodosTab ->
            "todos"

        FilesTab ->
            "files"

        OutlineTab ->
            "outline"

        SettingsTab ->
            "settings"


tabLabel : Tab -> String
tabLabel tab =
    case tab of
        TodosTab ->
            "Todos"

        FilesTab ->
            "Files"

        OutlineTab ->
            "Outline"

        SettingsTab ->
            "Settings"


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
                    { name = flags.name, color = flags.color, tab = tabId TodosTab, editing = Nothing, caret = Nothing }

        doc =
            OpDoc.init me schema
    in
    ( { me = me
      , doc = doc
      , presence = presence
      , peers = presence
      , tab = TodosTab
      , selectedFile = Nothing
      , newTodo = ""
      , newFileName = ""
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
      -- likes (counter)
    | AddLike
      -- files (dict of rich text)
    | NewFileNameChanged String
    | CreateFile
    | OpenFile String
    | CloseFile
    | RemoveFile String
      -- outline (movable tree)
    | AddOutlineRoot
    | AddOutlineChild OpId
    | IndentNode OpId (Maybe OpId)
    | OutdentNode OpId (Maybe OpId)
    | RemoveNode OpId
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
      -- tabs (local view state)
    | SwitchTab Tab
      -- rich-text editor (ProseMirror via custom element)
    | RichTextInput JD.Value
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

        AddLike ->
            let
                before =
                    OpDoc.version model.doc

                doc1 =
                    Ref.increment refs.likes 1 model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        NewFileNameChanged s ->
            ( { model | newFileName = s }, Cmd.none )

        CreateFile ->
            let
                name =
                    String.trim model.newFileName
            in
            if String.isEmpty name then
                ( model, Cmd.none )

            else
                let
                    before =
                        OpDoc.version model.doc

                    -- seed an empty rich-text file at this key, then open it
                    doc1 =
                        Ref.setKey S.richText name [] refs.files model.doc
                            |> orKeep model.doc
                in
                recordPush before
                    { model | doc = doc1, newFileName = "", selectedFile = Just name }
                    |> andRenderEditor doc1

        OpenFile k ->
            -- purely local: open a file into the editor, then push its spans down
            ( { model | selectedFile = Just k }
            , renderEditorFor k model.doc
            )

        CloseFile ->
            -- purely local: back to the file list
            ( { model | selectedFile = Nothing }, Cmd.none )

        RemoveFile k ->
            let
                before =
                    OpDoc.version model.doc

                doc1 =
                    Ref.removeKey k refs.files model.doc |> orKeep model.doc

                -- close the file if it was the open one
                selected =
                    if model.selectedFile == Just k then
                        Nothing

                    else
                        model.selectedFile
            in
            recordPush before { model | doc = doc1, selectedFile = selected }

        AddOutlineRoot ->
            outlineEdit (Ref.addChild nodeDoc.schema (Node "") Nothing refs.outline) model

        AddOutlineChild parent ->
            outlineEdit (Ref.addChild nodeDoc.schema (Node "") (Just parent) refs.outline) model

        IndentNode node maybePrev ->
            -- nest under the preceding sibling (Nothing = no previous sibling; no-op)
            case maybePrev of
                Just prev ->
                    outlineEdit (Ref.moveInto node (Just prev) refs.outline) model

                Nothing ->
                    ( model, Cmd.none )

        OutdentNode node maybeGrandparent ->
            -- promote to sit under the parent's parent (Nothing = already a root)
            outlineEdit (Ref.moveInto node maybeGrandparent refs.outline) model

        RemoveNode node ->
            outlineEdit (Ref.removeNode node refs.outline) model

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
                pushDocRerendering model.doc { model | doc = OpDoc.undo model.doc, viewing = Nothing }

            else
                ( model, Cmd.none )

        Redo ->
            if OpDoc.canRedo model.doc then
                pushDocRerendering model.doc { model | doc = OpDoc.redo model.doc, viewing = Nothing }

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
                    pushDocRerendering model.doc { model | doc = OpDoc.restoreTo v model.doc, viewing = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        SwitchTab tab ->
            -- flip the visible tab (the viewer's own choice) AND broadcast it as
            -- presence so peers see where we are. If we're opening the Files tab with
            -- a file already selected, push its spans to a freshly-mounted editor.
            let
                presence1 =
                    Presence.updateLocal (\c -> { c | tab = tabId tab }) model.presence
            in
            ( { model | tab = tab, presence = presence1, peers = Presence.merge model.peers presence1 }
            , Cmd.batch
                [ broadcastPresence presence1
                , case ( tab, model.selectedFile ) of
                    ( FilesTab, Just k ) ->
                        renderEditorFor k model.doc

                    _ ->
                        Cmd.none
                ]
            )

        RichTextInput raw ->
            case ( model.selectedFile, JD.decodeValue richTextIntentDecoder raw ) of
                ( Just file, Ok intent ) ->
                    let
                        before =
                            OpDoc.version model.doc

                        doc1 =
                            applyRichTextIntent file intent model.doc |> orKeep model.doc

                        ( m1, syncCmd ) =
                            recordPush before { model | doc = doc1 }
                    in
                    -- Text/mark intents were already applied optimistically by
                    -- ProseMirror, so re-rendering would fight the caret. Block
                    -- intents (split/merge/setType/indent/outdent) were preventDefault'd
                    -- in the editor — it hasn't changed yet — so those we must push back.
                    if isBlockIntent intent then
                        ( m1, Cmd.batch [ syncCmd, renderEditorFor file doc1 ] )

                    else
                        ( m1, syncCmd )

                _ ->
                    ( model, Cmd.none )

        GotMessage raw ->
            case decodeEnvelope raw of
                Ok (DocMsg incomingJson) ->
                    -- decodeInto merges the peer's ops into our log (idempotent).
                    -- These ops were broadcast to everyone, so advance `lastSent`
                    -- to avoid echoing them back on our next delta.
                    case OpDoc.decodeInto incomingJson model.doc of
                        Ok doc1 ->
                            -- a remote edit may have changed the open file; push the
                            -- new spans to the editor so ProseMirror reconciles.
                            ( { model | doc = doc1, lastSent = OpDoc.version doc1 }
                            , renderEditorIfChanged model.selectedFile model.doc doc1
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


{-| Apply one outline (tree) edit, record it as a single undo step, and broadcast.
-}
outlineEdit : (OpDoc Board -> Result OpDoc.Error (OpDoc Board)) -> Model -> ( Model, Cmd Msg )
outlineEdit edit model =
    let
        before =
            OpDoc.version model.doc
    in
    recordPush before { model | doc = edit model.doc |> orKeep model.doc }


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


{-| Like `pushDoc`, but also re-render the editor if the **open file's** contents
changed (used by undo/redo/restore, which can rewrite the doc under the editor's
feet). `old` is the pre-change doc for the diff.
-}
pushDocRerendering : OpDoc Board -> Model -> ( Model, Cmd Msg )
pushDocRerendering old model =
    let
        ( m1, syncCmd ) =
            pushDoc model
    in
    ( m1, Cmd.batch [ syncCmd, renderEditorIfChanged model.selectedFile old model.doc ] )


{-| Chain an editor re-render for file `k` onto an existing `(model, cmd)` (used
after `CreateFile`, which opens the new file).
-}
andRenderEditor : OpDoc Board -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andRenderEditor doc ( model, cmd ) =
    ( model
    , Cmd.batch
        [ cmd
        , case model.selectedFile of
            Just k ->
                renderEditorFor k doc

            Nothing ->
                Cmd.none
        ]
    )


{-| Push file `k`'s current blocks to the ProseMirror editor.
-}
renderEditorFor : String -> OpDoc Board -> Cmd Msg
renderEditorFor k doc =
    Ports.renderRichText (encodeBlocks (fileBlocks k doc))


{-| Re-render the editor only if the **open file's** contents changed between `old`
and `new` — avoids churning ProseMirror (and its caret) on unrelated edits.
-}
renderEditorIfChanged : Maybe String -> OpDoc Board -> OpDoc Board -> Cmd Msg
renderEditorIfChanged selected old new =
    case selected of
        Just k ->
            if fileBlocks k old == fileBlocks k new then
                Cmd.none

            else
                renderEditorFor k new

        Nothing ->
            Cmd.none


{-| File `k`'s current blocks (empty if the file or doc read is absent).
-}
fileBlocks : String -> OpDoc Board -> List Block
fileBlocks k doc =
    Ref.readBlocks (fileRef k) doc |> Result.withDefault []


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



-- RICH TEXT EDITOR (ProseMirror bridge) --------------------------------------


{-| An edit intent from the ProseMirror editor. Text/mark intents carry document-wide
character offsets (over the flattened text of all blocks); block intents identify a
block by its index (0 = leading block) — Elm resolves the index to a marker `OpId` via
`OpDoc.readBlocks` before calling the block edits, which are marker-addressed.

The editor sends a mark intent's `value` as `true` for a boolean mark being set, a
string for a value mark (link href), or `null` to clear — so set-vs-clear turns on
whether `value` is `null`, not on its type (an earlier bug decoded a boolean set as a
clear because `true` isn't a string).

-}
type RichTextIntent
    = TextIntent { blockIndex : Int, text : String }
    | SetMark { type_ : String, value : RichText.MarkValue, from : Int, to : Int }
    | ClearMark { type_ : String, from : Int, to : Int }
    | Split { blockIndex : Int, charOffset : Int }
    | Merge { blockIndex : Int }
    | SetType { blockIndex : Int, type_ : Maybe String }
    | Indent { blockIndex : Int }
    | Outdent { blockIndex : Int }
    | Reconcile (List BlockShape)


{-| The desired shape of one block from the editor, for a `Reconcile`: its type, depth,
and plain text (marks are re-applied by the editor's own mark intents, not here).
-}
type alias BlockShape =
    { type_ : String, depth : Int, text : String }


richTextIntentDecoder : JD.Decoder RichTextIntent
richTextIntentDecoder =
    JD.field "tag" JD.string
        |> JD.andThen
            (\tag ->
                case tag of
                    "text" ->
                        JD.map2 (\bi t -> TextIntent { blockIndex = bi, text = t })
                            (JD.field "blockIndex" JD.int)
                            (JD.field "text" JD.string)

                    "mark" ->
                        JD.map4 (\t v f to -> markIntent t v f to)
                            (JD.field "type" JD.string)
                            (JD.field "value" JD.value)
                            (JD.field "from" JD.int)
                            (JD.field "to" JD.int)

                    "split" ->
                        JD.map2 (\bi off -> Split { blockIndex = bi, charOffset = off })
                            (JD.field "blockIndex" JD.int)
                            (JD.field "charOffset" JD.int)

                    "merge" ->
                        JD.map (\bi -> Merge { blockIndex = bi }) (JD.field "blockIndex" JD.int)

                    "setType" ->
                        JD.map2 (\bi t -> SetType { blockIndex = bi, type_ = t })
                            (JD.field "blockIndex" JD.int)
                            (JD.maybe (JD.field "type" JD.string))

                    "indent" ->
                        JD.map (\bi -> Indent { blockIndex = bi }) (JD.field "blockIndex" JD.int)

                    "outdent" ->
                        JD.map (\bi -> Outdent { blockIndex = bi }) (JD.field "blockIndex" JD.int)

                    "reconcile" ->
                        JD.map Reconcile (JD.field "blocks" (JD.list blockShapeDecoder))

                    _ ->
                        JD.fail ("unknown rich-text intent: " ++ tag)
            )


blockShapeDecoder : JD.Decoder BlockShape
blockShapeDecoder =
    JD.map3 BlockShape
        (JD.field "type" JD.string)
        (JD.field "depth" JD.int)
        (JD.field "text" JD.string)


{-| Whether an intent changes block _structure_ (split/merge/type/indent/outdent).
The editor preventDefaults these and only emits the intent, so after applying we must
push the new blocks back; text/mark intents PM already applied, so we must not.
-}
isBlockIntent : RichTextIntent -> Bool
isBlockIntent intent =
    case intent of
        TextIntent _ ->
            False

        SetMark _ ->
            False

        ClearMark _ ->
            False

        Split _ ->
            -- ProseMirror performs Enter/Backspace splits & merges natively now, so the
            -- editor is already correct; mirroring them to the CRDT must NOT re-render
            -- (that raced with text typed right after — the "type d after Enter" bug).
            False

        Merge _ ->
            False

        SetType _ ->
            True

        Indent _ ->
            True

        Outdent _ ->
            True

        Reconcile _ ->
            -- A reconcile mirrors a structural edit ProseMirror ALREADY applied
            -- natively (paste / multi-block delete), so the editor is already correct —
            -- a re-render would fight the caret and could clobber text typed right after.
            False


{-| Build a set/clear mark intent from the decoded `value`: `null` clears; a string
is a value mark; anything else (i.e. `true`) is a boolean flag.
-}
markIntent : String -> JD.Value -> Int -> Int -> RichTextIntent
markIntent type_ rawValue from to =
    case JD.decodeValue (JD.nullable JD.string) rawValue of
        Ok Nothing ->
            -- `null` → clear. (A JSON `true` also decodes here as Nothing under the
            -- string decoder, so guard on it explicitly below.)
            if isJsonNull rawValue then
                ClearMark { type_ = type_, from = from, to = to }

            else
                SetMark { type_ = type_, value = RichText.Flag, from = from, to = to }

        Ok (Just s) ->
            SetMark { type_ = type_, value = RichText.Value s, from = from, to = to }

        Err _ ->
            SetMark { type_ = type_, value = RichText.Flag, from = from, to = to }


isJsonNull : JD.Value -> Bool
isJsonNull v =
    JD.decodeValue (JD.null True) v == Ok True


{-| Apply a rich-text intent to file `file` through the typed `Ref` API. Both text and
block-structure edits are **block-relative** — the editor reports a block index and the
library resolves it internally (text diffs are scoped to that block so typed text can't
leak across a block boundary; block edits address their marker by index).
-}
applyRichTextIntent : String -> RichTextIntent -> OpDoc Board -> Result OpDoc.Error (OpDoc Board)
applyRichTextIntent file intent doc =
    let
        ref =
            fileRef file
    in
    case intent of
        TextIntent { blockIndex, text } ->
            Ref.setBlockText ref blockIndex text doc

        SetMark { type_, value, from, to } ->
            Ref.mark ref from to type_ value doc

        ClearMark { type_, from, to } ->
            Ref.unmark ref from to type_ doc

        Split { blockIndex, charOffset } ->
            Ref.splitBlock ref blockIndex charOffset doc

        Merge { blockIndex } ->
            Ref.mergeBlock ref blockIndex doc

        SetType { blockIndex, type_ } ->
            Ref.setBlockType ref blockIndex type_ doc

        Indent { blockIndex } ->
            Ref.indentBlock ref blockIndex doc

        Outdent { blockIndex } ->
            Ref.outdentBlock ref blockIndex doc

        Reconcile shapes ->
            reconcileBlocks ref shapes doc


{-| Transform the CRDT's blocks to match the editor's desired `shapes` (used when a
plain text transaction changed the block COUNT — a bulk delete / Delete-key / cut /
paste spanning blocks, none of which go through the keydown handler). We first match
the block count (merging the last block into the previous to shrink, splitting the last
at its end to grow — both no-ops on block 0), then set each block's text, type, and
depth via the tested primitives. Marks on surviving characters follow their chars; a
bulk structural edit carries no mark changes (those ride their own intents).
-}
reconcileBlocks : Ref Board S.RichK (List Span) -> List BlockShape -> OpDoc Board -> Result OpDoc.Error (OpDoc Board)
reconcileBlocks ref shapes doc =
    let
        desiredCount =
            List.length shapes

        blocksOf d =
            Ref.readBlocks ref d |> Result.withDefault []

        lastBlockTextLen d =
            blocksOf d
                |> List.reverse
                |> List.head
                |> Maybe.map blockTextLength
                |> Maybe.withDefault 0

        -- shrink: merge the last block into the previous, `n` times
        shrink n d =
            List.range 1 n
                |> List.foldl
                    (\_ acc ->
                        acc |> Result.andThen (\dd -> Ref.mergeBlock ref (List.length (blocksOf dd) - 1) dd)
                    )
                    (Ok d)

        -- grow: split the last block at its end (append an empty block), `n` times
        grow n d =
            List.range 1 n
                |> List.foldl
                    (\_ acc ->
                        acc |> Result.andThen (\dd -> Ref.splitBlock ref (List.length (blocksOf dd) - 1) (lastBlockTextLen dd) dd)
                    )
                    (Ok d)

        adjustCount d =
            let
                c =
                    List.length (blocksOf d)
            in
            if c > desiredCount then
                shrink (c - desiredCount) d

            else if c < desiredCount then
                grow (desiredCount - c) d

            else
                Ok d

        -- set one block's text, then type, then depth to match `shape`
        applyShape i shape d =
            Ref.setBlockText ref i shape.text d
                |> Result.andThen (reconcileType ref i shape.type_)
                |> Result.andThen (reconcileDepth ref i shape.depth)
    in
    adjustCount doc
        |> Result.andThen
            (\counted ->
                List.indexedMap Tuple.pair shapes
                    |> List.foldl
                        (\( i, shape ) acc -> acc |> Result.andThen (applyShape i shape))
                        (Ok counted)
            )


{-| Set block `i`'s type to match `desired` ("" → clear the mark), only if it differs
from the current type (so we don't emit a redundant op every reconcile).
-}
reconcileType : Ref Board S.RichK (List Span) -> Int -> String -> OpDoc Board -> Result OpDoc.Error (OpDoc Board)
reconcileType ref i desired doc =
    let
        current =
            Ref.readBlocks ref doc
                |> Result.withDefault []
                |> List.drop i
                |> List.head
                |> Maybe.map .type_
                |> Maybe.withDefault ""
    in
    if current == desired then
        Ok doc

    else if desired == "" then
        Ref.setBlockType ref i Nothing doc

    else
        Ref.setBlockType ref i (Just desired) doc


{-| Indent/outdent block `i` until its depth matches `desired`.
-}
reconcileDepth : Ref Board S.RichK (List Span) -> Int -> Int -> OpDoc Board -> Result OpDoc.Error (OpDoc Board)
reconcileDepth ref i desired doc =
    let
        current =
            Ref.readBlocks ref doc
                |> Result.withDefault []
                |> List.drop i
                |> List.head
                |> Maybe.map .depth
                |> Maybe.withDefault 0

        step op_ n d =
            List.range 1 n |> List.foldl (\_ acc -> acc |> Result.andThen (op_ ref i)) (Ok d)
    in
    if desired > current then
        step Ref.indentBlock (desired - current) doc

    else if desired < current then
        step Ref.outdentBlock (current - desired) doc

    else
        Ok doc


{-| The character length of a block's text (sum of its span text lengths).
-}
blockTextLength : Block -> Int
blockTextLength b =
    b.spans |> List.map (.text >> String.length) |> List.sum


{-| Encode blocks for the editor: `{ blocks: [ { type, depth, spans:[…] } ] }`. The
block `type` is the app's opaque string (`""` = default paragraph); `spans` carry the
inline text + marks as before.
-}
encodeBlocks : List Block -> JE.Value
encodeBlocks blocks =
    JE.object [ ( "blocks", JE.list encodeBlock blocks ) ]


encodeBlock : Block -> JE.Value
encodeBlock block =
    JE.object
        [ ( "type", JE.string block.type_ )
        , ( "depth", JE.int block.depth )
        , ( "spans", JE.list encodeSpan block.spans )
        ]


encodeSpan : Span -> JE.Value
encodeSpan span =
    JE.object
        [ ( "text", JE.string span.text )
        , ( "marks"
          , JE.object
                (Dict.toList span.marks
                    |> List.map (\( k, v ) -> ( k, encodeMarkValue v ))
                )
          )
        ]


encodeMarkValue : RichText.MarkValue -> JE.Value
encodeMarkValue v =
    case v of
        RichText.Flag ->
            JE.bool True

        RichText.Value s ->
            JE.string s



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
            div [ class ("app status-bg-" ++ statusSlug board.status) ]
                [ viewHeader model board
                , viewTabs model
                , div [ class "columns" ]
                    [ div [ class "board" ] [ viewTab readOnly model board ]
                    , div [ class "sidebar" ]
                        [ viewPresence model
                        , viewHistory model
                        ]
                    ]
                ]

        Err err ->
            div [ class "error" ] [ text ("schema read error: " ++ S.errorToString err) ]


{-| A short slug for the board status, used to tint the page background subtly.
-}
statusSlug : Status -> String
statusSlug status =
    case status of
        Planning ->
            "planning"

        Active ->
            "active"

        Archived _ ->
            "archived"


statusLabel : Status -> String
statusLabel status =
    case status of
        Planning ->
            "Planning"

        Active ->
            "Active"

        Archived _ ->
            "Archived"


{-| The active tab's content.
-}
viewTab : Bool -> Model -> Board -> Html Msg
viewTab readOnly model board =
    case model.tab of
        TodosTab ->
            viewTodos readOnly model board

        FilesTab ->
            viewFiles readOnly model board

        OutlineTab ->
            div []
                [ h2 [] [ text "Outline" ]
                , viewOutline readOnly model board.outline
                ]

        SettingsTab ->
            viewSettings readOnly model board


{-| The tab bar. Each tab's button also shows dots for any peers currently on that
tab (shared awareness — see the `tab` field of `Peer`).
-}
viewTabs : Model -> Html Msg
viewTabs model =
    div [ class "tabs" ] (List.map (tabButton model) allTabs)


tabButton : Model -> Tab -> Html Msg
tabButton model tab =
    button
        [ class
            (if model.tab == tab then
                "tab active"

             else
                "tab"
            )
        , onClick (SwitchTab tab)
        ]
        [ text (tabLabel tab)
        , span [ class "tab-peers" ] (peerDotsOn model tab)
        ]


{-| Colored dots for the _other_ peers currently viewing `tab`.
-}
peerDotsOn : Model -> Tab -> List (Html Msg)
peerDotsOn model tab =
    Presence.peers model.peers
        |> List.filter (\( rid, _ ) -> rid /= model.me)
        |> List.filter (\( _, p ) -> p.tab == tabId tab)
        |> List.map
            (\( _, p ) ->
                span
                    [ class "tab-peer-dot"
                    , A.style "background" p.color
                    , A.title (p.name ++ " is here")
                    ]
                    []
            )


viewHeader : Model -> Board -> Html Msg
viewHeader model board =
    let
        titleText =
            if String.isEmpty (String.trim board.title) then
                "elm-crdt"

            else
                "elm-crdt — " ++ board.title
    in
    div [ class "header" ]
        [ h1 [] [ text titleText ]
        , span [ class ("status-badge status-" ++ statusSlug board.status) ]
            [ text (statusLabel board.status) ]
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


{-| The Todos tab: the movable todo list + add row.
-}
viewTodos : Bool -> Model -> Board -> Html Msg
viewTodos readOnly model board =
    div []
        [ h2 [] [ text "Todos" ]
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
        ]


{-| The Settings tab: the board title, its status (a sum type), and a likes counter.
-}
viewSettings : Bool -> Model -> Board -> Html Msg
viewSettings readOnly model board =
    div []
        [ h2 [] [ text "Title" ]
        , viewTextInput readOnly model refs.title titleField board.title "Untitled board"
        , h2 [] [ text "Status" ]
        , viewStatus readOnly model board.status
        , h2 [] [ text "Likes" ]
        , div [ class "likes-row" ]
            [ button
                [ class "like-btn", A.disabled readOnly, onClick AddLike ]
                [ text "👍 Like" ]
            , span [ class "likes-count" ] [ text (String.fromInt board.likes) ]
            ]
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


{-| The Files tab: a dict of **rich-text** documents. It is a master/detail view —
with no file open it shows the file list + a create row; open a file (click it) and
it shows just that file's editor with a **back** button. Which file is open (and
whether the list or the editor shows) is local view state (`model.selectedFile`),
not part of the document.
-}
viewFiles : Bool -> Model -> Board -> Html Msg
viewFiles readOnly model board =
    case model.selectedFile of
        Just k ->
            viewFileEditor readOnly (fileBlocks k model.doc) k

        Nothing ->
            viewFileList readOnly model board


viewFileList : Bool -> Model -> Board -> Html Msg
viewFileList readOnly model board =
    div [ class "files" ]
        [ h2 [] [ text "Files" ]
        , if Dict.isEmpty board.files then
            div [ class "editor-empty" ] [ text "No files yet — create one below." ]

          else
            ul [ class "file-names" ]
                (Dict.keys board.files
                    |> List.map (viewFileRow readOnly)
                )
        , if readOnly then
            text ""

          else
            div [ class "add-row" ]
                [ input
                    [ value model.newFileName
                    , placeholder "new file…"
                    , onInput NewFileNameChanged
                    , onEnter CreateFile
                    ]
                    []
                , button [ onClick CreateFile ] [ text "create" ]
                ]
        ]


viewFileRow : Bool -> String -> Html Msg
viewFileRow readOnly k =
    li [ class "file-name" ]
        [ span [ class "file-open", onClick (OpenFile k) ] [ text k ]
        , if readOnly then
            text ""

          else
            button [ class "file-remove", onClick (RemoveFile k) ] [ text "✕" ]
        ]


{-| The editor pane for the open file, with a back button to the list. The
`<crdt-richtext>` element is **keyed by the file name** so switching files remounts
a fresh editor (rather than mutating the open one), and its `docBlocks` property seeds
it with the file's blocks for the first paint.
-}
viewFileEditor : Bool -> List Block -> String -> Html Msg
viewFileEditor readOnly blocks k =
    Html.Keyed.node "div"
        [ class "file-editor" ]
        [ ( "file:" ++ k
          , div []
                [ div [ class "file-editor-head" ]
                    [ button [ class "file-back", onClick CloseFile ] [ text "← Files" ]
                    , span [ class "file-title" ] [ text k ]
                    ]
                , div [ class "editor-hint" ]
                    [ text "Enter for a new block; Tab/Shift-Tab to indent; toolbar for headings/lists/formatting. Open the same file in another tab to edit together." ]
                , Html.node "crdt-richtext"
                    [ A.property "docBlocks" (JE.list encodeBlock blocks)
                    , A.attribute "readonly"
                        (if readOnly then
                            "true"

                         else
                            "false"
                        )
                    ]
                    []
                ]
          )
        ]


{-| The collaborative **outline** (movable tree). Renders the forest recursively;
each node has an editable text field plus controls to indent (nest under the
preceding sibling), outdent (promote to the grandparent), add a child, and remove.
`indent`/`outdent` are computed from the node's position among its siblings, which
is why the recursion threads the parent id and the sibling list.
-}
viewOutline : Bool -> Model -> Tree.Forest Node -> Html Msg
viewOutline readOnly model forest =
    div []
        [ ul [ class "outline" ] (viewForest readOnly model Nothing forest)
        , if readOnly then
            text ""

          else
            div [ class "add-row" ]
                [ button [ onClick AddOutlineRoot ] [ text "+ node" ] ]
        ]


{-| Render a sibling list under `parent`, passing each node its preceding sibling
(for indent) and its parent's parent (for outdent).
-}
viewForest : Bool -> Model -> Maybe OpId -> Tree.Forest Node -> List (Html Msg)
viewForest readOnly model parent forest =
    let
        ids =
            List.map Tree.itemId forest
    in
    List.indexedMap
        (\i item ->
            let
                prevSibling =
                    List.drop (i - 1) ids |> List.head |> maybeIf (i > 0)
            in
            viewOutlineNode readOnly model parent prevSibling item
        )
        forest


{-| `Just x` only when the condition holds (used to gate "preceding sibling").
-}
maybeIf : Bool -> Maybe a -> Maybe a
maybeIf cond m =
    if cond then
        m

    else
        Nothing


viewOutlineNode : Bool -> Model -> Maybe OpId -> Maybe OpId -> Tree.Item Node -> Html Msg
viewOutlineNode readOnly model parent prevSibling item =
    let
        id =
            Tree.itemId item

        node =
            Tree.itemValue item

        controls =
            if readOnly then
                []

            else
                [ button [ onClick (IndentNode id prevSibling), A.disabled (prevSibling == Nothing), A.title "indent (nest under previous)" ] [ text "→" ]
                , button [ onClick (OutdentNode id parent), A.disabled (parent == Nothing), A.title "outdent" ] [ text "←" ]
                , button [ onClick (AddOutlineChild id), A.title "add child" ] [ text "+" ]
                , button [ onClick (RemoveNode id), A.title "remove" ] [ text "✕" ]
                ]
    in
    li [ class "outline-node" ]
        [ div [ class "outline-row" ]
            (viewTextInput readOnly model (outlineTextRef id) (outlineField id) node.text "untitled" :: controls)
        , ul [ class "outline" ] (viewForest readOnly model (Just id) (Tree.itemChildren item))
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
                            , span [ class "peer-tab" ] [ text (" · " ++ tabLabelForId cursor.tab) ]
                            , case cursor.editing of
                                Just f ->
                                    span [ class "editing" ] [ text (" editing " ++ f) ]

                                Nothing ->
                                    text ""
                            ]
                    )
            )
        ]


{-| Human label for a presence `tab` id (defaults gracefully on an unknown id).
-}
tabLabelForId : String -> String
tabLabelForId id =
    allTabs
        |> List.filter (\t -> tabId t == id)
        |> List.head
        |> Maybe.map tabLabel
        |> Maybe.withDefault id


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
        , Ports.richTextInput RichTextInput
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
