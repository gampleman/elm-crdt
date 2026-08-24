module Main exposing (main)

{-| Collaborative workspace demo for `gampleman/elm-crdt`, organized into four tabs:
**Todos** (movable list), **Files** (a dict of rich-text docs edited in TipTap),
**Outline** (movable tree), and **Settings** (title + status sum-type + likes
counter). The active tab is local view state but is broadcast on the presence
channel, so peers can see where everyone is.

Runs on the **op-log** core (`Crdt.Doc`): the document is an operation log,
edits emit ops, sync ships ops over a WebSocket, and history is collaborative
time-travel over the op DAG (`Doc.version` / `Doc.readAt`).

It exercises the full JSON-like schema (record + movable list + dict + text + rich
text + tree + sum type + counter + LWW), real WebSocket networking, live
**presence** (who's here, their tab + cursor), **collaborative history** (named
checkpoints + time-travel preview), **branching** (`Doc.fork`): the History panel's
"branch" button forks a private copy you edit without broadcasting, then `merge`s back
(the branch keeps merging peers in the background via `mainline`, so concurrent work
survives) or discards — and **automatic history compaction** (`Doc.stableFrontier`): peers
broadcast their `version`, and once the op log passes `historyCap` (~1000) the demo drops
its ancient history (keeping a recent window) up to the point everyone has seen, so the
scrubber and memory stay bounded over a long session without the user's recent edits ever
vanishing (a straggler that missed a compaction is caught up by an automatic snapshot on
reconnect). See `maybeAutoCompact`.

-}

import Browser
import Crdt as C exposing (Ref)
import Crdt.Cursor as Cursor exposing (Cursor)
import Crdt.Doc as Doc exposing (Checkpoint, Doc, Version)
import Crdt.Edit as Edit
import Crdt.Id exposing (OpId, ReplicaId)
import Crdt.Presence as Presence exposing (Presence)
import Crdt.RichText as RichText exposing (Block, Span)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, h2, input, li, span, text, ul)
import Html.Attributes as A exposing (class, placeholder, value)
import Html.Events exposing (on, onBlur, onClick, onFocus, onInput, preventDefaultOn)
import Html.Keyed
import Html.Lazy
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


{-| A node in the collaborative **outline** — a movable tree (`C.tree`). Each node
carries editable text; nodes can be nested under one another and re-parented, and
concurrent re-parents that would form a cycle converge safely.
-}
type alias Node =
    { text : String }


{-| A board-level lifecycle status — a **sum type**, showcasing `C.custom`. The
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


{-| The board's **flat bundle**: one `Ref` per field plus a reserved `.schema`. Built with
the ref-emitting builders. `boardDoc.schema` is the `Crdt` for `C.init`; `boardDoc.title`
etc. are the compile-checked refs every edit in `update` goes through.
-}
type alias BoardDoc =
    { title : Ref Board C.Settable String
    , status : Ref Board (C.Variants Status) Status
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (C.TreeK C.Nested Node) (Tree.Forest Node)
    , likes : Ref Board C.Counter Int
    , schema : C.Schema C.Nested Board
    }


boardDoc : BoardDoc
boardDoc =
    C.record Board BoardDoc
        |> C.field "title" .title C.text
        |> C.field "status" .status statusDoc
        |> C.field "todos" .todos todosList
        |> C.field "files" .files filesDict
        |> C.field "outline" .outline outlineTree
        |> C.field "likes" .likes C.counter
        |> C.build


schema : C.Schema C.Nested Board
schema =
    boardDoc.schema


{-| Alias so existing `refs.title` / `refs.todos` accesses read naturally — `boardDoc` is
now itself the flat record of refs (plus `.schema`).
-}
refs : BoardDoc
refs =
    boardDoc


type alias StatusDoc =
    { archived : Ref Status C.Settable String
    , schema : C.Schema (C.Variants Status) Status
    }


statusDoc : StatusDoc
statusDoc =
    C.custom
        (\planning active archived value ->
            case value of
                Planning ->
                    planning

                Active ->
                    active

                Archived reason ->
                    archived reason
        )
        StatusDoc
        |> C.variant0 "planning" Planning
        |> C.variant0 "active" Active
        |> C.variant1 "archived" Archived C.text
        |> C.buildCustom


{-| The reason text inside the `Archived` variant. Editing it applies only while the
board is actually `Archived` (silent no-op otherwise).
-}
archivedReasonRef : Ref Board C.Settable String
archivedReasonRef =
    refs.status |> C.at statusDoc.archived


type alias TodoDoc =
    { text : Ref Todo C.Settable String
    , done : Ref Todo C.Settable Bool
    , schema : C.Schema C.Nested Todo
    }


todoDoc : TodoDoc
todoDoc =
    C.record Todo TodoDoc
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


{-| The todos as a **list bundle**: `todosList.schema` goes in the record, and
`todosList.index i` points into an element with the element schema already captured (no
schema needed at the call site). The `files` dict and `outline` tree are the same idea.
-}
todosList : C.Crdt (C.ListK C.Movable C.Nested Todo) (List Todo) { index : Int -> Ref r (C.ListK mv C.Nested Todo) (List Todo) -> Ref r C.Nested Todo }
todosList =
    C.movableList todoDoc


filesDict : C.Crdt (C.DictK C.RichK (List Span)) (Dict String (List Span)) { key : String -> Ref r (C.DictK C.RichK (List Span)) (Dict String (List Span)) -> Ref r C.RichK (List Span) }
filesDict =
    C.dict C.richText


outlineTree : C.Crdt (C.TreeK C.Nested Node) (Tree.Forest Node) { node : OpId -> Ref r (C.TreeK C.Nested Node) (Tree.Forest Node) -> Ref r C.Nested Node }
outlineTree =
    C.tree nodeDoc


{-| A text ref into todo `i`'s `text` field — `todos[i].text`, composed from the
list ref, an element ref, and the field ref. Drives that todo's text input + caret.
-}
todoTextRef : Int -> Ref Board C.Settable String
todoTextRef i =
    refs.todos |> todosList.index i |> C.at todoDoc.text


type alias NodeDoc =
    { text : Ref Node C.Settable String
    , schema : C.Schema C.Nested Node
    }


nodeDoc : NodeDoc
nodeDoc =
    C.record Node NodeDoc
        |> C.field "text" .text C.text
        |> C.build


{-| A text ref into outline node `id`'s `text` field, composed from the tree ref, a
node ref (by stable id), and the field ref. Drives that node's text input + caret.
-}
outlineTextRef : OpId -> Ref Board C.Settable String
outlineTextRef id =
    refs.outline |> outlineTree.node id |> C.at nodeDoc.text


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
        |> Presence.optional "caret" .caret Presence.cursor
        |> Presence.buildCodec



-- REFS (typed accessors into the document) -----------------------------------
-- Composed from the schema-emitted `refs`; every edit goes through these, so a
-- wrong field name or wrong-kind op is a compile error, not a runtime one.


todoDoneRef : Int -> Ref Board C.Settable Bool
todoDoneRef i =
    refs.todos |> todosList.index i |> C.at todoDoc.done


{-| A rich-text ref into file `k`'s contents (a dict of rich text). Drives the
TipTap editor and mark commands for the open file.
-}
fileRef : String -> Ref Board C.RichK (List Span)
fileRef k =
    refs.files |> filesDict.key k



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
    , doc : Doc Board
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

    -- The scrubber slider's step, when previewing a version reached by dragging it. Stored
    -- so the slider thumb is positioned in O(1); inverting version→step by scanning every
    -- step (`versionAt` each) was O(n²) per render and made scrubbing crawl. `Nothing` while
    -- live, or while previewing a checkpoint that isn't a scrub step (thumb sits at the end).
    , scrubStep : Maybe Int
    , connected : Bool

    -- Branching (Doc.fork). `Nothing` = normal: `doc` is the live, syncing document.
    -- `Just main` = we're on a private branch — `doc` is the branch (edited locally but
    -- NOT broadcast), and `main` holds the mainline aside, still merging peers' ops in
    -- the background. "Merge to main" folds the branch back and resumes syncing; "discard"
    -- drops the branch and returns to `main`.
    , mainline : Maybe (Doc Board)

    -- Stable-frontier GC (regime 2): each connected peer's last-broadcast `Version`, keyed
    -- by replica id. We combine these with our own version via `Doc.stableFrontier` to find
    -- the causal cut everyone has seen, which is safe to `compact` below (design-docs/04-gc.md).
    -- `historyCap` bounds the op log: once it's exceeded we auto-compact to that cut, so the
    -- scrubber (and memory) stay bounded no matter how long a session runs.
    , peerVersions : Dict String Version
    , historyCap : Int

    -- delta sync: the version up to which peers already have our ops, so each
    -- broadcast ships only `encodeSince lastSent` instead of the whole log.
    , lastSent : Version

    -- A referentially-STABLE decoded slice of the todos, refreshed only when a
    -- doc change actually touched `refs.todos` (via the merge/ingest `Diff`). Held
    -- separately from the live `read` so an `Html.Lazy` view over it keeps the same
    -- reference — and skips re-rendering — when an unrelated part of the tree changed.
    -- This is the demonstration of the referential-stability + diff work (design-docs/12);
    -- see `viewTodoSummary` (lazy, with a Debug.log render marker).
    , todosSlice : List Todo
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

    -- Auto-compaction bound: once the op log grows past this many ops, we compact to the
    -- stable frontier so the history scrubber never exceeds it. Injected from JS (URL param
    -- `?historyCap=` overrides the default, so tests can force a tiny cap).
    , historyCap : Int
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
            C.init me schema
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
      , scrubStep = Nothing
      , connected = False
      , mainline = Nothing
      , peerVersions = Dict.empty
      , historyCap = Basics.max 1 flags.historyCap
      , lastSent = Doc.version doc
      , todosSlice = Doc.read doc |> Result.map .todos |> Result.withDefault []
      }
    , broadcastPresence presence
    )



-- UPDATE ---------------------------------------------------------------------


type Msg
    = -- any collaborative text field: carries a typed text `Ref` + the new value +
      -- the real DOM caret offset, so the broadcast caret is exact for that field
      TextEdited (Ref Board C.Settable String) String Int
    | CaretMoved (Ref Board C.Settable String) Int
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
    | OutdentNode OpId (Maybe OpId) (List OpId)
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
      -- branching (Doc.fork)
    | ForkBranch
    | MergeBranch
    | DiscardBranch
      -- tabs (local view state)
    | SwitchTab Tab
      -- rich-text editor (ProseMirror via custom element)
    | RichTextInput JD.Value
    | RichTextCaret JD.Value
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
                        Doc.version model.doc

                    -- append a fresh todo at the end of the list
                    doc1 =
                        Edit.append refs.todos (Todo model.newTodo False) model.doc
                            |> orKeep model.doc
                in
                recordPush before { model | doc = doc1, newTodo = "" }

        ToggleTodo i ->
            let
                before =
                    Doc.version model.doc

                current =
                    Doc.read model.doc
                        |> Result.toMaybe
                        |> Maybe.andThen (\b -> List.drop i b.todos |> List.head)
                        |> Maybe.map .done
                        |> Maybe.withDefault False

                doc1 =
                    Edit.set (todoDoneRef i) (not current) model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        RemoveTodo i ->
            let
                before =
                    Doc.version model.doc

                doc1 =
                    Edit.remove refs.todos i model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        SetStatus newStatus ->
            -- switch the board's status variant through the typed Ref API
            let
                before =
                    Doc.version model.doc

                doc1 =
                    Edit.switch refs.status newStatus model.doc |> orKeep model.doc
            in
            recordPush before { model | doc = doc1 }

        ArchivedReasonEdited s _ ->
            -- edit the Archived reason text; applies only while status is Archived.
            -- (Character-wise merge is preserved because it's a text leaf.)
            let
                doc1 =
                    Edit.set archivedReasonRef s model.doc |> orKeep model.doc
            in
            pushDoc { model | doc = doc1 }

        DragStart i ->
            -- capture the version now so the whole drag undoes as one step
            ( { model | dragging = Just i, dragStartVersion = Just (Doc.version model.doc) }, Cmd.none )

        DragOver target ->
            case model.dragging of
                Just from ->
                    if from == target then
                        ( model, Cmd.none )

                    else
                        -- reorder live as the row is dragged over a new slot, so
                        -- the move converges through the same op path as any edit
                        let
                            before =
                                Doc.version model.doc

                            doc1 =
                                Edit.move refs.todos from target model.doc
                                    |> orKeep model.doc
                        in
                        pushDoc (refreshSlices before { model | doc = doc1, dragging = Just target })

                Nothing ->
                    ( model, Cmd.none )

        DragEnd ->
            -- record the entire reorder (all the per-slot moves) as one undo step
            case model.dragStartVersion of
                Just before ->
                    ( { model
                        | dragging = Nothing
                        , dragStartVersion = Nothing
                        , doc = Doc.recordEdit before model.doc
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | dragging = Nothing }, Cmd.none )

        AddLike ->
            let
                before =
                    Doc.version model.doc

                doc1 =
                    Edit.increment refs.likes 1 model.doc |> orKeep model.doc
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
                        Doc.version model.doc

                    -- seed an empty rich-text file at this key, then open it
                    doc1 =
                        Edit.setKey refs.files name [] model.doc
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
                    Doc.version model.doc

                doc1 =
                    Edit.removeKey refs.files k model.doc |> orKeep model.doc

                -- close the file if it was the open one
                selected =
                    if model.selectedFile == Just k then
                        Nothing

                    else
                        model.selectedFile
            in
            recordPush before { model | doc = doc1, selectedFile = selected }

        AddOutlineRoot ->
            outlineEdit (Edit.addChild refs.outline nodeDoc (Node "") Nothing) model

        AddOutlineChild parent ->
            outlineEdit (Edit.addChild refs.outline nodeDoc (Node "") (Just parent)) model

        IndentNode node maybePrev ->
            -- nest under the preceding sibling (Nothing = no previous sibling; no-op)
            case maybePrev of
                Just prev ->
                    outlineEdit (Edit.moveInto refs.outline node (Just prev)) model

                Nothing ->
                    ( model, Cmd.none )

        OutdentNode node parent following ->
            -- Promote one level, the standard outliner way (Workflowy/org-mode): move `node`
            -- to sit immediately after its former parent, AND adopt `node`'s following
            -- siblings as its own children. Adoption is what preserves the visual order — the
            -- siblings that used to sit below `node` under the old parent would otherwise end
            -- up ABOVE it once `node` jumps out. No-op at the top level (no parent).
            case parent of
                Just p ->
                    let
                        outdent doc =
                            Edit.moveAfter refs.outline node p doc
                                |> Result.andThen
                                    (\d ->
                                        -- reparent each following sibling under `node`, in order
                                        -- (each `moveInto` appends, so order is preserved).
                                        List.foldl
                                            (\sib acc -> acc |> Result.andThen (Edit.moveInto refs.outline sib (Just node)))
                                            (Ok d)
                                            following
                                    )
                    in
                    outlineEdit outdent model

                Nothing ->
                    ( model, Cmd.none )

        RemoveNode node ->
            outlineEdit (Edit.removeNode refs.outline node) model

        FocusField fieldName ->
            -- mark the start of a typing session so it undoes as one step
            setEditing (Just fieldName) { model | editStartVersion = Just (Doc.version model.doc) }

        BlurField ->
            -- close the typing session: record everything typed since focus as one
            -- undo step (no-op if nothing changed)
            let
                model1 =
                    case model.editStartVersion of
                        Just before ->
                            { model | doc = Doc.recordEdit before model.doc, editStartVersion = Nothing }

                        Nothing ->
                            model
            in
            setEditing Nothing model1

        Undo ->
            if Doc.canUndo model.doc then
                pushDocRerendering model.doc { model | doc = Doc.undo model.doc, viewing = Nothing, scrubStep = Nothing }

            else
                ( model, Cmd.none )

        Redo ->
            if Doc.canRedo model.doc then
                pushDocRerendering model.doc { model | doc = Doc.redo model.doc, viewing = Nothing, scrubStep = Nothing }

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
            ( { model | doc = Doc.checkpoint label model.doc, checkpointMsg = "" }
            , Cmd.none
            )

        PreviewVersion v ->
            -- previewing a checkpoint: no scrub step (the thumb rests at the live end)
            ( { model | viewing = Just v, scrubStep = Nothing }, Cmd.none )

        LeavePreview ->
            ( { model | viewing = Nothing, scrubStep = Nothing }, Cmd.none )

        Scrub n ->
            -- scrubbing previews the version after the first `n` ops; dragging to
            -- the end (n == historyLength) drops back to the live document. Record `n` as
            -- the slider position so the view doesn't have to invert version→step.
            if n >= Doc.historyLength model.doc then
                ( { model | viewing = Nothing, scrubStep = Nothing }, Cmd.none )

            else
                ( { model | viewing = Just (Doc.versionAt n model.doc), scrubStep = Just n }, Cmd.none )

        RestoreHere ->
            case model.viewing of
                Just v ->
                    -- restore emits fresh winning ops, so it syncs like any edit;
                    -- leave the preview and broadcast the revert
                    pushDocRerendering model.doc { model | doc = Doc.restoreTo v model.doc, viewing = Nothing, scrubStep = Nothing }

                Nothing ->
                    ( model, Cmd.none )

        ForkBranch ->
            -- start a private branch off the current state: `doc` becomes the branch
            -- (edited locally, not broadcast), `mainline` holds the live doc aside so it
            -- keeps merging peers in the background. Re-key to a distinct replica so
            -- branch edits don't collide with the mainline's on merge-back.
            case model.mainline of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    let
                        branchReplica =
                            Crdt.Id.replica (Crdt.Id.replicaToString model.me ++ "-branch")
                    in
                    ( { model
                        | mainline = Just model.doc
                        , doc = Doc.fork branchReplica model.doc
                        , viewing = Nothing
                        , scrubStep = Nothing
                      }
                    , Cmd.none
                    )

        MergeBranch ->
            -- fold the branch back into the mainline (op-union), resume syncing, and
            -- broadcast the branch's ops so peers get them too.
            case model.mainline of
                Just mainDoc ->
                    let
                        merged =
                            Doc.merge mainDoc model.doc
                    in
                    pushDocRerendering model.doc
                        (refreshSlices (Doc.version mainDoc)
                            { model | doc = merged, mainline = Nothing }
                        )

                Nothing ->
                    ( model, Cmd.none )

        DiscardBranch ->
            -- throw the branch away and return to the mainline (which kept syncing).
            case model.mainline of
                Just mainDoc ->
                    ( { model | doc = mainDoc, mainline = Nothing }
                    , renderEditorIfChanged model.selectedFile model.doc mainDoc
                    )

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
            -- while previewing history (or on nothing selected) the editor is read-only;
            -- drop any stray input so a keystroke can't edit the live doc from the past view.
            if model.viewing /= Nothing then
                ( model, Cmd.none )

            else
                case ( model.selectedFile, JD.decodeValue richTextIntentDecoder raw ) of
                    ( Just file, Ok intent ) ->
                        let
                            before =
                                Doc.version model.doc

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

        RichTextCaret raw ->
            -- the editor's caret moved within the open file; mint a stable cursor for
            -- that offset and broadcast it as our presence caret, so peers can draw it.
            case ( model.selectedFile, JD.decodeValue (JD.field "offset" JD.int) raw ) of
                ( Just file, Ok offset ) ->
                    let
                        caret =
                            Cursor.cursorAtRich (fileRef file) offset model.doc

                        presence1 =
                            Presence.updateLocal (\c -> { c | caret = caret }) model.presence
                    in
                    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
                    , broadcastPresence presence1
                    )

                _ ->
                    ( model, Cmd.none )

        GotMessage raw ->
            case decodeEnvelope raw of
                Ok (DocMsg incomingJson) ->
                    case model.mainline of
                        Just mainDoc ->
                            -- while branched, a peer's ops belong to the MAINLINE, not our
                            -- private branch. Merge them into `mainDoc` (held aside) so it
                            -- stays current; the branch is untouched until we merge/discard.
                            -- Nothing on screen changes (we're viewing the branch).
                            case Doc.decodeInto incomingJson mainDoc of
                                Ok main1 ->
                                    ( { model | mainline = Just main1 }, Cmd.none )

                                Err _ ->
                                    ( model, Cmd.none )

                        Nothing ->
                            -- decodeInto merges the peer's ops into our log (idempotent).
                            -- These ops were broadcast to everyone, so advance `lastSent`
                            -- to avoid echoing them back on our next delta. `decodeWithDiff`
                            -- also hands back WHAT the peer changed, so we refresh only the
                            -- touched slices (keeping the rest referentially stable for lazy).
                            case Doc.decodeWithDiff incomingJson model.doc of
                                Ok ( doc1, diff ) ->
                                    let
                                        m1 =
                                            { model | doc = doc1, lastSent = Doc.version doc1 }

                                        m2 =
                                            case Doc.touched refs.todos doc1 diff of
                                                Just _ ->
                                                    { m1 | todosSlice = Doc.read doc1 |> Result.map .todos |> Result.withDefault m1.todosSlice }

                                                Nothing ->
                                                    m1
                                    in
                                    -- a remote edit may have changed the open file; push the
                                    -- new spans to the editor so ProseMirror reconciles. Bound
                                    -- the op log after merging too (a burst of remote ops can
                                    -- push us past the cap); compaction preserves the value, so
                                    -- the editor diff below is unaffected.
                                    ( maybeAutoCompact m2
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

                Ok (VersionMsg rid ver) ->
                    -- a peer told us its current version; remember it so we can compute a
                    -- safe stable-frontier compaction cut. No document change.
                    ( { model | peerVersions = Dict.insert (Crdt.Id.replicaToString rid) ver model.peerVersions }
                    , Cmd.none
                    )

                Ok HelloMsg ->
                    -- a peer just joined: send our full op set AND our presence so
                    -- they catch up on both the document and who's here / cursors. While
                    -- branched, share the MAINLINE (the shared doc), never our private branch.
                    -- Also announce our version for stable-frontier GC.
                    ( model
                    , Cmd.batch [ broadcastFull (syncDoc model), broadcastPresence model.presence, broadcastVersion (syncDoc model) ]
                    )

                Ok (LeftMsg rid) ->
                    -- the relay says this peer's socket closed: drop it from the
                    -- presence view (and its caret), and forget its version — it's no
                    -- longer a peer we must stay safe for (it'll snapshot-catch-up on return).
                    ( { model
                        | peers = Presence.remove rid model.peers
                        , peerVersions = Dict.remove (Crdt.Id.replicaToString rid) model.peerVersions
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ConnectionChanged isUp ->
            -- On (re)connect, exchange full state both ways: push our entire op
            -- set (so peers get anything we did offline) and `hello` (so they
            -- push theirs). After this, steady-state edits are deltas again. While
            -- branched, exchange the MAINLINE, never the private branch.
            ( { model | connected = isUp, lastSent = Doc.version (syncDoc model) }
            , if isUp then
                Cmd.batch
                    [ broadcastFull (syncDoc model)
                    , sayHello
                    , broadcastPresence model.presence
                    ]

              else
                Cmd.none
            )



-- EDIT / SYNC HELPERS --------------------------------------------------------


{-| Edit any collaborative text field + publish the caret in one step: apply the
text change (`Edit.set` on a text ref diffs old→new into minimal insert/delete ops so
concurrent typing merges character-wise), then publish a **stable caret** at the
real DOM offset within that ref. Because the caret is a `Crdt.Cursor`, it tracks the
right character on every viewer even as they make their own concurrent edits.
-}
editText : Ref Board C.Settable String -> String -> Int -> Model -> ( Model, Cmd Msg )
editText textRef newValue caretOffset model =
    let
        doc1 =
            Edit.set textRef newValue model.doc
                |> orKeep model.doc

        caret =
            Cursor.cursorAt textRef caretOffset doc1

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
publishCaret : Ref Board C.Settable String -> Int -> Model -> ( Model, Cmd Msg )
publishCaret textRef caretOffset model =
    let
        caret =
            Cursor.cursorAt textRef caretOffset model.doc

        presence1 =
            Presence.updateLocal (\c -> { c | caret = caret }) model.presence
    in
    ( { model | presence = presence1, peers = Presence.merge model.peers presence1 }
    , broadcastPresence presence1
    )


orKeep : Doc Board -> Result Edit.EditError (Doc Board) -> Doc Board
orKeep fallback result =
    Result.withDefault fallback result


{-| Record the just-made edit on the local undo stack (bracketing it with the
`before` version), then broadcast it. `model.doc` already holds the post-edit
document; `before` is the version captured before the edit. Used by every discrete
todo/note action so each is one Ctrl-Z step.
-}
recordPush : Version -> Model -> ( Model, Cmd Msg )
recordPush before model =
    pushDoc (refreshSlices before { model | doc = Doc.recordEdit before model.doc })


{-| Refresh the referentially-stable decoded slices held in the model, but ONLY the
ones a change since `before` actually touched (asking the `Diff` with the same typed
refs used to write). An untouched slice keeps its previous reference, so an
`Html.Lazy` view over it skips re-rendering. This is the demo-side payoff of the
merge/ingest diff (design-docs/12): local edits and remote merges alike flow through here.
-}
refreshSlices : Version -> Model -> Model
refreshSlices before model =
    let
        diff =
            Doc.diffSince before model.doc
    in
    case Doc.touched refs.todos model.doc diff of
        Just _ ->
            { model | todosSlice = Doc.read model.doc |> Result.map .todos |> Result.withDefault model.todosSlice }

        Nothing ->
            model


{-| Apply one outline (tree) edit, record it as a single undo step, and broadcast.
-}
outlineEdit : (Doc Board -> Result Edit.EditError (Doc Board)) -> Model -> ( Model, Cmd Msg )
outlineEdit edit model =
    let
        before =
            Doc.version model.doc
    in
    recordPush before { model | doc = edit model.doc |> orKeep model.doc }


{-| The document that represents our **shared** (syncable) state: the mainline when we're
on a private branch, otherwise the live `doc`. Used everywhere we send state to peers, so
an in-progress branch is never leaked over the wire.
-}
syncDoc : Model -> Doc Board
syncDoc model =
    Maybe.withDefault model.doc model.mainline


{-| Keep the op log bounded: once it grows past `historyCap`, regime-2 GC folds history up
to the **stable frontier** (the cut every connected peer has seen) into the base and drops
those ops, so the history scrubber and memory never grow without bound over a long session.

Two ideas combine into the cut. First we only ever want to drop **ancient** history — never
the recent tail, or the scrubber would appear to lose the user's own last edits. So the
target is `versionAt (opCount - keepRecent)`: the point `keepRecent` edits back from the tip.
Second, dropping ops is only safe below a cut every peer has already seen. `stableFrontier`
is exactly "frontier of the intersection of these versions' ancestors", so feeding it the
ancient target **alongside** the peer versions yields a cut that is at once no newer than the
recent window _and_ no past any laggard — safety and the keep-recent policy in one call. (The
stable frontier alone would be the wrong target: when everyone is synced it equals the tip,
so compacting to it would wipe the whole scrubber.)

`keepRecent` is half `historyCap`, so a compaction leaves ~half the cap and it takes that
many more edits to trip again — hysteresis, so the O(n) compaction amortizes instead of
firing every edit at steady state. A peer that reconnects below the cut is caught up by a
snapshot. Skipped while time-travelling (`viewing`) or on a private branch, both of which
hold the full op log. If a peer lags far enough that the safe cut sits inside the recent
window, less (or nothing) is dropped — best-effort, never at the cost of safety.

-}
maybeAutoCompact : Model -> Model
maybeAutoCompact model =
    case ( model.viewing, model.mainline ) of
        ( Nothing, Nothing ) ->
            if Doc.opCount model.doc > model.historyCap then
                let
                    keepRecent =
                        model.historyCap // 2

                    -- the point `keepRecent` edits back from the tip: our target, i.e. the
                    -- OLDEST version we're willing to drop below (keeps the recent tail).
                    ancientTarget =
                        Doc.versionAt (Doc.historyLength model.doc - keepRecent) model.doc

                    -- clamp the target to what every peer has seen: the intersection of the
                    -- ancient target with the peers is "≤ target AND ≤ everyone" — safe to drop.
                    cut =
                        Doc.stableFrontier (ancientTarget :: Dict.values model.peerVersions) model.doc
                in
                { model | doc = Doc.compact cut model.doc }

            else
                model

        _ ->
            model


{-| After any local change, broadcast only the **delta** since our last
broadcast, then advance `lastSent`. While connected every peer sees every
broadcast, so this keeps everyone in sync at edit-size bandwidth; fresh peers are
caught up by the full-state exchange on connect (see `ConnectionChanged` /
`hello`).
-}
pushDoc : Model -> ( Model, Cmd Msg )
pushDoc model =
    case model.mainline of
        Just _ ->
            -- on a private branch: keep edits local, don't broadcast. `lastSent` is left
            -- untouched so the accumulated branch delta ships in one go on merge-back.
            ( model, Cmd.none )

        Nothing ->
            let
                now =
                    Doc.version model.doc

                -- build the outgoing delta from the CURRENT doc first, then bound the local
                -- op log. Compaction preserves the tip frontier, so `now`/`lastSent` stay
                -- valid, and peers still received the ordinary delta above.
                delta =
                    Ports.outgoing (envelope "doc" (Doc.encodeSince model.lastSent model.doc))
            in
            ( maybeAutoCompact { model | lastSent = now }
            , Cmd.batch
                [ delta

                -- announce our new version too, so peers can recompute the stable
                -- frontier for safe compaction (tiny — just a frontier).
                , broadcastVersion model.doc
                ]
            )


{-| Like `pushDoc`, but also re-render the editor if the **open file's** contents
changed (used by undo/redo/restore, which can rewrite the doc under the editor's
feet). `old` is the pre-change doc for the diff.
-}
pushDocRerendering : Doc Board -> Model -> ( Model, Cmd Msg )
pushDocRerendering old model =
    let
        ( m1, syncCmd ) =
            pushDoc (refreshSlices (Doc.version old) model)
    in
    ( m1, Cmd.batch [ syncCmd, renderEditorIfChanged model.selectedFile old model.doc ] )


{-| Chain an editor re-render for file `k` onto an existing `(model, cmd)` (used
after `CreateFile`, which opens the new file).
-}
andRenderEditor : Doc Board -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
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
renderEditorFor : String -> Doc Board -> Cmd Msg
renderEditorFor k doc =
    Ports.renderRichText (encodeBlocks (fileBlocks k doc))


{-| Re-render the editor only if the **open file's** contents changed between `old`
and `new` — avoids churning ProseMirror (and its caret) on unrelated edits.
-}
renderEditorIfChanged : Maybe String -> Doc Board -> Doc Board -> Cmd Msg
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
fileBlocks : String -> Doc Board -> List Block
fileBlocks k doc =
    Edit.readBlocks (fileRef k) doc |> Result.withDefault []


{-| Broadcast our entire op set — used for catch-up (connect / answering a
`hello`), the one place full state is needed.
-}
broadcastFull : Doc Board -> Cmd Msg
broadcastFull doc =
    Ports.outgoing (envelope "doc" (Doc.encode doc))


{-| Announce ourselves so already-present peers send us their full state.
-}
sayHello : Cmd Msg
sayHello =
    Ports.outgoing (envelope "hello" (JE.object []))


broadcastPresence : Presence Peer -> Cmd Msg
broadcastPresence p =
    Ports.outgoing (envelope "presence" (Presence.encode p))


{-| Broadcast our current version (a tiny frontier) so peers can compute the stable
frontier for safe multi-replica compaction. Sent after every synced edit and on connect.
-}
broadcastVersion : Doc Board -> Cmd Msg
broadcastVersion doc =
    Ports.outgoing (envelope "version" (Doc.encodeVersion (Doc.version doc)))


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
    | VersionMsg ReplicaId Version -- a peer announcing its current version (for stable-frontier GC)
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

                        "version" ->
                            JD.map2 VersionMsg
                                (JD.field "from" JD.string |> JD.map Crdt.Id.replica)
                                (JD.field "payload" JD.value
                                    |> JD.andThen
                                        (\v ->
                                            case Doc.decodeVersion v of
                                                Ok ver ->
                                                    JD.succeed ver

                                                Err e ->
                                                    JD.fail e
                                        )
                                )

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
`Doc.readBlocks` before calling the block edits, which are marker-addressed.

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
applyRichTextIntent : String -> RichTextIntent -> Doc Board -> Result Edit.EditError (Doc Board)
applyRichTextIntent file intent doc =
    let
        ref =
            fileRef file
    in
    case intent of
        TextIntent { blockIndex, text } ->
            Edit.setBlockText ref blockIndex text doc

        SetMark { type_, value, from, to } ->
            Edit.mark ref from to type_ value doc

        ClearMark { type_, from, to } ->
            Edit.unmark ref from to type_ doc

        Split { blockIndex, charOffset } ->
            Edit.splitBlock ref blockIndex charOffset doc

        Merge { blockIndex } ->
            Edit.mergeBlock ref blockIndex doc

        SetType { blockIndex, type_ } ->
            Edit.setBlockType ref blockIndex type_ doc

        Indent { blockIndex } ->
            Edit.indentBlock ref blockIndex doc

        Outdent { blockIndex } ->
            Edit.outdentBlock ref blockIndex doc

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
reconcileBlocks : Ref Board C.RichK (List Span) -> List BlockShape -> Doc Board -> Result Edit.EditError (Doc Board)
reconcileBlocks ref shapes doc =
    let
        desiredCount =
            List.length shapes

        blocksOf d =
            Edit.readBlocks ref d |> Result.withDefault []

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
                        acc |> Result.andThen (\dd -> Edit.mergeBlock ref (List.length (blocksOf dd) - 1) dd)
                    )
                    (Ok d)

        -- grow: split the last block at its end (append an empty block), `n` times
        grow n d =
            List.range 1 n
                |> List.foldl
                    (\_ acc ->
                        acc |> Result.andThen (\dd -> Edit.splitBlock ref (List.length (blocksOf dd) - 1) (lastBlockTextLen dd) dd)
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
            Edit.setBlockText ref i shape.text d
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
reconcileType : Ref Board C.RichK (List Span) -> Int -> String -> Doc Board -> Result Edit.EditError (Doc Board)
reconcileType ref i desired doc =
    let
        current =
            Edit.readBlocks ref doc
                |> Result.withDefault []
                |> List.drop i
                |> List.head
                |> Maybe.map .type_
                |> Maybe.withDefault ""
    in
    if current == desired then
        Ok doc

    else if desired == "" then
        Edit.setBlockType ref i Nothing doc

    else
        Edit.setBlockType ref i (Just desired) doc


{-| Indent/outdent block `i` until its depth matches `desired`.
-}
reconcileDepth : Ref Board C.RichK (List Span) -> Int -> Int -> Doc Board -> Result Edit.EditError (Doc Board)
reconcileDepth ref i desired doc =
    let
        current =
            Edit.readBlocks ref doc
                |> Result.withDefault []
                |> List.drop i
                |> List.head
                |> Maybe.map .depth
                |> Maybe.withDefault 0

        step op_ n d =
            List.range 1 n |> List.foldl (\_ acc -> acc |> Result.andThen (op_ ref i)) (Ok d)
    in
    if desired > current then
        step Edit.indentBlock (desired - current) doc

    else if desired < current then
        step Edit.outdentBlock (current - desired) doc

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
textFieldAttrs : Ref Board C.Settable String -> List (Html.Attribute Msg)
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
viewFieldCarets : Ref Board C.Settable String -> Model -> List (Html Msg)
viewFieldCarets textRef model =
    let
        -- while scrubbing, show WHO made this step's edit (from the op log) at the spot it
        -- landed, instead of live presence carets — mirroring the rich-text editor.
        carets =
            case model.scrubStep of
                Just n ->
                    scrubCaret textRef
                        (\v -> Edit.readAt v textRef model.doc |> Result.withDefault "")
                        n
                        model
                        |> maybeToList
                        |> List.map (\c -> ( c.name, c.color, c.offset ))

                Nothing ->
                    liveFieldCarets textRef model
    in
    carets
        |> List.map
            (\( name, color, offset ) ->
                -- positioned over the input: 0.5rem left padding + 1px border,
                -- then `offset` character-widths in. Vertical: just inside the
                -- input's top/bottom padding. `ch`-approximation (see design-docs/03).
                span
                    [ class "remote-caret"
                    , A.style "position" "absolute"
                    , A.style "left" ("calc(0.5rem + 1px + " ++ String.fromInt offset ++ "ch)")
                    , A.style "top" "0.3rem"
                    , A.style "background" color
                    , A.style "width" "2px"
                    , A.style "height" "1.2rem"
                    , A.title (name ++ "'s cursor")
                    ]
                    []
            )


{-| The live presence carets pointing into `textRef` — every OTHER peer's current caret,
resolved to its offset (filtered to this field's container). As `(name, color, offset)`.
-}
liveFieldCarets : Ref Board C.Settable String -> Model -> List ( String, String, Int )
liveFieldCarets textRef model =
    let
        fieldCursor =
            Cursor.cursorAt textRef 0 model.doc
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
                                -- only this field's carets: the cursor must point
                                -- into the same container as the field we're rendering
                                if
                                    (fieldCursor |> Maybe.map (Cursor.sameContainer c))
                                        == Just True
                                then
                                    Cursor.cursorOffset c model.doc
                                        |> Maybe.map (\offset -> ( peer.name, peer.color, offset ))

                                else
                                    Nothing
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


{-| Read the board to render: the live document, or a past version when previewing.
-}
readShown : Model -> Result Doc.ReadError Board
readShown model =
    case model.viewing of
        Just v ->
            Doc.readAt v model.doc

        Nothing ->
            Doc.read model.doc


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
            div [ class "error" ] [ text ("schema read error: " ++ Doc.readErrorToString err) ]


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
viewTextInput : Bool -> Model -> Ref Board C.Settable String -> String -> String -> String -> Html Msg
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

        -- Referential-stability demo: on the LIVE path the summary is rendered by
        -- `Html.Lazy` from the STABLE `model.todosSlice` (refreshed only when a change
        -- actually touched the todos — see `refreshSlices`). Because an unrelated edit
        -- (title, a file, the counter, a peer's settings change) leaves `todosSlice`
        -- referentially unchanged, `lazy` skips this view entirely — visible as the
        -- ABSENCE of a "render:todo-summary" line in the console. Editing the todos DOES
        -- re-run it. During a time-travel PREVIEW (`readOnly`) the slice still tracks the
        -- live doc, so we summarise the *shown* `board.todos` instead — otherwise the
        -- count wouldn't match the previewed list below it.
        , if readOnly then
            viewTodoSummary board.todos

          else
            Html.Lazy.lazy viewTodoSummary model.todosSlice
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


{-| A pure summary of the todos, wrapped in `Html.Lazy` by the caller over the stable
`model.todosSlice`. The `Debug.log` fires ONLY when this actually re-renders — i.e. only
when the todos slice changed reference. Watch the console: editing the title / a file /
the counter, or a peer editing settings, does NOT log here (lazy skips it); editing a
todo does. That absence is the referential-stability win made visible.
-}
viewTodoSummary : List Todo -> Html Msg
viewTodoSummary todos =
    let
        _ =
            Debug.log "render:todo-summary" (List.length todos)

        doneCount =
            List.filter .done todos |> List.length
    in
    div [ class "todo-summary" ]
        [ text (String.fromInt doneCount ++ " / " ++ String.fromInt (List.length todos) ++ " done") ]


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
Demonstrates `C.custom` + the typed `Crdt` write API end to end.
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
            -- feed the editor the blocks as of the SHOWN version — while scrubbing history
            -- (`viewing`) that's the past state, so the rich-text editor previews it too
            -- (its `docBlocks` property reconciles on every render); otherwise the live doc.
            let
                blocks =
                    case model.viewing of
                        Just v ->
                            Edit.readBlocksAt v (fileRef k) model.doc |> Result.withDefault []

                        Nothing ->
                            fileBlocks k model.doc
            in
            viewFileEditor readOnly blocks (richTextRemoteCarets k model) k

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
viewFileEditor : Bool -> List Block -> List RemoteCaret -> String -> Html Msg
viewFileEditor readOnly blocks carets k =
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
                    , A.property "remoteCarets" (JE.list encodeRemoteCaret carets)
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


{-| A remote peer's caret resolved against the open file: where it sits (a document-wide
character offset) plus how to label it. Passed to the `<crdt-richtext>` element, which
draws each as a colored bar + name flag via a ProseMirror decoration.
-}
type alias RemoteCaret =
    { offset : Int, color : String, name : String }


encodeRemoteCaret : RemoteCaret -> JE.Value
encodeRemoteCaret c =
    JE.object
        [ ( "offset", JE.int c.offset )
        , ( "color", JE.string c.color )
        , ( "name", JE.string c.name )
        ]


{-| Every OTHER peer's caret that currently points into file `k`, resolved to its live
character offset. A peer's caret only shows here if its cursor is in the same container
as this file's rich text (so a caret left in a different file or a plain text field is
filtered out), mirroring `viewFieldCarets` for the plain inputs.
-}
richTextRemoteCarets : String -> Model -> List RemoteCaret
richTextRemoteCarets k model =
    case model.scrubStep of
        Just n ->
            -- while scrubbing history, don't show live presence carets (they reflect NOW,
            -- not the previewed past). Instead attribute the edit that produced this step
            -- to its author, drawn as a caret at the spot it changed. See `scrubCaret`.
            richTextScrubCaret k n model |> maybeToList

        Nothing ->
            liveRemoteCarets k model


{-| The live presence carets pointing into file `k` — every OTHER peer's current caret,
resolved to its offset (filtered to this file's container). Shown only on the live doc.
-}
liveRemoteCarets : String -> Model -> List RemoteCaret
liveRemoteCarets k model =
    let
        fileCursor =
            Cursor.cursorAtRich (fileRef k) 0 model.doc
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
                                if (fileCursor |> Maybe.map (Cursor.sameContainer c)) == Just True then
                                    Cursor.cursorOffset c model.doc
                                        |> Maybe.map (\offset -> { offset = offset, color = peer.color, name = peer.name })

                                else
                                    Nothing
                            )
            )


{-| Attribution caret for the rich-text file `k` at scrub step `n`. Thin wrapper over the
generic `scrubCaret`, reading the file's flat text via `Edit.readBlocksAt`.
-}
richTextScrubCaret : String -> Int -> Model -> Maybe RemoteCaret
richTextScrubCaret k n model =
    scrubCaret (fileRef k)
        (\v -> Edit.readBlocksAt v (fileRef k) model.doc |> Result.withDefault [] |> blocksFlatText)
        n
        model


{-| Attribution caret for scrub step `n` at an arbitrary text `ref`: if the edit that
carried the document from step `n-1` to step `n` touched `ref`, a caret in the author's
color/name at the character the edit landed on. `Nothing` if that step didn't touch `ref`.

The author comes from the op log (`Doc.diffBetween` → `Doc.touched` → the op's replica id),
NOT from presence — presence is ephemeral and can't say who made a _past_ edit. The
character offset is recovered by comparing the ref's text (via `readText`) just before and
just after the step, anchoring the caret just past the changed run (see `editOffset`).

-}
scrubCaret : Ref Board kind a -> (Version -> String) -> Int -> Model -> Maybe RemoteCaret
scrubCaret ref readText n model =
    let
        before =
            Doc.versionAt (n - 1) model.doc

        after =
            Doc.versionAt n model.doc

        stepDiff =
            Doc.diffBetween before after model.doc
    in
    Doc.touched ref model.doc stepDiff
        |> Maybe.map
            (\origin ->
                let
                    author =
                        attribution origin model

                    offset =
                        editOffset (readText before) (readText after)
                in
                { offset = offset, color = author.color, name = author.name }
            )


{-| Resolve an edit's `Origin` to a display name + color. A remote author is looked up in
the current presence view (they may still be here); if they've since left — or the author
is us — we fall back to what we know (our own identity, or the bare replica id). This is
why attribution survives a peer leaving: the author id is in the op, and we label it as
best we can.
-}
attribution : Doc.Origin -> Model -> { name : String, color : String }
attribution origin model =
    let
        selfLabel =
            Presence.local model.presence
                |> Maybe.map (\p -> { name = p.name, color = p.color })
                |> Maybe.withDefault { name = "you", color = "#888" }
    in
    case Doc.originReplica origin of
        Nothing ->
            -- Local: authored by this replica
            selfLabel

        Just rid ->
            Presence.peers model.peers
                |> List.filter (\( r, _ ) -> r == rid)
                |> List.head
                |> Maybe.map (\( _, p ) -> { name = p.name, color = p.color })
                |> Maybe.withDefault { name = Crdt.Id.replicaToString rid, color = "#888" }


{-| The character offset an edit landed on — where the caret sits in the ref's text
_after_ the step, given the text `beforeText`/`afterText` just before and after it. The
step changed the run between the two versions' common prefix and common suffix; the caret
belongs at the END of that run in the newer text, which is where an editor's caret lands
after the edit: just past an insertion, or at the join point of a deletion. (Anchoring at
the common-prefix start instead put the caret one character too far left for insertions —
a bug we fixed.)
-}
editOffset : String -> String -> Int
editOffset beforeText afterText =
    let
        prefix =
            commonPrefixLength beforeText afterText

        -- clamp the shared suffix so it can't overlap the shared prefix (e.g. "aa" → "aaa")
        suffix =
            Basics.min
                (commonSuffixLength beforeText afterText)
                (Basics.min (String.length beforeText) (String.length afterText) - prefix)
    in
    String.length afterText - suffix


{-| The flat character stream of a block list (span text concatenated, block boundaries not
counted) — matching the offset space the rich-text editor's carets use.
-}
blocksFlatText : List Block -> String
blocksFlatText blocks =
    blocks
        |> List.concatMap .spans
        |> List.map .text
        |> String.concat


commonPrefixLength : String -> String -> Int
commonPrefixLength a b =
    let
        go xs ys acc =
            case ( xs, ys ) of
                ( x :: xrest, y :: yrest ) ->
                    if x == y then
                        go xrest yrest (acc + 1)

                    else
                        acc

                _ ->
                    acc
    in
    go (String.toList a) (String.toList b) 0


commonSuffixLength : String -> String -> Int
commonSuffixLength a b =
    commonPrefixLength (String.reverse a) (String.reverse b)


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just x ->
            [ x ]

        Nothing ->
            []


{-| The collaborative **outline** (movable tree). Renders the forest recursively;
each node has an editable text field plus controls to indent (nest under the
preceding sibling), outdent (promote one level, landing right after its former parent),
add a child, and remove. `indent`/`outdent` are computed from the node's position among
its siblings, which is why the recursion threads the parent id and the sibling list.
-}
viewOutline : Bool -> Model -> Tree.Forest Node -> Html Msg
viewOutline readOnly model forest =
    div []
        [ ul [ class "outline outline-root" ] (viewForest readOnly model Nothing forest)
        , if readOnly then
            text ""

          else
            div [ class "add-row" ]
                [ button [ onClick AddOutlineRoot ] [ text "+ node" ] ]
        ]


{-| Render a sibling list whose common parent is `parent`. Each node gets its preceding
sibling (for **indent** — nest under it), `parent` (for **outdent**), and its **following
siblings** (which outdent re-parents under the node, to preserve visual order). Top-level
nodes have `parent = Nothing`, so their indent/outdent buttons are disabled.
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

                following =
                    List.drop (i + 1) ids
            in
            viewOutlineNode readOnly model parent prevSibling following item
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


viewOutlineNode : Bool -> Model -> Maybe OpId -> Maybe OpId -> List OpId -> Tree.Item Node -> Html Msg
viewOutlineNode readOnly model parent prevSibling following item =
    let
        id =
            Tree.itemId item

        node =
            Tree.itemValue item

        controls =
            if readOnly then
                []

            else
                -- indent → nest under the preceding sibling; outdent → promote out one level
                -- (right after `parent`), adopting the following siblings so the visible order
                -- is preserved. Both are disabled at the top level (no preceding sibling / no
                -- parent).
                [ button [ onClick (IndentNode id prevSibling), A.disabled (prevSibling == Nothing), A.title "indent (nest under previous)" ] [ text "→" ]
                , button [ onClick (OutdentNode id parent following), A.disabled (parent == Nothing), A.title "outdent (promote out one level)" ] [ text "←" ]
                , button [ onClick (AddOutlineChild id), A.title "add child" ] [ text "+" ]
                , button [ onClick (RemoveNode id), A.title "remove" ] [ text "✕" ]
                ]
    in
    li [ class "outline-node" ]
        [ div [ class "outline-row" ]
            (viewTextInput readOnly model (outlineTextRef id) (outlineField id) node.text "untitled" :: controls)

        -- children: their parent is this node.
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
                [ onClick Undo, A.disabled (not (Doc.canUndo model.doc)), A.title "undo your last edit" ]
                [ text "↶ undo" ]
            , button
                [ onClick Redo, A.disabled (not (Doc.canRedo model.doc)), A.title "redo" ]
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
            (List.map (viewCheckpoint model.viewing) (Doc.checkpoints model.doc))
        , case model.viewing of
            Just _ ->
                div [ class "preview-banner" ]
                    [ text "time-travelling to an old version — "
                    , button [ onClick LeavePreview ] [ text "back to latest" ]
                    ]

            Nothing ->
                text ""
        , viewBranch model
        , viewCompaction model
        ]


{-| A passive op-log gauge. Compaction is **automatic** (`maybeAutoCompact` bounds the log
at `historyCap` by folding history everyone has seen into the base), so there is no button —
this just surfaces the current op count and the cap so the bounding is visible. Hidden while
previewing history or on a branch (both hold the full op log).
-}
viewCompaction : Model -> Html Msg
viewCompaction model =
    case ( model.viewing, model.mainline ) of
        ( Nothing, Nothing ) ->
            div [ class "compact-row" ]
                [ span
                    [ class "op-count"
                    , A.title "history is compacted automatically to the point every connected peer has seen, so the op log stays bounded"
                    ]
                    [ text (String.fromInt (Doc.opCount model.doc) ++ " / " ++ String.fromInt model.historyCap ++ " ops (auto-compacted)") ]
                ]

        _ ->
            text ""


{-| The branching panel. On the mainline it offers a "branch" button (fork the current
state into a private, non-syncing scratch copy). On a branch it shows a banner, how far
the branch has diverged from the mainline (`Doc.divergence`), and merge-back / discard
actions. Edits made while the banner is showing stay local until you merge.
-}
viewBranch : Model -> Html Msg
viewBranch model =
    case model.mainline of
        Nothing ->
            div [ class "branch-row" ]
                [ button
                    [ onClick ForkBranch
                    , A.title "fork a private branch you can edit without affecting others, then merge or discard"
                    ]
                    [ text "⑃ branch" ]
                ]

        Just mainDoc ->
            let
                div_ =
                    Doc.divergence { branch = model.doc, mainline = mainDoc }
            in
            div [ class "branch-banner" ]
                [ div [ class "branch-title" ] [ text "on a private branch" ]
                , div [ class "branch-detail" ]
                    [ text
                        ("your edits are local — "
                            ++ String.fromInt div_.ahead
                            ++ " ahead"
                            ++ (if div_.behind > 0 then
                                    ", " ++ String.fromInt div_.behind ++ " behind main"

                                else
                                    ""
                               )
                        )
                    ]
                , div [ class "branch-actions" ]
                    [ button [ onClick MergeBranch ] [ text "merge to main" ]
                    , button [ class "discard-btn", onClick DiscardBranch ] [ text "discard" ]
                    ]
                ]


{-| A slider over the document's linear op history. Dragging it previews the state
at that step (`Doc.versionAt`); the slider sits at the far right (live) when not
previewing. While parked in the past, a "restore to here" button rewinds the live
document to that point — as new ops, so the revert syncs to every peer.
-}
viewScrubber : Model -> Html Msg
viewScrubber model =
    let
        len =
            Doc.historyLength model.doc

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


{-| The slider's current step: the recorded `scrubStep` when we're mid-scrub, otherwise
the live end (also used for a checkpoint preview, which has no scrub step). Reads the stored
step directly — O(1) — instead of scanning every step to invert version→step.
-}
scrubPosition : Model -> Int
scrubPosition model =
    case model.scrubStep of
        Just n ->
            n

        Nothing ->
            Doc.historyLength model.doc


viewCheckpoint : Maybe Version -> Checkpoint -> Html Msg
viewCheckpoint viewing cp =
    let
        cpVersion =
            Doc.checkpointVersion cp

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
        [ span [ class "cp-msg" ] [ text (Doc.checkpointMessage cp) ]
        , span [ class "cp-author" ] [ text (Crdt.Id.replicaToString (Doc.checkpointAuthor cp)) ]
        , button [ onClick (PreviewVersion cpVersion) ] [ text "preview" ]
        ]



-- SUBSCRIPTIONS / MAIN -------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.incoming GotMessage
        , Ports.connection ConnectionChanged
        , Ports.richTextInput RichTextInput
        , Ports.richTextCaret RichTextCaret
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
