module Crdt.History exposing
    ( Checkpoint, Version
    , commit, checkpoints, checkpointVersion, checkpointMessage
    , checkout, restore
    , undo, redo
    )

{-| Local history / version control for a document.

History is **local and non-replicated** — it is not part of the CRDT state and
never travels over the wire. Each replica keeps its own undo/redo stacks and its
own named checkpoints.

The important design point is `restore`: bringing back an old version is modeled
as a **new edit** that re-asserts the snapshot's values with fresh winning
stamps, not as a destructive rewind. So restoring takes effect locally _and_
propagates to peers, while still converging — a peer's genuinely concurrent edit
to an untouched field survives, and the usual CRDT merge reconciles the rest.

@docs Checkpoint, Version
@docs commit, checkpoints, checkpointVersion, checkpointMessage
@docs checkout, restore
@docs undo, redo

-}

import Crdt.Id as Id
import Crdt.Internal as I exposing (Doc)
import Crdt.Node as Node


{-| A named version of the document.
-}
type alias Checkpoint =
    I.Checkpoint


{-| Opaque handle identifying a checkpoint.
-}
type Version
    = Version Int



-- CHECKPOINTS ----------------------------------------------------------------


{-| Save a named checkpoint of the current document state.
-}
commit : String -> Doc -> Doc
commit message doc =
    let
        h =
            I.history doc

        cp =
            { version = h.nextVersion
            , message = message
            , author = Id.ctxReplica (I.ctx doc)
            , snapshot = I.root doc
            }

        h1 =
            { h | checkpoints = cp :: h.checkpoints, nextVersion = h.nextVersion + 1 }
    in
    I.setHistory h1 doc


{-| All checkpoints, most recent first.
-}
checkpoints : Doc -> List Checkpoint
checkpoints doc =
    (I.history doc).checkpoints


{-| The version handle of a checkpoint.
-}
checkpointVersion : Checkpoint -> Version
checkpointVersion cp =
    Version cp.version


{-| The message of a checkpoint.
-}
checkpointMessage : Checkpoint -> String
checkpointMessage cp =
    cp.message



-- CHECKOUT / RESTORE ---------------------------------------------------------


{-| Produce a read-only document showing the state as of a version. Returns
`Nothing` if the version is unknown. The result keeps the live context/history,
so it is safe to render but is intended only for preview.
-}
checkout : Version -> Doc -> Maybe Doc
checkout (Version v) doc =
    (I.history doc).checkpoints
        |> List.filter (\cp -> cp.version == v)
        |> List.head
        |> Maybe.map (\cp -> I.withRootNoHistory cp.snapshot (I.ctx doc) doc)


{-| Restore an old (checked-out) document as a new edit on `current`: the old
snapshot's values are re-asserted with freshly minted, winning stamps (see
`Node.restore`), so the revert actually takes effect and propagates to peers
instead of being swallowed by last-write-wins. Records the restore for undo.
-}
restore : Doc -> Doc -> Doc
restore old current =
    let
        ( restoredRoot, newCtx ) =
            Node.restore (I.ctx current) (I.root old) (I.root current)
    in
    I.withRoot restoredRoot newCtx current



-- UNDO / REDO ----------------------------------------------------------------


{-| Undo the last local edit. No-op if there is nothing to undo.
-}
undo : Doc -> Doc
undo doc =
    let
        h =
            I.history doc
    in
    case h.past of
        prev :: rest ->
            let
                h1 =
                    { h | past = rest, future = I.root doc :: h.future }
            in
            I.setHistory h1 (I.withRootNoHistory prev (I.ctx doc) doc)

        [] ->
            doc


{-| Redo the last undone edit. No-op if there is nothing to redo.
-}
redo : Doc -> Doc
redo doc =
    let
        h =
            I.history doc
    in
    case h.future of
        next :: rest ->
            let
                h1 =
                    { h | past = I.root doc :: h.past, future = rest }
            in
            I.setHistory h1 (I.withRootNoHistory next (I.ctx doc) doc)

        [] ->
            doc
