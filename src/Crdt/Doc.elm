module Crdt.Doc exposing
    ( Doc
    , merge, encode, decodeInto, encodeSince
    , Diff, Origin, isLocal, originReplica, mergeWithDiff, decodeWithDiff, diffSince
    , Version, version, historyLength, versionAt, restoreTo
    , fork, forkAt, Divergence, divergence
    , recordEdit, undo, redo, canUndo, canRedo
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
    , compact, opCount
    )

{-| A **document** — the live, collaborative value your application holds and edits.

A `Doc` is a JSON-like document (records, lists, dictionaries, text, counters, and
your own types — whatever your `Crdt` schema describes) that several people can edit at
the same time, on different devices, even while offline, and always end up agreeing on
the result. You never reconcile conflicts by hand: concurrent edits **merge**
automatically and every replica converges to the same value.

You **create** a document (`Crdt.init`) and **read** it (`Crdt.read`) through the `Crdt`
module, where the schema lives, and **edit** it through that module's typed refs. This
module is everything else you do with a document once you have one:

1.  **sync** it with other replicas by shipping bytes over any transport you like
    (`encode` / `decodeInto`, or the smaller `encodeSince` delta), and merging what
    arrives — the library guarantees convergence, the network is entirely yours;

2.  **react** to what a merge changed (`mergeWithDiff` + `Crdt.touched`);

3.  travel through **history** (`version` / `restoreTo`), **undo/redo**, name
    **checkpoints**, and **compact** old history away.

    -- sync: send `Doc.encode doc` to a peer; on receipt:
    doc2 =
    Doc.decodeInto incomingBytes doc
    |> Result.withDefault doc


# The document

You create a document (`Crdt.init`) and read it (`Crdt.read`) through the `Crdt` module,
where the schema lives. This module is everything else you do with one once you have it.

@docs Doc


# Syncing with other replicas

Convergence is the library's job; moving bytes is yours. `encode` a document (or
`encodeSince` a small delta) to a peer over any channel — WebSocket, HTTP, a file — and
`decodeInto` merges what arrives. Edits from any replica, in any order, converge.

@docs merge, encode, decodeInto, encodeSince


# Reacting to what changed

Merging is automatic, but sometimes you want to _react_ to an incoming change rather than
just re-render the new state (your `view` already reflects that for free). Knowing **what**
a merge touched and **who** did it lets you, for example, flash a highlight on a field a
collaborator just edited, show a "Bob is editing" marker, scroll a remote change into
view, or fire a notification. `mergeWithDiff` / `decodeWithDiff` hand back a `Diff`
alongside the new document; ask it questions with your typed refs — `Crdt.touched ref
diff` tells you whether (and by whom) a given spot changed.

@docs Diff, Origin, isLocal, originReplica, mergeWithDiff, decodeWithDiff, diffSince


# History and time travel

A document remembers how it got to its current state, so you can look at any past point,
scrub through the timeline, and restore an earlier version — and because a restore is
itself just more edits, it syncs to everyone else too.

@docs Version, version, historyLength, versionAt, restoreTo


# Branching

Because history is a shared op DAG (not a linear snapshot), you can **fork** a document
into an independent branch, edit it in isolation, and later **merge** it back — the merge
is just an ordinary document merge (op-union converges). Fork from the current state
(`fork`) or from any past `version` (`forkAt`), then edit the branch freely; the original
is untouched. `divergence` tells you how far a branch and the mainline have drifted apart
(and hence whether a merge-back is fast-forward, already-merged, or truly divergent).

A branch must edit under its **own** `ReplicaId` (you pass one to `fork`/`forkAt`), so its
edits are concurrent with the mainline's and both survive the merge — see `fork`.

    -- try a change on a branch without disturbing the live doc:
    branch =
        Doc.fork (Crdt.Id.replica "experiment") model.doc
            |> applySomeEdits

    -- decide later: keep it (merge back) or just drop `branch` on the floor
    merged =
        Doc.merge model.doc branch

@docs fork, forkAt, Divergence, divergence


# Undo and redo

Undo here is **per-user**, and different from the time-travel above. Time-travel
(`Crdt.readAt` / `restoreTo`) works on the _whole shared timeline_ — every replica's edits
together — and `restoreTo` rewinds the document for everyone. Undo instead walks back
only **your own** recent actions, and leaves concurrent edits from other people alone: if
you type a word while a collaborator deletes a paragraph elsewhere, your undo removes your
word and nothing else.

It works by emitting the **inverse** of your action as fresh edits, so an undo is itself
a normal, syncing change — it converges like any other, and a peer sees your undo the way
they saw the original. Mark where each undoable step begins by capturing a `version`
before an action and passing it to `recordEdit` after; that lets a burst of keystrokes or
a whole drag collapse into one undo step.

    -- when a logical action starts (focus a field, begin a drag), remember the version:
    startVersion =
        Doc.version model.doc

    -- …after the action's edits are applied, close the step:
    doc =
        Doc.recordEdit startVersion editedDoc

    -- later, wire buttons to it:
    onUndo model =
        if Doc.canUndo model.doc then
            { model | doc = Doc.undo model.doc }

        else
            model

@docs recordEdit, undo, redo, canUndo, canRedo


# Named checkpoints

@docs Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion


# Compacting history

@docs compact, opCount

-}

import Crdt.Doc.Internal as I
import Crdt.Id
import Json.Encode as JE


{-| A collaborative document holding a value of type `a` (the type your schema decodes
to). Opaque: create it with `init`, read it with `read`, edit it through `Crdt`.
-}
type alias Doc a =
    I.Doc a


{-| Merge another replica's document into this one. Commutative, associative and
idempotent: merging in any order, more than once, always converges to the same value —
so you can gossip documents around a network of peers without coordination.

Usually you merge over the wire with `decodeInto` rather than holding two live documents;
`merge` is the in-memory form.

-}
merge : Doc a -> Doc a -> Doc a
merge =
    I.merge


{-| Serialize the whole document for transport or storage (as JSON). Send it to a peer;
they `decodeInto` it. For steady-state syncing prefer `encodeSince`, which sends only
what changed.
-}
encode : Doc a -> JE.Value
encode =
    I.encode


{-| Merge a serialized document (from `encode` / `encodeSince`) that arrived from a peer
into this one. This is the normal sync path. `Err` if the payload is malformed.
-}
decodeInto : JE.Value -> Doc a -> Result String (Doc a)
decodeInto =
    I.decodeInto


{-| Serialize only the changes since a `Version` — a small **delta** to send a peer who
is already up to that point. Track the version you last sent each peer and ship
`encodeSince` from it; far cheaper than `encode` for ongoing collaboration.
-}
encodeSince : Version -> Doc a -> JE.Value
encodeSince =
    I.encodeSince


{-| What a merge changed, as an opaque value you query with your typed refs
(`Crdt.touched` / `Crdt.origins`). Produced by `mergeWithDiff` /
`decodeWithDiff`.
-}
type alias Diff =
    I.Diff


{-| Who authored a change: this replica, or a specific remote one. Inspect it with
`isLocal` and `originReplica`.
-}
type alias Origin =
    I.Origin


{-| Was this change made by the document's own replica (rather than a peer)?
-}
isLocal : Origin -> Bool
isLocal origin =
    originReplica origin == Nothing


{-| The remote replica that authored a change, or `Nothing` if it was local. Use it to
attribute or highlight a collaborator's edits.
-}
originReplica : Origin -> Maybe Crdt.Id.ReplicaId
originReplica origin =
    case origin of
        I.Local ->
            Nothing

        I.Remote r ->
            Just r


{-| Like `merge`, but also return a `Diff` of what changed — so you can react to and
attribute a collaborator's changes (highlight them, notify, scroll into view). See
`Crdt.touched`.
-}
mergeWithDiff : Doc a -> Doc a -> ( Doc a, Diff )
mergeWithDiff =
    I.mergeWithDiff


{-| Like `decodeInto`, but also return a `Diff` of what the incoming changes touched — the
network counterpart of `mergeWithDiff`.
-}
decodeWithDiff : JE.Value -> Doc a -> Result String ( Doc a, Diff )
decodeWithDiff =
    I.decodeWithDiff


{-| The `Diff` of everything that changed since a `Version` — works for your own edits too
(capture a version before an edit, then compare), so a UI can refresh only touched slices
however the change arrived.
-}
diffSince : Version -> Doc a -> Diff
diffSince =
    I.diffSince


{-| A point in the document's shared history — the state at some moment. Two replicas
holding the same edits agree on it, so a `Version` can be stored, sent, and revisited
later. Capture the current one with `version`.
-}
type alias Version =
    I.Version


{-| The document's current version — capture it now to return to "the state as of now"
later (for undo boundaries, checkpoints, or a `diffSince`).
-}
version : Doc a -> Version
version =
    I.version


{-| How many edit steps the document's history holds — the length of the timeline you can
scrub through with `versionAt`.
-}
historyLength : Doc a -> Int
historyLength =
    I.historyLength


{-| The `Version` after the first `n` edits — a scrubber handle into linear history.
`readAt (versionAt n doc) doc` shows the document after its `n`th edit. `n` is clamped to
`[0, historyLength]`.
-}
versionAt : Int -> Doc a -> Version
versionAt =
    I.versionAt


{-| Rewind the document to a past `Version` — but **as new edits**, so the revert itself
syncs to every peer and converges (it doesn't silently rewind only your copy, which a
later merge would undo). Survivors keep their identity; only things deleted since are
re-created fresh.
-}
restoreTo : Version -> Doc a -> Doc a
restoreTo =
    I.restoreTo


{-| Fork an **independent branch** from the current state, editing under a fresh
`ReplicaId`. The branch shares this document's history but diverges from now on: edit it
without touching the original, then `merge` the two back together.

Give the branch its **own** replica id (distinct from this document's and from any peer's).
A `Doc` is immutable, so without a new id the branch would keep minting `OpId`s from the
same `(replica, counter)` sequence as the original — a branch edit and a mainline edit
would mint the _same_ id and collide (one silently wins on merge). A distinct replica makes
the two sides genuinely concurrent, so both survive the merge-back.

-}
fork : Crdt.Id.ReplicaId -> Doc a -> Doc a
fork =
    I.fork


{-| Fork a branch from a **past** `version` instead of the current state — diverge from
"the state as of then". The branch keeps the history up to the fork point and drops
everything after it; the original document is untouched. Contrast `restoreTo`, which
rewinds the _live_ document onto its own timeline. Edits under the given `ReplicaId` (see
`fork` for why that matters); merge back with `merge`.
-}
forkAt : Crdt.Id.ReplicaId -> Version -> Doc a -> Doc a
forkAt =
    I.forkAt


{-| How far a branch and the mainline have drifted: `ahead` = edits on the branch the
mainline lacks, `behind` = edits on the mainline the branch lacks. `ahead == 0` ⇒ nothing
to merge back; `behind == 0` ⇒ a fast-forward; both `> 0` ⇒ genuinely divergent (the merge
integrates concurrent work from both sides).
-}
type alias Divergence =
    I.Divergence


{-| Compare a branch against the mainline — see `Divergence`. Pass them by name to keep
the direction unambiguous:

    Doc.divergence { branch = experiment, mainline = model.doc }

-}
divergence : { branch : Doc a, mainline : Doc a } -> Divergence
divergence =
    I.divergence


{-| Mark an **undo boundary**: record that the edits between the given past `Version` and
now form one undoable step. Call it after a logical action (a keystroke burst, a
drag-reorder) so `undo` collapses that whole action into a single step.
-}
recordEdit : Version -> Doc a -> Doc a
recordEdit =
    I.recordEdit


{-| Undo your own most recent recorded step — by emitting the **inverse** as fresh edits,
so a peer's concurrent edit survives your undo and the undo itself syncs. Local to this
replica (you undo your actions, not everyone's).
-}
undo : Doc a -> Doc a
undo =
    I.undo


{-| Redo the step most recently undone.
-}
redo : Doc a -> Doc a
redo =
    I.redo


{-| Whether there is a step available to `undo`.
-}
canUndo : Doc a -> Bool
canUndo =
    I.canUndo


{-| Whether there is a step available to `redo`.
-}
canRedo : Doc a -> Bool
canRedo =
    I.canRedo


{-| A named point in history — like a git tag. Capture the shared `Version` plus a message
and author, so collaborators can jump back to "before the big reorg" together.
-}
type alias Checkpoint =
    I.Checkpoint


{-| Record a checkpoint at the current version with a message (the author is the
document's replica).
-}
checkpoint : String -> Doc a -> Doc a
checkpoint =
    I.checkpoint


{-| All checkpoints recorded in this document, newest first.
-}
checkpoints : Doc a -> List Checkpoint
checkpoints =
    I.checkpoints


{-| A checkpoint's message.
-}
checkpointMessage : Checkpoint -> String
checkpointMessage =
    I.checkpointMessage


{-| The replica that recorded a checkpoint.
-}
checkpointAuthor : Checkpoint -> Crdt.Id.ReplicaId
checkpointAuthor =
    I.checkpointAuthor


{-| The `Version` a checkpoint points at — pass to `readAt` or `restoreTo`.
-}
checkpointVersion : Checkpoint -> Version
checkpointVersion =
    I.checkpointVersion


{-| Compact history up to a `Version`, dropping the fine-grained steps before it to
reclaim space, while keeping the current value and everything after that point intact.
Use it once old history is no longer needed for undo or time-travel.
-}
compact : Version -> Doc a -> Doc a
compact =
    I.compact


{-| How many operations the document holds — a rough size/complexity gauge (e.g. to decide
when to `compact`).
-}
opCount : Doc a -> Int
opCount =
    I.opCount
