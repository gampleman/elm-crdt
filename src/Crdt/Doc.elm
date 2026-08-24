module Crdt.Doc exposing
    ( Doc
    , read, readAt, ReadError, readErrorToString
    , merge, encode, decodeInto, encodeSince, encodeFrom
    , Diff, Origin, isLocal, originReplica, mergeWithDiff, decodeWithDiff, diffSince, diffBetween
    , touched, origins
    , Version, version, historyLength, versionAt, restoreTo
    , encodeVersion, decodeVersion
    , fork, forkAt, Divergence, divergence
    , recordEdit, undo, redo, canUndo, canRedo
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
    , compact, stableFrontier, opCount
    )

{-| A **document** — the live, collaborative value your application holds and edits.

A `Doc` is a JSON-like document (records, lists, dictionaries, text, counters, and
your own types — whatever your `Crdt` schema describes) that several people can edit at
the same time, on different devices, even while offline, and always end up agreeing on
the result. You never reconcile conflicts by hand: concurrent edits **merge**
automatically and every replica converges to the same value.

You **create** a document with `Crdt.init` (in the `Crdt` module, where the schema lives)
and **edit** it through that module's typed refs, via `Crdt.Edit`. Everything else you do
with a document once you have one is here:

1.  **read** its current typed value (`read`), or its value at a past `Version` (`readAt`);

2.  **sync** it with other replicas by shipping bytes over any transport you like
    (`encode` / `decodeInto`, or the smaller `encodeSince` delta), and merging what
    arrives — the library guarantees convergence, the network is entirely yours;

3.  **react** to what a merge changed (`mergeWithDiff` + `Doc.touched`);

4.  travel through **history** (`version` / `restoreTo`), **undo/redo**, name
    **checkpoints**, and **compact** old history away.

    -- sync: send `Doc.encode doc` to a peer; on receipt:
    doc2 =
    Doc.decodeInto incomingBytes doc
    |> Result.withDefault doc


# The document

You create a document with `Crdt.init` (in the `Crdt` module, where the schema lives), then
everything you do with it once you have one is here — starting with reading its value.

@docs Doc


# Reading the value

`read` decodes the document through its schema to the typed value your app renders; `readAt`
does the same for a past `Version` (time-travel). Both `Err` only on a genuinely corrupt or
schema-incompatible document — a `ReadError`, not a merge conflict.

@docs read, readAt, ReadError, readErrorToString


# Syncing with other replicas

Convergence is the library's job; moving bytes is yours. `encode` a document (or
`encodeSince` a small delta) to a peer over any channel — WebSocket, HTTP, a file — and
`decodeInto` merges what arrives. Edits from any replica, in any order, converge.

@docs merge, encode, decodeInto, encodeSince, encodeFrom


# Reacting to what changed

Merging is automatic, but sometimes you want to _react_ to an incoming change rather than
just re-render the new state (your `view` already reflects that for free). Knowing **what**
a merge touched and **who** did it lets you, for example, flash a highlight on a field a
collaborator just edited, show a "Bob is editing" marker, scroll a remote change into
view, or fire a notification. `mergeWithDiff` / `decodeWithDiff` hand back a `Diff`
alongside the new document; query it with the same typed refs you edit through — `touched`
tells you whether (and by whom) a given spot changed, and `origins` lists every replica
that contributed.

@docs Diff, Origin, isLocal, originReplica, mergeWithDiff, decodeWithDiff, diffSince, diffBetween
@docs touched, origins


# History and time travel

A document remembers how it got to its current state, so you can look at any past point,
scrub through the timeline, and restore an earlier version — and because a restore is
itself just more edits, it syncs to everyone else too.

@docs Version, version, historyLength, versionAt, restoreTo
@docs encodeVersion, decodeVersion


# Branching

Because history is a shared op DAG (not a linear snapshot), you can **fork** a document
into an independent branch, edit it in isolation, and later **merge** it back — the merge
is just an ordinary document merge (op-union converges). Fork from the current state
(`fork`) or from any past `version` (`forkAt`), then edit the branch freely; the original
is untouched. `divergence` tells you how far a branch and the mainline have drifted apart
(and hence whether a merge-back is fast-forward, already-merged, or truly divergent).

A branch must edit under its **own** `ReplicaId` (you pass one to `fork`/`forkAt`), so its
edits are concurrent with the mainline's and both survive the merge — see [`fork`](#fork).

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
(`readAt` / `restoreTo`) works on the _whole shared timeline_ — every replica's edits
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

Old history accumulates — every edit is an op the store keeps so you can time-travel and so
a lagging peer can still merge. **Compaction** is the one operation that reclaims it: fold
history up to a `Version` into a base and drop the ops below it. The materialized value is
unchanged; what you lose is the fine-grained history (time-travel and per-op provenance)
below the cut.

Compaction comes in two forms — **shrink yourself** or **export a shallow copy** — sharing
the same underlying transform:

  - `compact` mutates _your_ document: it reclaims local space, at the cost of your own
    ability to time-travel below the cut.
  - `encodeFrom` leaves you untouched and produces a shallow snapshot to _send_: your live
    doc keeps every op while a peer receives only the state at the cut plus the tail.

Either way the key question is **which cut**. Below the cut, the history is gone — so it must
be a point you (or your peers) will never need to merge from again. Three safe choices:

  - **Local save / single replica** — compact up to your own `version`; there is no
    concurrent peer for that store.

  - **Multiple live replicas** — `stableFrontier` computes the safe cut from the connected
    peers' versions (the point _all_ of them have seen); a peer that's offline is caught up
    by an automatic snapshot when it reconnects.

        -- shrink my own doc up to the point every peer has seen:
        safe =
            Doc.stableFrontier (myVersion :: peerVersions) model.doc

        doc =
            Doc.compact safe model.doc

        -- OR keep my full history but hand peers a compacted copy:
        payloadForPeers =
            Doc.encodeFrom safe model.doc

`encodeFrom` drops the _operations_ below the cut, so a peer receives no per-edit record of
who changed what and when. It is **not** a redaction tool, though: the snapshot it folds
those ops into still carries deleted text (as tombstones that keep their content), removed
dictionary values, and replica-identifying ids — see [`encodeFrom`](#encodeFrom).

@docs compact, stableFrontier, opCount

-}

import Crdt.Doc.Internal as I
import Crdt.Id
import Crdt.Ref.Internal exposing (Ref(..))
import Crdt.Schema.Internal as SI
import Json.Encode as JE


{-| A collaborative document holding a value of type `a` (the type your schema decodes
to). Opaque: create it with `Crdt.init`, read it with `read`, edit it through `Crdt.Edit`.
-}
type alias Doc a =
    I.Doc a


{-| Read the document's current value through its schema. `Err` only if the stored data
can't be interpreted by the schema (a genuinely corrupt document); render the error with
`readErrorToString`.
-}
read : Doc a -> Result ReadError a
read =
    I.read


{-| Read the value as it stood at a past `Version` — true time-travel, without disturbing
the live document. Great for previews and history views.
-}
readAt : Version -> Doc a -> Result ReadError a
readAt =
    I.readAt


{-| Why reading a document through its schema failed — returned by `read` and `readAt`.
Unlike an edit error this is not a race: it means the stored data doesn't match the schema
(a genuinely corrupt or incompatible document). Opaque; render with `readErrorToString`.
-}
type alias ReadError =
    SI.Error


{-| A human-readable description of a `ReadError`.
-}
readErrorToString : ReadError -> String
readErrorToString =
    SI.errorToString


{-| Merge another replica's document into this one. Commutative, associative and
idempotent: merging in any order, more than once, always converges to the same value —
so you can gossip documents around a network of peers without coordination.

Usually you merge over the wire with `decodeInto` rather than holding two live documents;
`merge` is the in-memory form. It handles a `compact`ed argument the same way
`decodeInto` handles a compacted peer's snapshot: history the incoming document folded
into its base is adopted rather than lost.

The exception, for both paths, is two documents that were compacted **independently** at
cuts where neither covers the other — each base then holds folded history the other
lacks, and one result can only carry one base, so the local one wins and the other's
folded history is dropped. That only arises by compacting at a cut a future merge partner
had not reached, which `compact` already documents as unsafe.

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


{-| Serialize a **shallow** copy of the document, keeping the history that comes after the
given `Version` but discarding the history before it.

`encode` serializes the whole document — every operation, back to the first edit.
`encodeFrom v` serializes the document's current value plus only the operations made after
`v`; the earlier operations are collapsed into a starting snapshot. The result is smaller,
and decodes to a document with the **same value** but a shorter history.

The use for it: **archive the full history, share a shallow copy.** Persist `encode doc` for
yourself so you keep undo and time-travel, but send peers `encodeFrom v doc` so they get a
small document without your entire edit log. A good `v` is
[`stableFrontier`](#stableFrontier) over the connected peers (the point they've all already
reached).

**It is not a redaction tool.** It drops the _operations_ below `v` — so the fine-grained
"who typed what, when" is gone — but the snapshot it folds them into carries more than the
document's visible value:

  - a deleted list or text element survives as a **tombstone that still holds its content**,
    along with the anchors giving its position, so deleted text is reconstructible;
  - a removed dictionary key keeps its **value node**;
  - every element `OpId` names the **replica** that minted it.

(Tombstones can't be dropped here the way [`compact`](#compact) drops them when it folds the
whole store, because an operation above `v` may still anchor after one.) So do not reach for
`encodeFrom` to strip sensitive content that was typed and later deleted — it won't. See
[`compact`](#compact) and the module's **Compacting history** section.

-}
encodeFrom : Version -> Doc a -> JE.Value
encodeFrom =
    I.encodeFrom


{-| What a merge changed, as an opaque value you query with your typed refs
(`Doc.touched` / `Doc.origins`). Produced by `mergeWithDiff` /
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
`Doc.touched`.
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


{-| The `Diff` of what changed **between** two versions — the ops at `later` that weren't
yet at `earlier`. Pass two adjacent scrubber steps (`versionAt (n-1)` and `versionAt n`) to
get the single edit that produced step `n`, then `touched` it with a ref to find where it
landed and `originReplica` to attribute it. `earlier` should be an ancestor of `later`.
-}
diffBetween : Version -> Version -> Doc a -> Diff
diffBetween =
    I.diffBetween


{-| Did the spot a `ref` points at — or anything under it, or a container it lives in —
change in `diff`, and if so who? You ask with the same typed refs you edit through (from
your schema bundle); the `Origin` in a `Just` says whether the change was `Local` or
from a particular remote replica. `doc` is the post-merge document.

Use it to _react_ to an incoming change (your `view` already reflects the new state) — e.g.
to flash a highlight on a field a collaborator just touched:

    ( doc1, diff ) =
        Doc.mergeWithDiff model.doc incoming

    highlightTitle =
        case Doc.touched board.title doc1 diff of
            Just origin ->
                not (Doc.isLocal origin)

            Nothing ->
                False

-}
touched : Ref r kind a -> Doc doc -> Diff -> Maybe Origin
touched (Ref r) doc diff =
    I.diffTouches r.path doc diff


{-| Every `Origin` that contributed a change to `diff` — a quick "was there any remote
edit, and whose?" without threading refs.
-}
origins : Diff -> List Origin
origins =
    I.diffOrigins


{-| A point in the document's shared history — the state at some moment. Two replicas
holding the same edits agree on it, so a `Version` can be stored, sent, and revisited
later. Capture the current one with `version`.

It describes **everything the document has**, including history that `compact` has folded
away — which is what lets a peer answer `encodeSince` with a real delta rather than
re-sending ops you have already garbage-collected.

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


{-| Serialize a `Version` to JSON — a tiny payload (a causal frontier is a handful of op
ids), for broadcasting your position to peers. The counterpart to `decodeVersion`. This is
how a stable-frontier compaction policy gathers everyone's version: each peer sends
`encodeVersion (version doc)`, and `stableFrontier` turns the collected versions into a
safe compaction cut.
-}
encodeVersion : Version -> JE.Value
encodeVersion =
    I.encodeVersion


{-| Decode a `Version` from `encodeVersion`. `Err` on malformed input.
-}
decodeVersion : JE.Value -> Result String Version
decodeVersion =
    I.decodeVersion


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
    { ahead : Int, behind : Int }


{-| Compare a branch against the mainline — see [`Divergence`](#Divergence). Pass them by
name to keep the direction unambiguous:

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


{-| The **multi-replica-safe** cut for `compact`: given the `Version`s of the replicas you
might still merge with (typically every connected peer, plus your own `version`), returns
the causal point all of them have already delivered past. Compacting below it drops no op
any listed peer still needs — an op even one of them is missing is kept, and concurrent
work nobody has shipped yet is never included.

The library computes the frontier; **you** decide who is in the list. A peer left out
(disconnected, or lagging) isn't protected by this cut, but that's safe: when it
reconnects, being behind the compacted base, it is caught up by an automatic full-state
snapshot rather than a delta. So the worst case of an aggressive cut is a heavier catch-up
for a straggler, never lost data or divergence. An empty list compacts nothing.

-}
stableFrontier : List Version -> Doc a -> Version
stableFrontier =
    I.stableFrontier


{-| How many operations the document holds — a rough size/complexity gauge (e.g. to decide
when to `compact`).
-}
opCount : Doc a -> Int
opCount =
    I.opCount
