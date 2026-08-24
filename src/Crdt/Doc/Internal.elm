module Crdt.Doc.Internal exposing
    ( Doc, Error(..)
    , init, read, merge
    , Diff, Origin(..), mergeWithDiff, decodeWithDiff, diffSince, diffBetween, diffOrigins, diffTouches
    , setText, setBool, setInt, setString, increment
    , listAppend, listInsert, listRemove, listMove
    , setKey, removeKey
    , contribute, retract
    , opCount, cacheConsistent, pendingCount
    , cursorAt, cursorOffset, cursorRange
    , Version, version, readAt, encodeVersion, decodeVersion
    , historyLength, versionAt, restoreTo
    , fork, forkAt, Divergence, divergence
    , recordEdit, undo, redo, canUndo, canRedo
    , Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
    , compact, stableFrontier
    , encode, encodeSince, encodeFrom, decodeInto
    , seedNodeAt, subValue, subValueAt
    , treeAddChild, treeMoveInto, treeMoveBefore, treeMoveAfter, treeRemove
    , setRichText, setBlockText, mark, clearMark
    , splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock, readBlocks, readBlocksAt
    )

{-| **Internal.** The full op-log document implementation: the opaque `Doc` type and
every operation on it, including the low-level `Path`-addressed edit primitives
(`setText`, `listAppend`, `mark`, `treeAddChild`, `seedNodeAt`, …) that the `Crdt.Edit`
module wraps in its type-safe API.

`Crdt.Doc` is the **public facade** over this module: it re-exposes only the
document-lifecycle surface (read/merge, history, checkpoints, diff, encode/decode); the
cursor surface is re-exposed by `Crdt.Cursor`. The path-addressed edit functions here take
`Path`/`Seed`/`Prim`/`OpId` values that are not part of the public API — application code
edits through `Crdt.Edit`, the only supported write path — so they live here rather than
in the published module.

@docs Doc, Error
@docs init, read, merge
@docs Diff, Origin, mergeWithDiff, decodeWithDiff, diffSince, diffBetween, diffOrigins, diffTouches
@docs setText, setBool, setInt, setString, increment
@docs listAppend, listInsert, listRemove, listMove
@docs setKey, removeKey
@docs contribute, retract
@docs opCount, cacheConsistent, pendingCount
@docs cursorAt, cursorOffset, cursorRange
@docs Version, version, readAt, encodeVersion, decodeVersion
@docs historyLength, versionAt, restoreTo
@docs fork, forkAt, Divergence, divergence
@docs recordEdit, undo, redo, canUndo, canRedo
@docs Checkpoint, checkpoint, checkpoints, checkpointMessage, checkpointAuthor, checkpointVersion
@docs compact, stableFrontier
@docs encode, encodeSince, encodeFrom, decodeInto
@docs seedNodeAt, subValue, subValueAt
@docs treeAddChild, treeMoveInto, treeMoveBefore, treeMoveAfter, treeRemove
@docs setRichText, setBlockText, mark, clearMark
@docs splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock, readBlocks, readBlocksAt

-}

import Array
import Crdt.Cursor.Internal as Cursor exposing (Cursor)
import Crdt.Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (Ctx, OpId, ReplicaId)
import Crdt.Json as Json
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.OpJson as OpJson
import Crdt.OpLog as OpLog exposing (Action(..), Op, OpStore, Target, TargetStep(..))
import Crdt.Path as Path exposing (Path, Seg(..))
import Crdt.Rga as Rga
import Crdt.RichText exposing (Block)
import Crdt.RichText.Internal as RichText
import Crdt.Schema.Internal as SchemaI exposing (Crdt, Seed)
import Crdt.Text as Text
import Crdt.Tree.Internal as Tree
import Dict
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Set


{-| An op-log document for a schema `a`: the op store, the local clock, the
schema, the empty-tree materialization `base`, and a **cached materialized
state** (`cached`) so `read` costs a decode of the current value rather than a
re-fold of every op.

The cache is kept correct incrementally. A local edit's op causally follows
everything already in the store (its `deps` is the current frontier), so
`materialize (store ++ localOp) == applyOp (materialize store) localOp` exactly —
local ops are folded straight onto `cached` without a re-materialization. A
`merge` (or an ingested delta) may interleave ops causally anywhere in the DAG, yet it
too folds only the _added_ ops onto `cached`, because every action is a commutative,
idempotent function of the op set with resolution deferred to read (see `mergeWithDiff`).
Adopting a **new base** (a peer's snapshot, in `rebuild`) does re-materialize from
scratch, since that changes the fold origin — as does `forkAt`, which checks out a past
cut into a branch.

-}
type Doc a
    = Doc
        { -- the schema's decoder, kept kind-erased so `Doc a` needs no kind
          -- param (the root schema's edit-kind is irrelevant once stored — an
          -- `Doc` is only ever *read* through it, and edited via `Ref`s the
          -- caller holds separately).
          decode : Node -> Result SchemaI.Error a
        , base : Node
        , store : OpStore
        , ctx : Ctx
        , cached : Node

        -- The store's causal frontier (tips), cached so a local edit doesn't rescan
        -- the whole op set to stamp its `deps`. `commit` maintains it incrementally
        -- (add the new op ids, drop everything they depend on), and so do the merge /
        -- delta-ingest paths (`OpLog.advanceFrontier` over the added ops); only a
        -- wholesale store swap (`rebuild`, `forkAt`) recomputes it from the store. Was
        -- O(N) per edit via `OpLog.frontier`, making an N-op build O(N²).
        , frontier : OpLog.Frontier

        -- The causal cut that `base` already incorporates. Starts empty (base =
        -- the schema's empty tree). `compact` folds ops at-or-below a frontier into
        -- `base` and advances this; it's the boundary below which history (and
        -- time-travel) has been compacted away.
        , baseFrontier : OpLog.Frontier

        -- Single-slot append fast-path: the (list target, id of the element we
        -- last appended there). Lets consecutive appends to the same list skip
        -- the O(n) element-order walk in `appendAnchor`. Lives here (never `==`-compared),
        -- never in Node/Rga, so it can't corrupt the convergence oracle. Any
        -- other mutation (`commit`) or a `merge` clears it.
        , lastAppend : Maybe ( List TargetStep, OpId )

        -- Sibling of `lastAppend` for TREE `addChild`: the (tree target, parent id,
        -- frac position) of the child we last appended under that parent. A run of
        -- `addChild`s under one parent then skips the O(n) `resolve` in `endPos` — the
        -- new child's position is just after the cached frac. Same discipline as
        -- `lastAppend`: outside Node/Tree so it can't corrupt convergence; cleared by
        -- `commit` and re-set by `treeAddChild`.
        , lastTreeChild : Maybe ( List TargetStep, Maybe OpId, Crdt.Frac.Frac )

        -- Named checkpoints (most recent first). Each pins a `Version` (a causal
        -- frontier) with a label + author, so a checkpoint is collaborative
        -- time-travel, not a local snapshot.
        , checkpoints : List Checkpoint

        -- Loro-style LOCAL undo/redo. Each entry brackets ONE local change with the
        -- versions before/after it. `undo` inverts exactly the ops that change
        -- added (recomputed fresh from the `before → after` range each time, so it
        -- never goes stale as other undos/redos re-mint ids), emitting the inverse
        -- as NEW ops. Because it inverts *specific* ops (delete this element, set
        -- this register) rather than diffing whole states, a peer's concurrent edit
        -- to the same container survives and same-path conflicts resolve by LWW
        -- (the undo, being newer, wins). The reverts are new ops, so they sync.
        --
        -- Local and non-replicated (like checkpoints); preserved across `merge`.
        , undoStack : List UndoEntry
        , redoStack : List UndoEntry

        -- Ops that arrived but could not be applied yet, because they name an element
        -- whose insert we don't hold (see `OpLog.applyOpsWithPending`). Kept so the next
        -- ingest can retry them — without this the cache would drift permanently from
        -- `materialize base store` (which retries them on every fold), and a nested edit
        -- delivered ahead of its container would be lost rather than delayed.
        --
        -- Empty for any causally closed delivery, i.e. always, unless something between
        -- the peers filters or truncates the op set. Derived from (base, store), not
        -- independent state: satisfaction is monotone, so the fixpoint is the same however
        -- the ops arrived.
        , pendingOps : List Op

        -- Id remapping for undo/redo. Undoing a *delete* can't resurrect the
        -- original element (tombstones are permanent) — it re-creates a FRESH copy
        -- with a new id. A *later* inverse op that still names the original id (e.g.
        -- undoing the earlier insert that created it) would then miss the live copy
        -- and orphan it. This table records `originalId → revivedId` so every
        -- subsequent inverse op is retargeted onto the copy that actually exists.
        -- Chains resolve transitively (a copy re-revived maps through). Persistent
        -- across undo/redo steps (the original ops it rewrites never go away), and
        -- local like the stacks. This is Loro's UndoManager remapping.
        , idRemap : Dict.Dict String OpId
        }


{-| One undo/redo step: the document versions immediately **before** and **after** a
single tracked edit. Undo inverts the ops in the `before → after` range; the
inverse is recomputed at undo time (from these frozen versions against the live
store), so the entry stays valid no matter what other undos/redos ran in between.
-}
type alias UndoEntry =
    { before : Version, after : Version }


{-| One reverse step produced by inverting an edit's op. `Rev` is an action ready to
emit verbatim (with a fresh op id). `ReInsert` re-creates a list/text element a
delete removed (fresh id — tombstones are permanent). `ReGraft` re-creates a
deleted tree node **and its whole subtree** as one atomic step, so it inverts back
to a _single_ delete (keeping undo/redo symmetric across cycles).
-}
type RevAction
    = Rev Action
    | ReInsert { container : List TargetStep, elemId : OpId, afterValue : Maybe OpId, content : ElemContent }
    | ReGraft { container : List TargetStep, parent : Maybe OpId, rootId : OpId, source : Tree.Tree Node }
    | ReMark { container : List TargetStep, type_ : String, value : Prim, start : Node.MarkAnchor, end : Node.MarkAnchor }


{-| What a re-inserted element holds, which is whatever its container's elements hold
(`design-docs/16-typed-sequence-content.md`). Undoing a delete has to put back the same
_kind_ of element, and the three take different ops to create: a document goes back with an
`InsertElem`, a character with a one-character `InsertText`, a structural token with an
`InsertToken`.
-}
type ElemContent
    = DocElem Node
    | CharElem String
    | TokenElem Node.BlockToken


{-| A named point in history: a label, the replica that saved it, and the
`Version` (causal frontier) it pins. `readAt (checkpointVersion cp)` time-travels to it.
-}
type Checkpoint
    = Checkpoint
        { message : String
        , author : ReplicaId
        , version : Version
        }


{-| Why an edit failed: the path didn't resolve against the current state, or it
pointed at the wrong kind of node.
-}
type Error
    = PathNotFound String
    | WrongNodeType String



-- LIFECYCLE ------------------------------------------------------------------


{-| A fresh, empty op-document for a replica and schema.
-}
init : ReplicaId -> Crdt kind a -> Doc a
init replica schema =
    let
        ( base, ctx ) =
            SchemaI.emptyNode schema (Id.ctx replica)
    in
    Doc
        { decode = SchemaI.decodeNode schema
        , base = base
        , store = OpLog.empty
        , ctx = ctx
        , cached = base
        , frontier = []
        , lastAppend = Nothing
        , lastTreeChild = Nothing
        , checkpoints = []
        , baseFrontier = []
        , undoStack = []
        , redoStack = []
        , pendingOps = []
        , idRemap = Dict.empty
        }


{-| The current materialized `Node` — the maintained cache (no re-fold).
-}
state : Doc a -> Node
state (Doc d) =
    d.cached


{-| Read the typed value: decode the maintained materialization (`cached`) through the
schema — no re-fold of the log.
-}
read : Doc a -> Result SchemaI.Error a
read ((Doc d) as doc) =
    d.decode (state doc)


{-| Merge another op-document into this one: op-store union, with the clock
advanced past everything seen so future ids never collide. If the incoming document
has **compacted** history we lack, its `base` is adopted first (see `mergeWithDiff`).
-}
merge : Doc a -> Doc a -> Doc a
merge local incoming =
    mergeApplying local incoming |> Tuple.first


{-| Merge, and return **what changed** as a `Diff` you can query with your typed refs
(`touched` / `origins`). The document result is identical to `merge`; the diff
is derived at no extra cost from the ops this merge actually applied (the incoming ops
not already present), each tagged with its `Origin` (which replica authored it, relative
to _this_ document's own replica). Use it to re-read only the changed slices — keeping
untouched slices referentially stable so `Html.Lazy` views over them don't re-render —
and to drive provenance-aware effects.

Op-store union alone is **not** enough when the incoming document has been
[`compact`](#compact)ed: the ops it folded into its `base` are gone from its store, so
they can only arrive as that base. This mirrors `decodeInto`'s snapshot handling (the
wire path for the same situation) — if the incoming base frontier strictly covers ours,
we adopt its base, keep only our ops above it, and re-materialize. Without this,
merging in a peer that had compacted away work we never saw silently dropped it, and
`merge a b` disagreed with `merge b a`.

The other direction needs the mirror of that filter: when **we** are the compacted side,
the ops the incoming store still carries below our cut are already folded into our `base`,
in rebuilt form, so re-importing them would put back the pre-compaction sequence structure.
They are dropped too — see the comment on the union branch.

The one case neither path can resolve is **two documents compacted independently** at
cuts where neither covers the other: each base holds folded ops the other lacks, and a
single result can only carry one base, so the local one is kept. Compacting a replica
whose history a future merge partner has not yet seen already violates `compact`'s
documented contract (see `design-docs/04-gc.md`) — this is that violation showing up as
lost data rather than as an error.

-}
mergeWithDiff : Doc a -> Doc a -> ( Doc a, Diff )
mergeWithDiff local incoming =
    mergeApplying local incoming |> Tuple.mapSecond (diffFromOps (replicaOf local))


{-| The merge itself, reporting the ops it applied rather than a `Diff` built from them.

Both public entry points share this, and only `mergeWithDiff` pays for `diffFromOps` —
which walks the applied ops building a string key per (target, origin) pair. That is
proportional to the delta and entirely wasted on `merge`, the hot path, which throws the
diff away. The op list itself is free: `ingest` already returns it.

-}
mergeApplying : Doc a -> Doc a -> ( Doc a, List Op )
mergeApplying (Doc local) (Doc incoming) =
    if OpLog.frontierCovers incoming.baseFrontier local.baseFrontier && not (OpLog.frontierCovers local.baseFrontier incoming.baseFrontier) then
        -- The incoming base subsumes ours: it incorporates ops that are no longer in
        -- ANY store, so adopt it, keep only our ops above it (the rest are already
        -- folded in), and add their tail. Same shape as `applyPayload`'s snapshot
        -- branch; a new fold origin means a full re-materialize (correctness over
        -- referential identity on this rare catch-up path).
        let
            keptOps =
                OpLog.opsAfter incoming.baseFrontier local.store

            store =
                List.foldl OpLog.insert OpLog.empty (keptOps ++ OpLog.ops incoming.store)

            ( Doc adopted, applied ) =
                rebuild store incoming.base incoming.baseFrontier (Doc local)
        in
        -- `rebuild` catches the clock up over the materialized tree; also observe the
        -- incoming replica's counter, which can exceed every stamp it left behind.
        ( Doc { adopted | ctx = Id.observe (Id.ctxCounter incoming.ctx) adopted.ctx }, applied )

    else
        let
            -- Ops the incoming store still carries but WE have already folded into
            -- `base` must not be re-imported: our base is a rebuilt representation of
            -- them (`Node.compactTombstones` drops tombstones and re-chains the
            -- survivors as a right-spine), so replaying the originals on top puts back
            -- their old `parent`/`side` and the derived Fugue order changes. The reverse
            -- merge takes the snapshot branch above and drops them, so keeping them here
            -- made `merge a b` disagree with `merge b a` whenever one side had
            -- compacted. Ops merely *concurrent* with our cut are not ancestors of it,
            -- so they survive the filter — this drops only what `base` already holds.
            --
            -- Skipped entirely while `baseFrontier` is empty (nothing has been compacted
            -- away), which is the hot delta-sync path: no ancestor walk, just the union.
            incomingOps =
                if List.isEmpty local.baseFrontier then
                    incoming.store

                else
                    List.foldl OpLog.insert OpLog.empty (OpLog.opsAfter local.baseFrontier incoming.store)

            store =
                OpLog.merge local.store incomingOps

            -- the ops this merge adds, in causal order — the incremental fold input.
            added =
                OpLog.addedOpsInOrder local.store store

            -- INCREMENTAL merge: `local.cached` already reflects `local.store`, so fold
            -- only the added ops onto that cache — rather than re-materializing the whole
            -- tree from `base`. Correct because every `Action` is a commutative, idempotent
            -- function of the op *set* with resolution deferred to read (see
            -- `OpLog.addedOpsInOrder`), and it PRESERVES REFERENTIAL IDENTITY: containers no
            -- incoming op touched keep their exact previous reference (structural sharing),
            -- so `Html.Lazy` views over them don't re-render. The `cacheConsistent` invariant
            -- (== full re-materialize) is tested to still hold.
            folded =
                ingest added (Doc local)
        in
        -- a peer's concurrent append may now sit after our cached last id, so the append
        -- fast-path is invalidated. The clock must advance past EVERY stamp, else a later
        -- local edit could mint a colliding/losing id. Both operands are in-memory `Doc`s
        -- that already keep their `Ctx` counter `>=` every stamp they hold (including the
        -- register stamps buried in insert-op seeds, which can exceed the insert op's own
        -- id), so the merged clock is just the larger of the two counters — O(1), no
        -- whole-tree `Node.maxCounter` scan.
        ( Doc
            { local
                | store = store
                , ctx = Id.observe (Id.ctxCounter incoming.ctx) local.ctx
                , cached = folded.cached
                , pendingOps = folded.pending
                , frontier = OpLog.advanceFrontier local.frontier added
                , lastAppend = Nothing
            }
        , folded.applied
        )


{-| Fold newly-arrived ops onto the cache, **together with anything a previous ingest had
to hold back**, and report what landed and what is still waiting.

Retrying the held-back ops is what keeps the incremental cache equal to
`OpLog.materialize base store` (`cacheConsistent`): the full fold retries them on every
materialization, so an incremental fold that dropped them would drift from it for good. The
ops that landed — the new ones _plus_ any pending one this batch unblocked — are the raw
material for the `Diff`, since a pending op landing changes the document just as a fresh
one does.

Costs nothing when nothing is pending, which is every causally closed delivery: the batch
is `added` itself and the applied set needs no filtering.

-}
ingest : List Op -> Doc a -> { cached : Node, applied : List Op, pending : List Op }
ingest added (Doc d) =
    let
        batch =
            d.pendingOps ++ added

        ( cached, pending ) =
            OpLog.applyOpsWithPending d.cached batch

        applied =
            if List.isEmpty pending then
                batch

            else
                let
                    held =
                        pending |> List.map (.id >> Id.opIdToString) |> Set.fromList
                in
                List.filter (\theOp -> not (Set.member (Id.opIdToString theOp.id) held)) batch
    in
    { cached = cached, applied = applied, pending = pending }



-- DIFF -----------------------------------------------------------------------


{-| What a merge/ingest changed: an opaque set of touched locations, each tagged with
the `Origin` that authored the change. Query it with the typed refs you already hold
(`touched ref`, `origins`) — it never exposes an untyped path. Built
from the ops a `mergeWithDiff` / `decodeWithDiff` applied; empty if nothing changed
(e.g. re-merging a peer you already have).
-}
type Diff
    = Diff (List DiffEntry)


{-| One changed location: the identity-addressed container/target an applied op touched,
plus who authored it. Internal — `Target` never leaves the module.
-}
type alias DiffEntry =
    { target : Target, origin : Origin }


{-| Who authored a change: this document's own replica (`Local`), or a specific remote
replica (`Remote`).
-}
type Origin
    = Local
    | Remote ReplicaId


{-| The replica this document edits on behalf of — the `me` every `Origin` is relative to.
-}
replicaOf : Doc a -> ReplicaId
replicaOf (Doc d) =
    Id.ctxReplica d.ctx


{-| Build a diff from applied ops: each op's container/target + an `Origin` derived from
its id's replica relative to `me` (this doc's own replica). Character-level text runs on
the same container collapse to one entry per (target, origin), so a 100-char paste is one
changed location, not 100.
-}
diffFromOps : ReplicaId -> List Op -> Diff
diffFromOps me appliedOps =
    let
        originOf theOp =
            let
                r =
                    Id.opIdReplica theOp.id
            in
            if Id.toString r == Id.toString me then
                Local

            else
                Remote r

        entryOf theOp =
            { target = OpLog.actionTarget theOp.action, origin = originOf theOp }

        -- dedupe by (target, origin) so a run of char ops on one container is one entry
        key entry =
            targetKey entry.target ++ "|" ++ originKey entry.origin

        deduped =
            List.foldl
                (\theOp acc ->
                    let
                        e =
                            entryOf theOp
                    in
                    if Dict.member (key e) acc then
                        acc

                    else
                        Dict.insert (key e) e acc
                )
                Dict.empty
                appliedOps
    in
    Diff (Dict.values deduped)


targetKey : Target -> String
targetKey target =
    target
        |> List.map
            (\step ->
                case step of
                    IntoKey k ->
                        "k:" ++ k

                    IntoElem id ->
                        "e:" ++ Id.opIdToString id
            )
        |> String.join "/"


originKey : Origin -> String
originKey origin =
    case origin of
        Local ->
            "L"

        Remote r ->
            "R:" ++ Id.toString r


{-| Every distinct `Origin` that contributed a change — quick "was there any remote edit,
and whose?" without threading refs. `[]` for an empty diff.
-}
diffOrigins : Diff -> List Origin
diffOrigins (Diff entries) =
    entries
        |> List.map .origin
        |> List.foldl
            (\o acc ->
                if List.member o acc then
                    acc

                else
                    o :: acc
            )
            []


{-| Did the spot at `path` — or anything **under** it, or an **ancestor** container of it
— change in this diff, and if so by whom? `path` is resolved against `doc` to its
identity-addressed target, then compared to each changed target by prefix (either
direction: a change under `path`, or to a container `path` lives in, both count). When
several origins touched it, a `Remote` wins over `Local` (a peer's change is the
interesting one for "did someone else edit this?"). `touched` is the typed front
door; `Path` never appears in a public signature.
-}
diffTouches : Path -> Doc a -> Diff -> Maybe Origin
diffTouches path doc (Diff entries) =
    case resolve path doc of
        Ok ( refTarget, _ ) ->
            entries
                |> List.filter (\e -> targetsOverlap refTarget e.target)
                |> List.map .origin
                |> pickOrigin

        Err _ ->
            -- The ref doesn't resolve in the current state (e.g. its container was
            -- concurrently removed), so there is no target to compare: `targetOfPath`
            -- yields the ROOT target, which prefix-matches every entry. Net effect: an
            -- unresolvable ref reports "touched" whenever the diff is non-empty —
            -- deliberately over-reporting (re-read too much) rather than missing the
            -- change that removed it.
            let
                refTarget =
                    targetOfPath path doc
            in
            entries
                |> List.filter (\e -> targetsOverlap refTarget e.target)
                |> List.map .origin
                |> pickOrigin


{-| `Path` → `Target`, falling back to the **root** target `[]` when the path no longer
resolves at all (there is no partial resolution: `walk` either reaches the end or fails).
Used only by `diffTouches`, where `[]` prefix-matches every changed target.
-}
targetOfPath : Path -> Doc a -> Target
targetOfPath path doc =
    case resolve path doc of
        Ok ( target, _ ) ->
            target

        Err _ ->
            []


{-| Two targets overlap if one is a prefix of the other (a change at `a`, under `a`, or
at an ancestor of `a` all count as touching `a`).
-}
targetsOverlap : Target -> Target -> Bool
targetsOverlap a b =
    isPrefixOf a b || isPrefixOf b a


isPrefixOf : Target -> Target -> Bool
isPrefixOf short long =
    case ( short, long ) of
        ( [], _ ) ->
            True

        ( x :: xs, y :: ys ) ->
            targetStepEq x y && isPrefixOf xs ys

        ( _ :: _, [] ) ->
            False


targetStepEq : TargetStep -> TargetStep -> Bool
targetStepEq a b =
    case ( a, b ) of
        ( IntoKey x, IntoKey y ) ->
            x == y

        ( IntoElem x, IntoElem y ) ->
            Id.opIdToString x == Id.opIdToString y

        _ ->
            False


{-| Prefer a `Remote` origin over `Local` when a spot was touched by both. `Nothing` if
untouched.
-}
pickOrigin : List Origin -> Maybe Origin
pickOrigin origins =
    case List.filter isRemote origins of
        first :: _ ->
            Just first

        [] ->
            case origins of
                first :: _ ->
                    Just first

                [] ->
                    Nothing


isRemote : Origin -> Bool
isRemote origin =
    case origin of
        Local ->
            False

        Remote _ ->
            True


{-| The diff of everything that changed **since** `version` — all ops added after that
frontier, tagged with origin. Works uniformly for local edits and merges (capture the
version before an edit, then `diffSince` after), so a UI can refresh only the touched
slices no matter how the change arrived. `mergeWithDiff` / `decodeWithDiff` give the
same information for the network path without needing a captured version.
-}
diffSince : Version -> Doc a -> Diff
diffSince (Version known) (Doc d) =
    diffFromOps (Id.ctxReplica d.ctx) (OpLog.opsAfter known d.store)


{-| The diff of everything that changed **between** two versions: the ops present at
`later` but not at `earlier` (both frontiers), tagged with origin. `earlier` should be an
ancestor of `later` — e.g. two adjacent `versionAt` steps — so this is the single edit (or
edits) that carried the document from one to the other. Used by a history scrubber to
attribute the edit at a given step to its author.
-}
diffBetween : Version -> Version -> Doc a -> Diff
diffBetween (Version earlier) (Version later) (Doc d) =
    let
        withinLater =
            OpLog.ancestorKeys later d.store
    in
    OpLog.opsAfter earlier d.store
        |> List.filter (\theOp -> Set.member (Id.opIdToString theOp.id) withinLater)
        |> diffFromOps (Id.ctxReplica d.ctx)


{-| How many operations the document holds. Useful to reason about transport
size / delta minimality without exposing the op representation.
-}
opCount : Doc a -> Int
opCount (Doc d) =
    List.length (OpLog.ops d.store)


{-| How many delivered ops are still **held back** because an op they name has not
arrived yet (see `ingest`). Zero for any causally closed delivery — a non-causal
transport is the only way to make it non-zero, and it must return to zero once the
missing ops land. Exposed so properties can assert "nothing is stuck" alongside
`cacheConsistent`, without exposing the op representation.
-}
pendingCount : Doc a -> Int
pendingCount (Doc d) =
    List.length d.pendingOps


{-| Whether the incrementally-maintained read cache equals a full
re-materialization from scratch. Always `True` for any sequence of edits and
merges — the Phase 2 correctness invariant (see `design-docs/02-oplog.md`). Exposed
(rather than the raw `Node`s it compares) so the invariant stays checkable
without leaking the internal state type.
-}
cacheConsistent : Doc a -> Bool
cacheConsistent (Doc d) =
    d.cached == OpLog.materialize d.base d.store



-- WIRE -----------------------------------------------------------------------


{-| Serialize the document for transport as a **full sync**: if the document has
been compacted (`base` holds folded-away history), this is a _snapshot_ — the
materialized base, its frontier, and the live tail ops — so a fresh peer can
catch up even though the early ops are gone. Otherwise it's just the op set.
-}
encode : Doc a -> JE.Value
encode (Doc d) =
    if List.isEmpty d.baseFrontier then
        opsPayload (OpLog.ops d.store)

    else
        snapshotPayload d.base d.baseFrontier (OpLog.ops d.store)


{-| Serialize only what a peer at `Version` is missing — a **delta**. If that peer
is at or ahead of our compacted `baseFrontier`, the delta is just the ops they
lack (`opsAfter`). If they are _behind_ our `baseFrontier`, the ops they need are
gone — so we send a snapshot (base + frontier + tail) instead.
-}
encodeSince : Version -> Doc a -> JE.Value
encodeSince (Version known) (Doc d) =
    if OpLog.frontierCovers known d.baseFrontier then
        -- peer already has everything our base subsumes: a plain op delta
        opsPayload (OpLog.opsAfter known d.store)

    else
        -- peer is behind the compaction boundary: only a snapshot can catch them up
        snapshotPayload d.base d.baseFrontier (OpLog.ops d.store)


{-| Serialize a **shallow** copy of the document as of `cut`: history at or below `cut` is
folded into the exported base and its ops are dropped from the export, while the tail above
`cut` ships as ops. **The source document is untouched** — this is `compact` as a
projection you _send_, not a mutation of yourself.

Decoding it yields a document that **reads identically** to this one (the materialized value
is unchanged) but carries none of the pre-`cut` op log — no per-op provenance, no
time-travel below `cut`, no superseded register values. Two uses:

  - **Keep full history locally, hand peers a compacted view** — archive `encode doc` to
    disk for yourself, but `encodeFrom (stableFrontier peers doc) doc` to peers so they get
    a small doc without your whole edit log. Unlike `compact` + `encode`, your live doc keeps
    every op (undo, time-travel).
  - **Redacting the edit _history_** — export at a `cut` above the history you must not
    disclose and the payload holds no ops below it, so the sequence of edits (who changed
    what, in what order, and the states in between) is gone.

**What it removes vs. keeps:** it drops the _ops/history_ below `cut`; it does **not** remove
the folded state, and that state is more than the currently-visible value. The exported base
still carries **tombstones** — a deleted list/text element keeps its content, a removed
dictionary key keeps its value node — plus the element `OpId`s, which name the replica
that minted them. (Unlike `compact`'s whole-store case, tombstones can't be dropped: a tail
op above `cut` may still anchor after one.) So this scrubs "how the document got here",
not "everything that was ever typed": it is not a data-redaction tool.

-}
encodeFrom : Version -> Doc a -> JE.Value
encodeFrom (Version cut) (Doc d) =
    let
        -- pure compaction: fold ≤cut ops into a fresh base, keep the tail — WITHOUT
        -- installing it back into `self` (the whole point vs. `compact`).
        ( base1, store1 ) =
            OpLog.compact d.base cut d.store
    in
    snapshotPayload base1 cut (OpLog.ops store1)


opsPayload : List Op -> JE.Value
opsPayload ops =
    JE.object
        [ ( "kind", JE.string "ops" )
        , ( "ops", OpJson.encodeOps ops )
        ]


snapshotPayload : Node -> OpLog.Frontier -> List Op -> JE.Value
snapshotPayload base frontier tail =
    JE.object
        [ ( "kind", JE.string "snapshot" )
        , ( "base", Json.encodeNode base )
        , ( "frontier", JE.list Json.encodeOpId frontier )
        , ( "ops", OpJson.encodeOps tail )
        ]


{-| Decode a peer's payload and merge it in. Two shapes:

  - **ops** — union the ops into our store (idempotent) and fold the genuinely-new
    ones onto the cache incrementally.
  - **snapshot** — the peer compacted history we may lack. We union the tail ops
    as usual; and if the snapshot's base is **ahead of ours** (its frontier
    covers our `baseFrontier`, and we're not already past it), we adopt the
    snapshot's base + frontier, dropping our now-redundant ops below it — a new fold
    origin, so that one path re-materializes in full. A peer that is _not_ behind the
    snapshot ignores the base and just takes the ops (incrementally).

The clock is advanced past everything seen.

-}
decodeInto : JE.Value -> Doc a -> Result String (Doc a)
decodeInto value doc =
    decodeApplying value doc |> Result.map Tuple.first


{-| Ingest a wire payload (`encode`/`encodeSince` from a peer) and return **what
changed** as a `Diff`, alongside the updated document — the network counterpart of
`mergeWithDiff`. This is the demo's real incoming-message path; the diff lets it re-read
only the changed slices (keeping the rest referentially stable for `Html.Lazy`) and
attribute the change to the peer that sent it. Same result document as `decodeInto`.
-}
decodeWithDiff : JE.Value -> Doc a -> Result String ( Doc a, Diff )
decodeWithDiff value doc =
    decodeApplying value doc
        |> Result.map (Tuple.mapSecond (diffFromOps (replicaOf doc)))


{-| The decode itself, reporting the ops it applied rather than a `Diff` built from them —
the wire counterpart of `mergeApplying`, and shared by `decodeInto`/`decodeWithDiff` for
the same reason: only the caller that asked for a diff should pay to build one.
-}
decodeApplying : JE.Value -> Doc a -> Result String ( Doc a, List Op )
decodeApplying value doc =
    JD.decodeValue payloadDecoder value
        |> Result.mapError JD.errorToString
        |> Result.map (\payload -> applyPayload payload doc)


type Payload
    = OpsPayload (List Op)
    | SnapshotPayload Node OpLog.Frontier (List Op)


payloadDecoder : Decoder Payload
payloadDecoder =
    JD.field "kind" JD.string
        |> JD.andThen
            (\kind ->
                case kind of
                    "ops" ->
                        JD.map OpsPayload (JD.field "ops" OpJson.opsDecoder)

                    "snapshot" ->
                        JD.map3 SnapshotPayload
                            (JD.field "base" Json.nodeDecoder)
                            (JD.field "frontier" (JD.list Json.opIdDecoder))
                            (JD.field "ops" OpJson.opsDecoder)

                    other ->
                        JD.fail ("unknown payload kind: " ++ other)
            )


applyPayload : Payload -> Doc a -> ( Doc a, List Op )
applyPayload payload (Doc d) =
    case payload of
        OpsPayload incomingOps ->
            -- base unchanged → INCREMENTAL: fold only the added ops onto the existing
            -- cache (preserving referential identity), like `merge`.
            rebuildIncremental incomingOps (Doc d)

        SnapshotPayload snapBase snapFrontier tailOps ->
            if OpLog.frontierCovers snapFrontier d.baseFrontier && not (OpLog.frontierCovers d.baseFrontier snapFrontier) then
                -- the snapshot is strictly ahead of our base: adopt it, keep only
                -- our ops the snapshot doesn't already subsume, plus the tail.
                let
                    keptOps =
                        OpLog.opsAfter snapFrontier d.store

                    store1 =
                        List.foldl OpLog.insert OpLog.empty (keptOps ++ tailOps)
                in
                -- a NEW base is a different fold origin, so re-materialize fully. This is
                -- the rare catch-up path; correctness over identity here.
                rebuild store1 snapBase snapFrontier (Doc d)

            else
                -- we're at/ahead of the snapshot's base: ignore it, take the tail
                -- incrementally (base unchanged).
                rebuildIncremental tailOps (Doc d)


{-| Re-materialize from a (possibly new) base + store and advance the clock. Used when
`base` changes (snapshot adoption), where the fold origin differs from the current cache.
Reports every added op as applied (a snapshot catch-up conservatively treats the whole
delta as changed).
-}
rebuild : OpStore -> Node -> OpLog.Frontier -> Doc a -> ( Doc a, List Op )
rebuild store base baseFrontier (Doc d) =
    let
        -- a full re-fold recomputes what has to be held back from scratch, so the previous
        -- `pendingOps` is discarded rather than carried over (see `ingest`).
        ( cached, pending ) =
            OpLog.materializeWithPending base store

        added =
            OpLog.addedOpsInOrder d.store store
    in
    ( Doc
        { d
            | store = store
            , base = base
            , baseFrontier = baseFrontier
            , cached = cached
            , pendingOps = pending
            , frontier = OpLog.frontier store
            , ctx = Id.observe (Node.maxCounter cached) d.ctx
            , lastAppend = Nothing
        }
    , added
    )


{-| Drop incoming ops that our own `base` **already holds** — the wire counterpart of the
`opsAfter local.baseFrontier` filter in `mergeWithDiff`, and needed for the same reason.

Applying an op is not idempotent by itself; idempotence comes from the _store_
(`addedFromCandidates` skips ops we already have). Compaction takes ops out of the store,
so it takes away the very thing that made a re-delivery harmless — and re-applying a folded
op is worse than a no-op: `Rga.insertElement` replaces an element wholesale from the op's
own seed, so a re-applied insert **reverts every later write into that element**, and `base`
holds the rebuilt tombstone-free spine, so replaying originals on top can change the derived
Fugue order. Both showed up as a peer's redundant re-send silently deleting the receiver's
post-compaction edits (and desynchronizing the cache).

`version` now names the base boundary so a cooperating peer stops re-sending in the first
place; this is the receiver-side guard that does not depend on the sender — a full `encode`,
a relay replaying its table, or an older peer can all still hand over ops we compacted away.

**What it can and cannot prove.** The ancestor walk runs over the incoming batch, so it
drops an op it can reach by walking down from `baseFrontier` _within that batch_. A
causally closed delta (every shape the library itself sends) is fully resolvable. A
deliberately **sparse** batch — one op here, one there, intermediate ops withheld — can hide
an already-folded op from the walk, and that op is re-applied as before. Closing that
residue needs the receiver to remember which ops it folded (an op-id set, or per-replica
counter ranges), which trades away the memory GC is there to reclaim; it is not worth paying
until a transport that sparse actually shows up.

Costs nothing until something has been compacted (`baseFrontier` empty → the batch as-is),
which is the hot delta-sync path.

-}
dropFolded : OpLog.Frontier -> List Op -> List Op
dropFolded baseFrontier incomingOps =
    if List.isEmpty baseFrontier then
        incomingOps

    else
        let
            folded =
                OpLog.ancestorKeys baseFrontier
                    (List.foldl OpLog.insert OpLog.empty incomingOps)
        in
        List.filter (\theOp -> not (Set.member (Id.opIdToString theOp.id) folded)) incomingOps


{-| Ingest a batch of `incomingOps` (a delta, on the SAME base) by folding only the
genuinely-new ones onto the existing cache — the incremental, identity-preserving
counterpart of `rebuild`, mirroring `merge`. `base`/`baseFrontier` are unchanged. Returns
the diff of the added ops.

Everything scales with the **delta size** `k`, not the document `n`: the new ops are found
by scanning the candidate batch (`addedFromCandidates`, O(k)) rather than the merged store
(O(n)); the clock catches up over just those ops' stamps (`opsMaxCounter`, O(k)) rather
than a whole-tree `Node.maxCounter` scan; the frontier advances incrementally. Inserting
the batch into the store is the only O(k·log n) step, which is intrinsic.

-}
rebuildIncremental : List Op -> Doc a -> ( Doc a, List Op )
rebuildIncremental incomingOps (Doc d) =
    let
        added =
            OpLog.addedFromCandidates d.store (dropFolded d.baseFrontier incomingOps)

        store =
            List.foldl OpLog.insert d.store added

        folded =
            ingest added (Doc d)
    in
    ( Doc
        { d
            | store = store
            , cached = folded.cached
            , pendingOps = folded.pending
            , frontier = OpLog.advanceFrontier d.frontier added

            -- catch the clock up past the delta's stamps only (O(delta), including seed
            -- stamps) — not a whole-tree `Node.maxCounter` scan. `d.ctx` already covers
            -- everything we held before.
            , ctx = Id.observe (OpLog.opsMaxCounter added) d.ctx
            , lastAppend = Nothing
        }
    , folded.applied
    )



-- HISTORY / TIME-TRAVEL ------------------------------------------------------


{-| A point in the document's shared history — the causal frontier at some
moment. A `Version` is **collaborative**: it is derived from the op DAG, so any two
peers that hold the same ops agree on it, and it can be carried, stored, and checked
out later.

A `Version` also doubles as a **branch handle** — checking out a version and
continuing to edit from the live document, then comparing, is the basis for
fork/branch workflows.

The ids it carries are **cut points, not necessarily minimal tips**: everything that
reads a `Version` unions the ancestors of every id it holds (`OpLog.ancestorKeys`), so a
redundant id — one already implied by another — narrows nothing and costs nothing. That
is what lets `version` name a compacted document's base boundary alongside its store tips.

-}
type Version
    = Version OpLog.Frontier


{-| The current version: everything this document has, expressed as a causal cut.
Capture it before an edit to be able to return to "the state as of now" later.

That is the store's tips **plus `baseFrontier`**, because ops folded into `base` are no
longer in the store and so are implied by nothing in its frontier. Without them, a peer
asked for "everything since this version" cannot tell "I never had these ops" from "I
compacted them away": `OpLog.ancestorKeys` ignores ids absent from the store it walks, so
as soon as one local edit supersedes the boundary the peer resolves no ancestry at all,
concludes we know nothing, and re-sends its whole store — which
`rebuildIncremental` would then fold on top of a base that already holds it. Naming the
boundary keeps the peer's `opsAfter` walk resolvable, so the delta stays a delta.

-}
version : Doc a -> Version
version (Doc d) =
    Version (OpLog.frontier d.store ++ d.baseFrontier)


{-| Serialize a `Version` (a causal frontier — a small list of op ids) to JSON, so peers
can exchange their versions. This is what a stable-frontier GC policy needs: each replica
broadcasts `encodeVersion (version doc)`, and the others feed the decoded versions to
`stableFrontier` to pick a safe compaction cut. Tiny (a handful of ids), unlike `encode`.
-}
encodeVersion : Version -> JE.Value
encodeVersion (Version frontier) =
    JE.list Json.encodeOpId frontier


{-| Decode a `Version` produced by `encodeVersion`. `Err` on malformed input.
-}
decodeVersion : JE.Value -> Result String Version
decodeVersion value =
    JD.decodeValue (JD.list Json.opIdDecoder) value
        |> Result.map Version
        |> Result.mapError JD.errorToString


{-| The **stable frontier** across a set of peer `Version`s (typically the versions of
every currently-connected replica, including your own): the causal cut every one of them
has delivered past. Passing this to `compact` is the **multi-replica-safe** GC policy
(regime 2 in `design-docs/04-gc.md`) — history below it can be dropped without losing anything a
listed peer still needs, because an op even one peer is missing stays out of the cut, and
not-yet-shipped concurrent work is nobody's ancestor.

The library computes the frontier but does **not** decide who is in the list — the app
gathers connected peers' versions (e.g. each broadcasts its `version`) and passes them.
The safety is exactly "safe across the listed peers": a peer omitted from the list
(offline/behind) is caught up by a snapshot transfer when it reconnects (the wire layer
sends one automatically when a peer is behind `baseFrontier`), so omitting it is not a
correctness bug, just a smaller cut. An empty list yields an empty cut (compact nothing).

-}
stableFrontier : List Version -> Doc a -> Version
stableFrontier peers (Doc d) =
    Version (OpLog.stableFrontier (List.map (\(Version f) -> f) peers) d.store)


{-| Garbage-collect history at a causal cut: fold every op at-or-below `cut`
into `base` and drop those ops from the store, advancing `baseFrontier`. The
**read model is unchanged** (`compact` is equivalence-preserving), so this is
purely a representation shrink — but it is irreversible: you can no longer
`readAt` a version below `cut` (the ops to replay are gone).

**Soundness is the caller's responsibility.** `compact` never loses information
`materialize` would use, but because `merge` is op-union, dropping ops is only
safe across replicas if every replica you will merge with has already
incorporated everything below `cut`. Passing your own `version` is always safe
for a _local_ store (single replica / before persistence); passing a frontier a
future merge partner hasn't reached can drop their not-yet-merged concurrent work
below `cut`. See `design-docs/04-gc.md`.

-}
compact : Version -> Doc a -> Doc a
compact (Version cut) (Doc d) =
    let
        ( base1, store1 ) =
            OpLog.compact d.base cut d.store

        storeEmpty =
            List.isEmpty (OpLog.ops store1)

        -- When the cut folds in the WHOLE store (`store1` empty — e.g. `compact (version
        -- doc) doc`), physically drop the now-settled sequence/text tombstones from `base`
        -- too: no remaining op can anchor to them, so the right-spine rebuild is safe and
        -- the read model is still identical. (References from *inside* `base` do survive —
        -- rich-text mark anchors name character ids — so `Node.compactTombstones` re-anchors
        -- them onto survivors rather than assuming nothing points at a tombstone.) With a
        -- tail left (`store1` non-empty) a tail op could still anchor after a dropped
        -- tombstone, so we keep tombstones then.
        -- (Dropping tombstones has the SAME cross-replica soundness envelope as dropping
        -- ops — safe below a stable cut; `compact`'s existing contract already owns that.)
        base2 =
            if storeEmpty then
                Node.compactTombstones base1

            else
                base1

        -- `cached == materialize base store`; with `store1` empty, `base1` IS that
        -- materialization, so the tombstone-compacted `base2` is the new cache (visible
        -- value unchanged). With a tail, base/cache are untouched by the tombstone pass.
        cached1 =
            if storeEmpty then
                base2

            else
                d.cached

        -- Undo/redo entries are version RANGES whose inverses are recomputed from the
        -- ops each time (see `inverseBetween`), so an entry only survives while the ops
        -- it names are still individually in the store. Compaction is exactly what takes
        -- them away, and an entry that has lost them does not fail loudly — it produces
        -- NO inverses (or, worse, inverses computed against a `checkout` polluted by the
        -- new base) while `canUndo` keeps reporting `True` and `undo` keeps popping the
        -- stack. So drop the entries this cut invalidates, and keep the honest ones.
        --
        -- An entry survives iff everything folded away is at-or-below its `before`
        -- version: then no op of its own range was folded, AND checking out `before`
        -- against the new base still yields the same state. Slightly conservative — an
        -- op below the cut that stayed behind as *pending* is counted as folded — which
        -- costs an undo entry, never correctness. (`forkAt` clears both stacks outright;
        -- a branch starts its own history, whereas compaction is the same history with
        -- less of it retained.)
        foldedKeys =
            OpLog.ancestorKeys cut d.store

        survives entry =
            case entry.before of
                Version beforeFrontier ->
                    Set.diff foldedKeys (OpLog.ancestorKeys beforeFrontier d.store)
                        |> Set.isEmpty
    in
    Doc
        { d
            | base = base2
            , store = store1
            , baseFrontier = cut
            , cached = cached1
            , undoStack = List.filter survives d.undoStack
            , redoStack = List.filter survives d.redoStack

            -- `ctx` is unchanged: it is stored independently and already exceeds every
            -- stamp, so dropping tombstone stamps can only lower `Node.maxCounter`, never
            -- make the clock unsafe.
            , lastAppend = Nothing
        }


{-| The materialized `Node` as of a `Version` — only ops causally at or before
that frontier are folded. Newer ops (and concurrent ops from peers) are excluded.
-}
stateAt : Version -> Doc a -> Node
stateAt (Version frontier) (Doc d) =
    OpLog.checkout frontier d.base d.store


{-| Read the typed value as of a `Version` — time-travel through the schema.
The live document is unchanged; this is a read-only view of the past.
-}
readAt : Version -> Doc a -> Result SchemaI.Error a
readAt v ((Doc d) as doc) =
    d.decode (stateAt v doc)


{-| How many ops the live history holds — the number of distinct edit steps you
can scrub through. `versionAt 0` is the start of the live history (the empty document,
or the state at the compaction cut once `compact` has folded ops into `base` — those
are no longer scrubbable); `versionAt (historyLength doc)` is the current state.
-}
historyLength : Doc a -> Int
historyLength (Doc d) =
    -- the count, not the order: a linearization has exactly as many ops as the store, so
    -- `causalOrder` here was sorting the whole log to measure a list it then threw away.
    OpLog.size d.store


{-| The `Version` after the first `step` ops in causal order — a scrubber handle
into linear history. `readAt (versionAt n doc) doc` shows the document as it stood
after its `n`th edit. `step` is clamped to `[0, historyLength]`.

A prefix of the causal order is downward-closed (every op's deps precede it), so
the frontier of that prefix checks out exactly those ops.

-}
versionAt : Int -> Doc a -> Version
versionAt step (Doc d) =
    OpLog.causalOrder d.store
        |> List.take (max 0 step)
        |> OpLog.frontierOfOps
        |> Version


{-| Revert the live document to a past `Version`, **as new edits** — `restoreTo`
diffs the past state against the current one and emits the fresh, winning ops that
turn the latter back into the former. Because it goes through the op log like any
other edit, the revert **syncs to peers and converges**; it does not silently
rewind only the local replica (which a later merge would clobber).

Identity is preserved where it can be: the past state replays a subset of the same
ops that built the present, so unchanged registers/elements keep their ids (a
cursor or in-flight move anchored to a surviving item still resolves). Only items
that were _deleted_ since the version are re-created with fresh ids — the originals
are tombstoned forever. Restoring to the current version is a no-op.

-}
restoreTo : Version -> Doc a -> Doc a
restoreTo v doc =
    restoreNode [] (stateAt v doc) (state doc) doc


{-| Fork an **independent branch** from the current state, editing under a new
`ReplicaId`. The branch shares this document's whole history, but its future edits
diverge: edit it freely without touching the original, then bring the two back
together with `merge` (op-union — a branch merge is just a document merge).

The new replica id is essential. A `Doc` is immutable, so a plain copy would keep
minting `OpId`s from the _same_ `(replica, counter)` sequence as the original — so an
edit on the branch and an edit on the mainline would mint the **same id** and collide
(one silently wins on merge). Re-keying to a distinct replica makes the two sides
genuinely concurrent, so both survive the merge-back. Pick an id unique among your
replicas (a fresh random id, or `"<base>-branch"`).

-}
fork : ReplicaId -> Doc a -> Doc a
fork branchReplica doc =
    forkAt branchReplica (version doc) doc


{-| Fork a branch from a **past** `Version` rather than the current state — diverge from
"the state as of then", editing under a new `ReplicaId`. The branch keeps only the ops
that are causal ancestors of `at` (its history _is_ the mainline up to the fork point);
everything after the fork point is dropped from the branch. Merge the branch back with
`merge` when done.

Unlike `restoreTo` (which rewinds the _live_ document onto its own timeline as new
ops), `forkAt` produces a **separate** document you edit in isolation — the original is
untouched. This is the building block for "try a change on a branch, compare, then keep
or discard it".

The branch's clock is preserved (kept ahead of every inherited stamp) but re-keyed to
`branchReplica`, so its new edits are concurrent with the mainline's — see `fork`.
Local-only state (undo/redo stacks, checkpoints, append fast-paths) resets: a branch
starts its own local history.

-}
forkAt : ReplicaId -> Version -> Doc a -> Doc a
forkAt branchReplica (Version cut) (Doc d) =
    let
        -- keep only the ops at-or-below the fork point; the branch's history is the
        -- mainline up to `at`, and it diverges from there.
        store =
            OpLog.ancestorsOf cut d.store

        -- a checkout is a fresh fold from `base`, so what has to be held back is
        -- recomputed for the branch's own (smaller) op set rather than inherited.
        ( cached, pending ) =
            OpLog.materializeWithPending d.base store
    in
    Doc
        { d
            | store = store
            , cached = cached
            , pendingOps = pending
            , frontier = OpLog.frontier store

            -- re-key to the branch replica, keeping the clock ahead of every inherited
            -- stamp so branch edits neither collide with nor lose to mainline edits.
            , ctx = Id.withReplica branchReplica d.ctx

            -- a branch starts its own local history; shared/replicated state (base,
            -- baseFrontier, the ops themselves) carries over, local bookkeeping does not.
            , checkpoints = []
            , undoStack = []
            , redoStack = []
            , idRemap = Dict.empty
            , lastAppend = Nothing
            , lastTreeChild = Nothing
        }


{-| How far two documents have diverged: the ops each holds that the other lacks. `ahead`
is what `branch` would contribute to `mainline` on a merge (the branch's new work);
`behind` is what `mainline` has that `branch` hasn't seen yet. Both zero ⇒ the two are at
the same version. Defined purely over the causal DAG, so it is symmetric and
delivery-order-independent (see `OpLog.opsAfter`).
-}
type alias Divergence =
    { ahead : Int, behind : Int }


{-| Compare a `branch` against a `mainline` document: `ahead` = ops on the branch that
mainline lacks, `behind` = ops on mainline the branch lacks. Use before a merge-back to
preview the scope of a branch, or to tell whether it is fast-forward (`behind == 0`),
already-merged (`ahead == 0`), or truly divergent (both nonzero).
-}
divergence : { branch : Doc a, mainline : Doc a } -> Divergence
divergence { branch, mainline } =
    let
        (Doc b) =
            branch

        (Doc m) =
            mainline

        -- how many ops in `has` are absent from `other` — a plain store set-difference.
        -- (Not `opsAfter`, which resolves a frontier *within a single store*: the two
        -- docs have different stores and each other's tips are absent, so frontier
        -- ancestry can't be walked across them. Op ids are globally unique, so
        -- membership is the sound cross-store comparison.)
        onlyIn has other =
            OpLog.ops has
                |> List.filter (\o -> not (OpLog.member o.id other))
                |> List.length
    in
    { ahead = onlyIn b.store m.store
    , behind = onlyIn m.store b.store
    }


{-| Emit one op against the current frontier, advancing the clock and folding it
onto the cache (the same O(1) path as any single edit).
-}
emit : Action -> Doc a -> Doc a
emit action doc =
    let
        ( id, doc1 ) =
            mint doc
    in
    commit [ op id (frontierOf doc1) action ] doc1


{-| Emit an `InsertElem` whose op id _is_ the new element id (the convention the
materializer relies on), returning that id so callers can position it afterwards.

Anchors as a **right-child of `after`** (`side = Right`) — i.e. "immediately after
`after`", or a head root when `after` is `Nothing`. This is the anchor rule for
appends and for undo re-insertion (revival), where we always want to sit right after
a known predecessor. Fugue's `Left`-side anchoring comes from `fuguePlacement` (text and
list inserts, block splits) and from `indentBlock`, which pick parent/side themselves to
keep concurrent runs from interleaving.

-}
emitInsert : List TargetStep -> Maybe OpId -> Node -> Doc a -> ( OpId, Doc a )
emitInsert target after seed doc =
    let
        ( elemId, doc1 ) =
            mint doc
    in
    ( elemId
    , commit [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, parent = after, side = Rga.Right, seed = seed }) ] doc1
    )


{-| Emit a one-character `InsertText` anchored as a right-child of `after`, with `charId` as
both the op id and the character's element id. Used to revive a deleted character on undo:
`emitInsert`'s `InsertElem` cannot land in a text or rich sequence, whose elements hold a
character rather than a document (`design-docs/16-typed-sequence-content.md`).

One character means one derived id (`start` itself), so no id span has to be reserved.

-}
emitTextRun : List TargetStep -> OpId -> String -> Maybe OpId -> Doc a -> Doc a
emitTextRun target charId char after doc =
    commit
        [ op charId
            (frontierOf doc)
            (InsertText { container = target, start = charId, text = char, parent = after, side = Rga.Right })
        ]
        doc


ctxOf : Doc a -> Ctx
ctxOf (Doc d) =
    d.ctx


withCtx : Ctx -> Doc a -> Doc a
withCtx ctx (Doc d) =
    Doc { d | ctx = ctx }


{-| Emit the ops that turn `current` (at `target`) back into `old`. Recurses
structurally; under one schema both nodes always share a shape at every path.
-}
restoreNode : List TargetStep -> Node -> Node -> Doc a -> Doc a
restoreNode target old current doc =
    case ( old, current ) of
        ( Node.Reg ro, Node.Reg rc ) ->
            if ro.value == rc.value then
                doc

            else
                emit (SetReg target ro.value) doc

        ( Node.Cnt _, Node.Cnt _ ) ->
            let
                diff =
                    Maybe.withDefault 0 (Node.asCounter old) - Maybe.withDefault 0 (Node.asCounter current)
            in
            if diff == 0 then
                doc

            else
                emit (Increment { target = target, delta = diff }) doc

        ( Node.Map mo, Node.Map mc ) ->
            restoreMap target mo mc doc

        ( Node.Seq _, Node.Seq _ ) ->
            restoreSeq target (visibleElems old) (visibleElems current) doc

        ( Node.Txt oldRga, Node.Txt curRga ) ->
            -- text restores by DIFFING THE STRING, not by re-inserting elements: that is
            -- the same minimal insert/delete run `setText` emits, so surviving characters
            -- keep their ids (cursors and marks anchored to them still resolve) and only
            -- what actually changed moves. `restoreSeq`'s element-wise path can't be used
            -- here — a text element's content is a character, so there is no subtree to
            -- recurse into and no `Node` seed to re-insert with.
            applyTextDiff target curRga (Text.read oldRga) doc

        ( Node.Mov mo, Node.Mov mc ) ->
            restoreMov target (MoveList.toEntries mo) (MoveList.toEntries mc) doc

        ( Node.Tree to, Node.Tree tc ) ->
            restoreTree target to tc doc

        _ ->
            -- shapes differ (shouldn't happen under a fixed schema): leave as-is
            doc


{-| Make the tree at `target` read like `old` again, diffing against `current`:

  - a node in BOTH (by id) that moved → move it back to its `old` parent/pos, and
    recurse into its payload;
  - a node only in `current` (added since) → delete it;
  - a node only in `old` (deleted since) → revive it + its subtree with fresh ids
    under its `old` parent.

Id-agnostic: works off the two node maps, so it's stable no matter how many other
undos/redos have re-minted ids around it.

-}
restoreTree : List TargetStep -> Tree.Tree Node -> Tree.Tree Node -> Doc a -> Doc a
restoreTree target old current doc =
    let
        oldIds =
            treeNodeIds old

        curIds =
            treeNodeIds current

        oldKeys =
            List.map Id.opIdToString oldIds |> Set.fromList

        curKeys =
            List.map Id.opIdToString curIds |> Set.fromList

        -- 1. delete nodes that exist now but not in `old` (added since). Deleting a
        -- parent hides its subtree, so only delete nodes whose parent is NOT itself
        -- being deleted (avoids redundant ops).
        toDelete =
            curIds
                |> List.filter (\id -> not (Set.member (Id.opIdToString id) oldKeys))
                |> List.filter
                    (\id ->
                        case Tree.parentOf id current of
                            Just p ->
                                Set.member (Id.opIdToString p) oldKeys

                            Nothing ->
                                True
                    )

        afterDeletes =
            List.foldl (\id d -> emit (DeleteElem { container = target, elem = id }) d) doc toDelete

        -- 2. revive nodes in `old` not present now (deleted since). Only revive
        -- subtree roots (whose parent still exists or is a root), to avoid double.
        toRevive =
            oldIds
                |> List.filter (\id -> not (Set.member (Id.opIdToString id) curKeys))
                |> List.filter
                    (\id ->
                        case Tree.parentOf id old of
                            Just p ->
                                Set.member (Id.opIdToString p) curKeys

                            Nothing ->
                                True
                    )

        afterRevives =
            List.foldl
                (\id d -> reviveNode target (Tree.parentOf id old) id old d)
                afterDeletes
                toRevive

        -- 3. for nodes in BOTH: move back if reparented/reordered, recurse payload
        shared =
            oldIds |> List.filter (\id -> Set.member (Id.opIdToString id) curKeys)

        afterMoves =
            List.foldl (restoreTreeNode target old) afterRevives shared
    in
    afterMoves


{-| For a node present in both `old` and the live doc: move it back to its `old`
parent/pos if it differs, then restore its payload.
-}
restoreTreeNode : List TargetStep -> Tree.Tree Node -> OpId -> Doc a -> Doc a
restoreTreeNode target old id doc =
    let
        liveTree =
            navigateTarget target (state doc) |> Maybe.andThen Node.asTree

        oldParent =
            Tree.parentOf id old

        moved =
            case liveTree of
                Just t ->
                    (Tree.parentOf id t /= oldParent)
                        || (Tree.siblingPos id t /= Tree.siblingPos id old)

                Nothing ->
                    False

        afterMove =
            if moved then
                case Tree.siblingPos id old of
                    Just pos ->
                        emit (TreeMove { container = target, child = id, parent = oldParent, pos = pos, seed = Nothing }) doc

                    Nothing ->
                        doc

            else
                doc

        -- restore the node's payload (recurse via the node ref path)
        payloadTarget =
            target ++ [ IntoElem id ]

        afterPayload =
            case ( Tree.get id old, navigateTarget payloadTarget (state afterMove) ) of
                ( Just oldContent, Just curContent ) ->
                    restoreNode payloadTarget oldContent curContent afterMove

                _ ->
                    afterMove
    in
    afterPayload


{-| All node ids in a tree, roots-first pre-order (parents before children, so a
revive/delete of a parent is emitted before its descendants).
-}
treeNodeIds : Tree.Tree c -> List OpId
treeNodeIds t =
    let
        go ids =
            ids |> List.concatMap (\id -> id :: go (Tree.childrenOf id t))
    in
    go (Tree.roots t)


{-| The visible (id, content) pairs of a `Seq` node, in order.

`Txt` is not included: its elements hold characters, not documents, so it restores by
diffing the string rather than by re-inserting elements one at a time — see `restoreNode`.

-}
visibleElems : Node -> List ( OpId, Node )
visibleElems node =
    case Node.asSeq node of
        Just rga ->
            Rga.toElementsInOrder rga
                |> List.filter (not << .deleted)
                |> List.map (\e -> ( e.id, e.content ))

        Nothing ->
            []


restoreMap : List TargetStep -> Dict.Dict String Node.Entry -> Dict.Dict String Node.Entry -> Doc a -> Doc a
restoreMap target mo mc doc =
    -- Emit the sum-type `$tag` key LAST so its op causally depends on the payload
    -- ops (each `emit` chains on the current frontier). Otherwise `$tag` — which
    -- sorts first — flips before its payload is created, and a downward-closed
    -- prefix (a history scrub, or a peer's deps-respecting op set) can expose the
    -- new tag with no payload yet, reading as `MissingField "variant argument 0"`.
    (Dict.keys mo ++ Dict.keys mc)
        |> Set.fromList
        |> Set.toList
        |> List.partition (\k -> k == SchemaI.tagKey)
        |> (\( tagKeys, otherKeys ) -> otherKeys ++ tagKeys)
        |> List.foldl (\k d -> restoreMapKey (target ++ [ IntoKey k ]) (Dict.get k mo) (Dict.get k mc) d) doc


restoreMapKey : List TargetStep -> Maybe Node.Entry -> Maybe Node.Entry -> Doc a -> Doc a
restoreMapKey keyTarget mOld mCur doc =
    case ( mOld, mCur ) of
        ( Just oe, Just ce ) ->
            let
                d1 =
                    if oe.present == ce.present then
                        doc

                    else
                        -- the key is right here, so this only flips presence; the skeleton
                        -- is for a peer that merges the flip without the creation under it
                        emit
                            (SetKeyPresence
                                { target = keyTarget
                                , present = oe.present
                                , seed = Node.vacate ce.value |> Maybe.withDefault emptyMap
                                }
                            )
                            doc
            in
            if oe.present then
                restoreNode keyTarget oe.value ce.value d1

            else
                d1

        ( Just oe, Nothing ) ->
            -- Key existed at the version but not now: create it empty, then restore its
            -- value with ordinary ops (see `setKey` — a presence op only ever carries the
            -- canonical skeleton). A key that was *tombstoned* at the version needs
            -- nothing: absent and tombstoned read alike, and a removal cannot create the
            -- entry it would tombstone.
            if not oe.present then
                doc

            else
                case Node.vacate oe.value of
                    Just skeleton ->
                        emit (SetKeyPresence { target = keyTarget, present = True, seed = skeleton }) doc
                            |> restoreNode keyTarget oe.value skeleton

                    Nothing ->
                        -- a kind the diff cannot rebuild from empty: carry the value whole,
                        -- deep-restamped (tombstones are permanent, so the copy needs new
                        -- identity)
                        let
                            ( seedNode, ctx1 ) =
                                Node.reStamp (ctxOf doc) oe.value
                        in
                        emit (SetKeyPresence { target = keyTarget, present = True, seed = seedNode }) (withCtx ctx1 doc)

        ( Nothing, Just ce ) ->
            -- key added after the version: tombstone it
            if ce.present then
                emit (SetKeyPresence { target = keyTarget, present = False, seed = emptyMap }) doc

            else
                doc

        ( Nothing, Nothing ) ->
            doc


{-| Restore a `Seq`/`Txt`. Sequences cannot reorder, so survivors keep their
order: delete current-only elements, recurse into kept ones (identity preserved),
and re-insert version-only ones (deleted since) as fresh elements, chained into
position.
-}
restoreSeq : List TargetStep -> List ( OpId, Node ) -> List ( OpId, Node ) -> Doc a -> Doc a
restoreSeq target oldEls curEls doc =
    let
        oldKeys =
            List.map (Tuple.first >> Id.opIdToString) oldEls |> Set.fromList

        curById =
            List.map (\( id, c ) -> ( Id.opIdToString id, c )) curEls |> Dict.fromList

        afterDeletes =
            List.foldl
                (\( id, _ ) d ->
                    if Set.member (Id.opIdToString id) oldKeys then
                        d

                    else
                        emit (DeleteElem { container = target, elem = id }) d
                )
                doc
                curEls
    in
    List.foldl
        (\( oid, oContent ) ( d, after ) ->
            case Dict.get (Id.opIdToString oid) curById of
                Just cContent ->
                    ( restoreNode (target ++ [ IntoElem oid ]) oContent cContent d, Just oid )

                Nothing ->
                    let
                        ( seedNode, ctx1 ) =
                            Node.reStamp (ctxOf d) oContent

                        ( newId, d1 ) =
                            emitInsert target after seedNode (withCtx ctx1 d)
                    in
                    ( d1, Just newId )
        )
        ( afterDeletes, Nothing )
        oldEls
        |> Tuple.first


{-| Restore a `Mov` (movable list). Reconcile membership like `restoreSeq`
(delete extras, recurse into survivors, re-insert deleted), then a positioning
pass moves each item after the previous one's home cell so the final visible order
matches the version's.
-}
restoreMov : List TargetStep -> List ( OpId, Node ) -> List ( OpId, Node ) -> Doc a -> Doc a
restoreMov target oldEntries curEntries doc =
    let
        oldKeys =
            List.map (Tuple.first >> Id.opIdToString) oldEntries |> Set.fromList

        curById =
            List.map (\( id, c ) -> ( Id.opIdToString id, c )) curEntries |> Dict.fromList

        afterDeletes =
            List.foldl
                (\( id, _ ) d ->
                    if Set.member (Id.opIdToString id) oldKeys then
                        d

                    else
                        emit (DeleteElem { container = target, elem = id }) d
                )
                doc
                curEntries

        -- recurse into survivors / re-insert deleted; collect the live valueId of
        -- each version entry, in the version's order
        ( afterContent, liveOrder ) =
            List.foldl
                (\( oid, oContent ) ( d, acc ) ->
                    case Dict.get (Id.opIdToString oid) curById of
                        Just cContent ->
                            ( restoreNode (target ++ [ IntoElem oid ]) oContent cContent d, acc ++ [ oid ] )

                        Nothing ->
                            let
                                ( seedNode, ctx1 ) =
                                    Node.reStamp (ctxOf d) oContent

                                ( newId, d1 ) =
                                    emitInsert target Nothing seedNode (withCtx ctx1 d)
                            in
                            ( d1, acc ++ [ newId ] )
                )
                ( afterDeletes, [] )
                oldEntries
    in
    -- positioning pass: chain each item after the previous one's current home cell
    List.foldl
        (\valueId ( d, prev ) ->
            let
                anchor =
                    case prev of
                        Nothing ->
                            Nothing

                        Just prevId ->
                            navigateTarget target (state d)
                                |> Maybe.andThen Node.asMov
                                |> Maybe.andThen (MoveList.homeCell prevId)
            in
            ( emit (MoveElem { container = target, elem = valueId, after = anchor }) d, Just valueId )
        )
        ( afterContent, Nothing )
        liveOrder
        |> Tuple.first



-- LOCAL UNDO / REDO ----------------------------------------------------------


{-| Record a local change for undo, given the version **before** it and the doc
**after** it. The library does not know which of your `Doc` calls form one
user-level "edit", so you bracket them: capture `version doc` before, make your
edits, then call `recordEdit before edited`. The version range bracketing exactly the
ops added in between is pushed onto the undo stack (the inverse itself is computed later,
at `undo` time) and the redo stack is cleared (a new edit forks history), matching every
editor's undo model.

This is **local, Loro-style** undo: `undo` later inverts _your_ ops as fresh ops,
so a peer's concurrent edit to another field survives and the revert still syncs.
A no-op change (no ops added) records nothing.

-}
recordEdit : Version -> Doc a -> Doc a
recordEdit before ((Doc d) as doc) =
    let
        after =
            version doc
    in
    if before == after then
        -- no ops added: nothing to undo (a no-op edit)
        doc

    else
        Doc { d | undoStack = { before = before, after = after } :: d.undoStack, redoStack = [] }


{-| Whether there is a local edit to undo.
-}
canUndo : Doc a -> Bool
canUndo (Doc d) =
    not (List.isEmpty d.undoStack)


{-| Whether there is an undone local edit to redo.
-}
canRedo : Doc a -> Bool
canRedo (Doc d) =
    not (List.isEmpty d.redoStack)


{-| Undo the most recent recorded local edit: invert exactly the ops the edit added
(between its `before` and `after` versions) and emit them as fresh ops, so the undo
syncs to peers and only touches what the edit changed. Records the inverse range on
the redo stack. No-op when there is nothing to undo.

Robust across sequences: the inverse is recomputed from the frozen version range
each time, so earlier undos/redos re-minting ids never invalidate this entry.

-}
undo : Doc a -> Doc a
undo ((Doc d) as doc) =
    case d.undoStack of
        [] ->
            doc

        entry :: rest ->
            let
                -- the doc version *immediately before* this undo emits its ops; the
                -- redo must invert exactly the ops this undo adds, i.e. the range
                -- (preUndo, postUndo] — NOT (entry.after, postUndo], which would also
                -- sweep in ops emitted by earlier undos between entry.after and now.
                preUndo =
                    version doc

                applied =
                    applyRevs (inverseBetween entry.before entry.after doc) doc

                redoEntry =
                    { before = preUndo, after = version applied }
            in
            case applied of
                Doc ad ->
                    Doc { ad | undoStack = rest, redoStack = redoEntry :: ad.redoStack }


{-| Redo the most recently undone edit: symmetric to `undo` — invert the undo's own
ops (the range it recorded), restoring the edit.
-}
redo : Doc a -> Doc a
redo ((Doc d) as doc) =
    case d.redoStack of
        [] ->
            doc

        entry :: rest ->
            let
                -- symmetric to `undo`: the new undo entry must cover only the ops
                -- THIS redo emits — the range (preRedo, postRedo].
                preRedo =
                    version doc

                applied =
                    applyRevs (inverseBetween entry.before entry.after doc) doc

                undoEntry =
                    { before = preRedo, after = version applied }
            in
            case applied of
                Doc ad ->
                    Doc { ad | redoStack = rest, undoStack = undoEntry :: ad.undoStack }


{-| The reverse actions that undo the ops added between `before` and `after` — the
ops causally in `after` but not `before`, inverted, newest-first (so they unwind in
reverse emission order). Each inverse is computed against the state as of _just
before_ that op applied, so a register set inverts to its prior value, a delete to
a re-create, etc.
-}
inverseBetween : Version -> Version -> Doc a -> List RevAction
inverseBetween (Version beforeFrontier) (Version afterFrontier) (Doc d) =
    let
        beforeKeys =
            OpLog.ancestorKeys beforeFrontier d.store

        afterKeys =
            OpLog.ancestorKeys afterFrontier d.store

        -- ops in the (before, after] range: ancestors of `after`, not of `before`.
        added =
            OpLog.causalOrder d.store
                |> List.filter
                    (\o ->
                        Set.member (Id.opIdToString o.id) afterKeys
                            && not (Set.member (Id.opIdToString o.id) beforeKeys)
                    )
    in
    -- fold forward from the before-checkout, materializing each op's pre-state,
    -- collecting inverses; reverse so undo unwinds last-emitted first.
    added
        |> List.foldl
            (\o ( preState, acc ) ->
                ( OpLog.applyOps preState [ o ]
                , inverseOf o preState ++ acc
                )
            )
            ( OpLog.checkout beforeFrontier d.base d.store, [] )
        |> Tuple.second


{-| The reverse action(s) for one op, given the state **just before** it applied.
-}
inverseOf : Op -> Node -> List RevAction
inverseOf theOp preState =
    case theOp.action of
        SetReg target _ ->
            case navigateTarget target preState |> Maybe.andThen Node.asPrim of
                Just prior ->
                    [ Rev (SetReg target prior) ]

                Nothing ->
                    []

        Increment { target, delta } ->
            [ Rev (Increment { target = target, delta = -delta }) ]

        SetKeyPresence { target, present } ->
            -- the key is there in `preState` (this op is what changed it), so the seed only
            -- matters to a peer that gets the inverse without it: carry the same canonical
            -- skeleton a creation would (see `setKey`).
            [ Rev
                (SetKeyPresence
                    { target = target
                    , present = not present
                    , seed =
                        navigateTarget target preState
                            |> Maybe.andThen Node.vacate
                            |> Maybe.withDefault emptyMap
                    }
                )
            ]

        InsertElem { container, elemId } ->
            [ Rev (DeleteElem { container = container, elem = elemId }) ]

        InsertText { container, start, text } ->
            -- undo a run: delete each char it created. The run's char ids are the
            -- derived span start .. start+len-1 (see `OpLog.insertTextRun`).
            let
                replica =
                    Id.opIdReplica start

                base =
                    Id.opIdCounter start
            in
            List.range 0 (String.length text - 1)
                |> List.map (\i -> Rev (DeleteElem { container = container, elem = Id.opId (base + i) replica }))

        InsertToken { container, elemId } ->
            [ Rev (DeleteElem { container = container, elem = elemId }) ]

        MoveElem { container, elem } ->
            case navigateTarget container preState of
                Just node ->
                    [ Rev (MoveElem { container = container, elem = elem, after = priorAnchor elem node }) ]

                Nothing ->
                    []

        DeleteElem { container, elem } ->
            -- undo a delete. Tombstones are permanent, so this revives a FRESH copy
            -- (new id), content/structure preserved, identity not.
            case navigateTarget container preState of
                Just (Node.Tree t) ->
                    -- a tree node: revive it + its subtree as ONE atomic graft, so
                    -- it inverts back to a single delete (keeps undo/redo symmetric).
                    if Tree.get elem t == Nothing then
                        []

                    else
                        [ ReGraft { container = container, parent = Tree.parentOf elem t, rootId = elem, source = t } ]

                Just node ->
                    case elementContent elem node of
                        Just content ->
                            [ ReInsert { container = container, elemId = elem, afterValue = priorAnchor elem node, content = content } ]

                        Nothing ->
                            []

                Nothing ->
                    []

        TreeMove { container, child, seed } ->
            case seed of
                Just _ ->
                    -- op CREATED this node: undo = delete it (subtree hides with it)
                    [ Rev (DeleteElem { container = container, elem = child }) ]

                Nothing ->
                    -- op RE-PARENTED: move it back to its prior parent/pos
                    case navigateTarget container preState |> Maybe.andThen Node.asTree of
                        Just t ->
                            case Tree.siblingPos child t of
                                Just pos ->
                                    [ Rev (TreeMove { container = container, child = child, parent = Tree.parentOf child t, pos = pos, seed = Nothing }) ]

                                Nothing ->
                                    []

                        Nothing ->
                            []

        AddMark { container, type_, start, end } ->
            -- undo a mark by re-asserting, as a fresh (higher-id) op over the same
            -- range, the value that range had *before* this op — sampled from the
            -- pre-state (see RichText.valueInRange for the uniform-range caveat).
            case navigateTarget container preState |> Maybe.andThen Node.asRich of
                Just r ->
                    [ ReMark
                        { container = container
                        , type_ = type_
                        , value = RichText.valueInRange r type_ start end
                        , start = start
                        , end = end
                        }
                    ]

                Nothing ->
                    []


{-| Apply reverse actions as fresh ops (each syncs). Ids named by the inverse ops
are resolved through (and revivals recorded into) the doc's `idRemap` table, so a
delete undone as a fresh copy stays targetable by any later inverse — see `idRemap`.
-}
applyRevs : List RevAction -> Doc a -> Doc a
applyRevs revs doc =
    List.foldl applyRev doc revs


applyRev : RevAction -> Doc a -> Doc a
applyRev rev doc =
    case rev of
        Rev action ->
            emit (remapAction (remapOf doc) action) doc

        ReInsert { container, elemId, afterValue, content } ->
            let
                remap =
                    remapOf doc

                target =
                    remapTarget remap container

                after =
                    liveAnchor target (Maybe.map (remapId remap) afterValue) (state doc)

                -- a revival always mints a FRESH element id (tombstones are permanent), so
                -- every kind returns the new id to record against the original
                ( newId, doc1, innerRemap ) =
                    case content of
                        DocElem node ->
                            let
                                ( seedNode, ctx1, contentRemap ) =
                                    Node.reStampWithMap (ctxOf doc) node

                                ( created, d1 ) =
                                    emitInsert target after seedNode (withCtx ctx1 doc)
                            in
                            ( created, d1, contentRemap )

                        CharElem ch ->
                            -- a one-character run: the text op, not `InsertElem`, because a
                            -- text/rich element holds a character. No inner ids to remap.
                            let
                                ( charId, d1 ) =
                                    mint doc
                            in
                            ( charId
                            , emitTextRun target charId ch after d1
                            , Dict.empty
                            )

                        TokenElem token ->
                            let
                                ( tokenId, d1 ) =
                                    mint doc
                            in
                            ( tokenId
                            , emitBlockElemWithId tokenId target after Rga.Right token d1
                            , Dict.empty
                            )
            in
            -- record original elem → its fresh revived id, PLUS every id inside the
            -- re-stamped content (`innerRemap`), so later inverses that name the
            -- original element OR anything within it retarget onto this live copy.
            doc1
                |> registerRemapAll innerRemap
                |> registerRemap elemId newId

        ReGraft { container, parent, rootId, source } ->
            let
                remap =
                    remapOf doc
            in
            reviveNode (remapTarget remap container) (Maybe.map (remapId remap) parent) rootId source doc

        ReMark { container, type_, value, start, end } ->
            let
                remap =
                    remapOf doc
            in
            emitMark (remapTarget remap container) type_ value (remapAnchor remap start) (remapAnchor remap end) doc


{-| Emit an `AddMark` op (the op id doubles as the mark id) over `[start, end]`.
-}
emitMark : List TargetStep -> String -> Prim -> Node.MarkAnchor -> Node.MarkAnchor -> Doc a -> Doc a
emitMark container type_ value start end doc =
    let
        ( markId, doc1 ) =
            mint doc
    in
    commit
        [ op markId (frontierOf doc1) (AddMark { container = container, markId = markId, type_ = type_, value = value, start = start, end = end }) ]
        doc1


{-| Remap a mark anchor's `ref` through the undo id-remap table.
-}
remapAnchor : Dict.Dict String OpId -> Node.MarkAnchor -> Node.MarkAnchor
remapAnchor table anchor =
    { anchor | ref = Maybe.map (remapId table) anchor.ref }


{-| The doc's current id-remap table.
-}
remapOf : Doc a -> Dict.Dict String OpId
remapOf (Doc d) =
    d.idRemap


{-| Record `original → replacement` in the remap table.
-}
registerRemap : OpId -> OpId -> Doc a -> Doc a
registerRemap original replacement (Doc d) =
    Doc { d | idRemap = Dict.insert (Id.opIdToString original) replacement d.idRemap }


{-| Merge a whole `originalId → revivedId` map (from `Node.reStampWithMap`) into the
remap table. Existing entries win (they were recorded by more recent revivals).
-}
registerRemapAll : Dict.Dict String OpId -> Doc a -> Doc a
registerRemapAll mapping (Doc d) =
    Doc { d | idRemap = Dict.union d.idRemap mapping }


{-| Resolve an id through the remap table, transitively (a revived copy that was
itself later revived chains through). Ids are globally fresh, so the chain is acyclic
and terminates.
-}
remapId : Dict.Dict String OpId -> OpId -> OpId
remapId table id =
    case Dict.get (Id.opIdToString id) table of
        Just next ->
            remapId table next

        Nothing ->
            id


remapTarget : Dict.Dict String OpId -> Target -> Target
remapTarget table =
    List.map
        (\step ->
            case step of
                IntoElem id ->
                    IntoElem (remapId table id)

                IntoKey _ ->
                    step
        )


{-| Rewrite every element id an inverse action names through the remap table.
-}
remapAction : Dict.Dict String OpId -> Action -> Action
remapAction table action =
    case action of
        SetReg target prim ->
            SetReg (remapTarget table target) prim

        SetKeyPresence r ->
            SetKeyPresence { r | target = remapTarget table r.target }

        InsertElem r ->
            InsertElem { r | container = remapTarget table r.container, elemId = remapId table r.elemId, parent = Maybe.map (remapId table) r.parent }

        InsertText r ->
            InsertText { r | container = remapTarget table r.container, parent = Maybe.map (remapId table) r.parent }

        InsertToken r ->
            InsertToken { r | container = remapTarget table r.container, elemId = remapId table r.elemId, parent = Maybe.map (remapId table) r.parent }

        DeleteElem r ->
            DeleteElem { container = remapTarget table r.container, elem = remapId table r.elem }

        MoveElem r ->
            MoveElem { container = remapTarget table r.container, elem = remapId table r.elem, after = Maybe.map (remapId table) r.after }

        Increment r ->
            Increment { r | target = remapTarget table r.target }

        TreeMove r ->
            TreeMove { r | container = remapTarget table r.container, child = remapId table r.child, parent = Maybe.map (remapId table) r.parent }

        AddMark r ->
            AddMark
                { r
                    | container = remapTarget table r.container
                    , start = remapAnchor table r.start
                    , end = remapAnchor table r.end
                }


{-| The element to anchor _after_ to put `elem` back where it was: the id of its
predecessor in the current order, or `Nothing` if it was at the head.
-}
priorAnchor : OpId -> Node -> Maybe OpId
priorAnchor elem node =
    let
        elemKey =
            Id.opIdToString elem
    in
    orderedIds node
        |> Maybe.withDefault []
        |> List.foldl
            (\id ( prev, found ) ->
                if found then
                    ( prev, found )

                else if Id.opIdToString id == elemKey then
                    ( prev, True )

                else
                    ( Just id, False )
            )
            ( Nothing, False )
        |> Tuple.first


{-| Resolve a re-insert anchor against the live container: use the recorded
predecessor if still present, else fall back to the head.
-}
liveAnchor : List TargetStep -> Maybe OpId -> Node -> Maybe OpId
liveAnchor container afterValue root =
    case afterValue of
        Nothing ->
            Nothing

        Just a ->
            case navigateTarget container root |> Maybe.andThen orderedIds of
                Just ids ->
                    if List.any (\id -> Id.opIdToString id == Id.opIdToString a) ids then
                        Just a

                    else
                        Nothing

                Nothing ->
                    Nothing


{-| What element `elem` holds, in whichever sequence container `node` is.

Tombstoned elements included: the caller is undoing a delete, so the element it wants is
precisely the one that was just tombstoned.

-}
elementContent : OpId -> Node -> Maybe ElemContent
elementContent elem node =
    case node of
        Node.Mov ml ->
            MoveList.get elem ml |> Maybe.map DocElem

        Node.Seq rga ->
            Rga.get elem rga |> Maybe.map (.content >> DocElem)

        Node.Txt rga ->
            Rga.get elem rga |> Maybe.map (.content >> CharElem)

        Node.Rich r ->
            Rga.get elem r.text
                |> Maybe.map
                    (\el ->
                        case el.content of
                            Node.TextChar ch ->
                                CharElem ch

                            Node.Token token ->
                                TokenElem token
                    )

        _ ->
            Nothing


{-| Revive tree node `sourceId` (from `source`) and its subtree under `newParent`
in the live doc, as fresh create ops. Each node gets a new id (the original is
tombstoned); children are created under their re-minted parent, preserving order.
-}
reviveNode : List TargetStep -> Maybe OpId -> OpId -> Tree.Tree Node -> Doc a -> Doc a
reviveNode container newParent sourceId source doc =
    case Tree.get sourceId source of
        Nothing ->
            doc

        Just content ->
            let
                ( childId, ctx1 ) =
                    Id.nextId (ctxOf doc)

                -- fresh ids for the payload subtree too, keeping the inner id map so
                -- a later inverse referencing something inside the payload (e.g. a
                -- text char a follow-up insert anchored after) still resolves.
                ( seedNode, ctx2, innerRemap ) =
                    Node.reStampWithMap ctx1 content

                -- restore the node at its ORIGINAL sibling position (from `source`),
                -- not the end, so undo puts it back where it was among its siblings.
                pos =
                    Tree.siblingPos sourceId source
                        |> Maybe.withDefault (treeEndPos container newParent (state doc))

                doc1 =
                    withCtx ctx2 doc
                        |> emit (TreeMove { container = container, child = childId, parent = newParent, pos = pos, seed = Just seedNode })
                        |> registerRemapAll innerRemap
                        -- record original node id → its fresh revived id, so a later
                        -- inverse that still names the original node retargets here.
                        |> registerRemap sourceId childId
            in
            -- recurse into the source node's children, under the new id
            List.foldl
                (\srcChild d -> reviveNode container (Just childId) srcChild source d)
                doc1
                (Tree.childrenOf sourceId source)


{-| The end position for a new child under `parent` in the live tree at `container`
(so a revived node lands after existing siblings).
-}
treeEndPos : List TargetStep -> Maybe OpId -> Node -> Frac
treeEndPos container parent root =
    case navigateTarget container root |> Maybe.andThen Node.asTree of
        Just t ->
            let
                siblings =
                    case parent of
                        Just p ->
                            Tree.childrenOf p t

                        Nothing ->
                            Tree.roots t
            in
            case List.reverse siblings |> List.head of
                Just last ->
                    Crdt.Frac.between (Tree.siblingPos last t) Nothing

                Nothing ->
                    Crdt.Frac.between Nothing Nothing

        Nothing ->
            Crdt.Frac.between Nothing Nothing



-- STABLE CURSORS -------------------------------------------------------------


{-| Make a stable `Cursor` for a visible `offset` within the text/list addressed
by `path`. The cursor anchors to element identity, so it stays meaningful as
other replicas edit around it — resolve it back with `cursorOffset`.

`offset` 0 anchors before the first element; otherwise it anchors _after_ the
element currently at visible index `offset - 1`. Fails if `path` doesn't resolve
to a sequence/text container.

-}
cursorAt : Path -> Int -> Doc a -> Result Error Cursor
cursorAt path offset doc =
    resolve path doc
        |> Result.andThen
            (\( tgt, node ) ->
                case cursorIds node of
                    Just ids ->
                        let
                            anchor =
                                if offset <= 0 then
                                    Cursor.Start

                                else
                                    -- anchor to the element just before `offset`;
                                    -- clamp past-the-end to the last element.
                                    case List.drop (offset - 1) ids |> List.head of
                                        Just id ->
                                            Cursor.After id

                                        Nothing ->
                                            case List.reverse ids |> List.head of
                                                Just id ->
                                                    Cursor.After id

                                                Nothing ->
                                                    Cursor.Start
                        in
                        Ok (Cursor.fromParts tgt anchor)

                    Nothing ->
                        Err (WrongNodeType "expected a text or list at the cursor path")
            )


{-| Resolve a `Cursor` to its current visible offset in this document. `Nothing`
if the cursor's container no longer exists here, or if this document **cannot place the
anchor at all**.

For `Seq`/`Txt`/`Rich` this is robust across deletion of the anchored element, whose
tombstone still marks the spot (see `Crdt.Rga.liveCountThroughWith`). What it cannot do is
place an anchor the sequence has never heard of, which happens two ways: `compact`
physically dropped the tombstone, or the insert that mints the anchor **has not arrived
yet** (ops are not delivered in causal order — see `pendingOps`). Both yield `Nothing`, so
a caller renders no caret rather than a confidently wrong one; a cursor is ephemeral
presence data, so the next broadcast from that peer resolves normally.

For `Mov` it counts live values at-or-before the anchor in the current order, and an
unplaceable anchor still falls back to the end (`countThrough`): there, deleting a value
removes it from the visible order too, so "absent" is the ordinary delete case rather than
the lost-anchor one.

-}
cursorOffset : Cursor -> Doc a -> Maybe Int
cursorOffset cursor doc =
    let
        node =
            navigateTarget (Cursor.steps cursor) (state doc)
    in
    case Cursor.anchor cursor of
        Cursor.Start ->
            node |> Maybe.andThen cursorIds |> Maybe.map (always 0)

        Cursor.After id ->
            node |> Maybe.andThen (offsetOfAnchor id)


{-| The character offset just past `anchor`, per container.

The three RGA-backed containers take the **tombstone-robust** path: the count runs over the
Fugue order, which retains tombstones, so a deleted anchor still has a position and the
caret lands at the nearest surviving spot. Rich text counts **characters only** — matching
`cursorIds` and the offsets `markRange` and the editor speak in — while `Seq`/`Txt` have
nothing but characters, so their predicate is a constant. That difference used to be a
runtime guess over `Prim`; `RichElem` makes it a `case`.

The anchor must actually **be** in the sequence: a count that never meets it silently
returns the live total, i.e. a caret at the end, which is indistinguishable from a real
end-of-text caret. Absent means unplaceable (tombstone compacted away, or the insert has not
arrived), so say so with `Nothing`.

`Mov` has no tombstones in its visible order — deleting a value removes it — so "absent" is
the ordinary delete case there and falling back to the end is right.

-}
offsetOfAnchor : OpId -> Node -> Maybe Int
offsetOfAnchor anchor node =
    let
        inRga keep rga =
            Rga.get anchor rga
                |> Maybe.map (\_ -> Rga.liveCountThroughWith keep anchor rga)
    in
    case node of
        Node.Rich r ->
            inRga (richCharOf >> (/=) Nothing) r.text

        Node.Txt rga ->
            inRga (always True) rga

        Node.Seq rga ->
            inRga (always True) rga

        _ ->
            cursorIds node |> Maybe.map (countThrough anchor)


{-| 1 + the index of `anchor` in `ids` (i.e. the offset just after it); if the
anchor isn't present, the count of all ids (caret at the end).
-}
countThrough : OpId -> List OpId -> Int
countThrough anchor ids =
    let
        anchorKey =
            Id.opIdToString anchor

        go n remaining =
            case remaining of
                [] ->
                    n

                x :: rest ->
                    if Id.opIdToString x == anchorKey then
                        n + 1

                    else
                        go (n + 1) rest
    in
    go 0 ids


{-| Resolve a selection `Range` to a `(start, end)` pair of visible offsets in
this document, normalized so `start <= end`. `Nothing` if either endpoint's
container is gone.
-}
cursorRange : Cursor.Range -> Doc a -> Maybe ( Int, Int )
cursorRange r doc =
    Maybe.map2
        (\a f -> ( min a f, max a f ))
        (cursorOffset (Cursor.rangeAnchor r) doc)
        (cursorOffset (Cursor.rangeFocus r) doc)


{-| The visible element/value ids of an ordered node — `Seq`/`Txt`/`Rich` (RGA) **or**
`Mov` (movable list) — in order. The uniform "ordered, id-addressed sequence" view that list
edits and cursors resolve against, so they work for every kind.

The four RGA cases are spelled out rather than sharing one accessor: the containers hold
different content types now (`design-docs/16-typed-sequence-content.md`), and `visibleIds`
never looks at content, so it is the same call four times over four types.

-}
orderedIds : Node -> Maybe (List OpId)
orderedIds node =
    case node of
        Node.Mov ml ->
            Just (MoveList.toEntries ml |> List.map Tuple.first)

        Node.Seq rga ->
            Just (Rga.visibleIds rga)

        Node.Txt rga ->
            Just (Rga.visibleIds rga)

        Node.Rich r ->
            Just (Rga.visibleIds r.text)

        _ ->
            Nothing


{-| The ids a **cursor** offset indexes into — like `orderedIds`, except a rich-text node
yields its **characters only**.

A `Rich` sequence interleaves characters with block markers and nest tokens, but every
offset a caller hands us is a character offset: that is what the editor reports, what the
span stream reads as, and what `markRange` marks over. Counting markers here would drift the
caret one position per preceding block boundary (invisible in a single-block document, which
is why it went unnoticed). `Seq`/`Txt`/`Mov` have no non-character elements, so this is
`orderedIds` for them.

-}
cursorIds : Node -> Maybe (List OpId)
cursorIds node =
    case Node.asRich node of
        Just r ->
            liveChars richCharOf r.text |> List.map Tuple.first |> Just

        Nothing ->
            orderedIds node


{-| The id of the element/value at visible index `i` of an ordered node.
-}
elemIdAt : Int -> Node -> Maybe OpId
elemIdAt i node =
    orderedIds node |> Maybe.andThen (List.drop i >> List.head)


{-| The id of the last visible element/value (the append anchor).
-}
lastElemId : Node -> Maybe OpId
lastElemId node =
    orderedIds node |> Maybe.andThen (List.reverse >> List.head)


{-| Navigate an **id-based** `Target` into a node, returning the addressed node.
Mirrors `walk` but keyed by element id rather than visible index, so it is the
read-only inverse used to resolve a stable cursor. Handles `Mov` (value-by-id) as
well as `Seq`/`Txt` (element-by-id).
-}
navigateTarget : Target -> Node -> Maybe Node
navigateTarget tgt node =
    case tgt of
        [] ->
            Just node

        (IntoKey k) :: rest ->
            Node.asMap node
                |> Maybe.andThen (Dict.get k)
                |> Maybe.andThen (\entry -> navigateTarget rest entry.value)

        (IntoElem id) :: rest ->
            case Node.asMov node of
                Just ml ->
                    MoveList.get id ml
                        |> Maybe.andThen (\content -> navigateTarget rest content)

                Nothing ->
                    case Node.asTree node of
                        Just t ->
                            -- descend into a tree node by its stable id
                            Tree.get id t
                                |> Maybe.andThen (\content -> navigateTarget rest content)

                        Nothing ->
                            -- a `Seq` element holds a document to descend into; a text or
                            -- rich element holds a character, so nothing does
                            Node.asSeq node
                                |> Maybe.andThen (Rga.get id)
                                |> Maybe.andThen (\el -> navigateTarget rest el.content)


{-| Save a named checkpoint pinning the current version. Records the label and
the saving replica; does not change the document (no op is emitted).
-}
checkpoint : String -> Doc a -> Doc a
checkpoint message ((Doc d) as doc) =
    let
        cp =
            Checkpoint
                { message = message
                , author = Id.ctxReplica d.ctx
                , version = version doc
                }
    in
    Doc { d | checkpoints = cp :: d.checkpoints }


{-| All saved checkpoints, most recent first.
-}
checkpoints : Doc a -> List Checkpoint
checkpoints (Doc d) =
    d.checkpoints


{-| The label of a checkpoint.
-}
checkpointMessage : Checkpoint -> String
checkpointMessage (Checkpoint cp) =
    cp.message


{-| The replica that saved a checkpoint.
-}
checkpointAuthor : Checkpoint -> ReplicaId
checkpointAuthor (Checkpoint cp) =
    cp.author


{-| The version a checkpoint pins (pass to `readAt` to time-travel to it).
-}
checkpointVersion : Checkpoint -> Version
checkpointVersion (Checkpoint cp) =
    cp.version



-- EMITTING OPS ---------------------------------------------------------------


{-| Append ops to the log and advance the clock. Every op in the batch causally follows
everything already in the log — either directly (`deps` = the frontier as it stood before
the batch) or transitively through an earlier op of the same batch (`applyCharDiff` chains
its run) — so the ops apply straight onto the cached state in emission order: no
re-materialization (the O(1) hot path).
-}
commit : List Op -> Doc a -> Doc a
commit newOps (Doc d) =
    Doc
        { d
            | store = List.foldl OpLog.insert d.store newOps
            , cached = OpLog.applyOps d.cached newOps
            , frontier = OpLog.advanceFrontier d.frontier newOps

            -- any committed edit invalidates the append fast-paths by default;
            -- `emitAppend` / `treeAddChild` re-establish them for a genuine append.
            , lastAppend = Nothing
            , lastTreeChild = Nothing
        }


{-| Mint a fresh op id, advancing the clock.
-}
mint : Doc a -> ( OpId, Doc a )
mint (Doc d) =
    let
        ( id, ctx1 ) =
            Id.nextId d.ctx
    in
    ( id, Doc { d | ctx = ctx1 } )



-- PRIMITIVE SETTERS ----------------------------------------------------------


{-| Set a register leaf (LWW) to a primitive.
-}
setPrim : Path -> Prim -> Doc a -> Result Error (Doc a)
setPrim path prim doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (SetReg target prim) ] doc1
            )


{-| Set a boolean register.
-}
setBool : Path -> Bool -> Doc a -> Result Error (Doc a)
setBool path b =
    setPrim path (PBool b)


{-| Set an integer register.
-}
setInt : Path -> Int -> Doc a -> Result Error (Doc a)
setInt path n =
    setPrim path (PInt n)


{-| Set a string register (overwrite; for collaborative text use `setText`).
-}
setString : Path -> String -> Doc a -> Result Error (Doc a)
setString path s =
    setPrim path (PString s)


{-| Add `delta` to a counter field (use a negative `delta` to decrement).
Concurrent increments from different replicas sum, rather than one clobbering the
other.
-}
increment : Path -> Int -> Doc a -> Result Error (Doc a)
increment path delta doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (Increment { target = target, delta = delta }) ] doc1
            )



-- TEXT -----------------------------------------------------------------------


{-| Edit a text field so it reads as `value`, emitting the minimal run of
character insert/delete ops (a common-prefix/suffix diff) so concurrent edits in
other regions survive.
-}
setText : Path -> String -> Doc a -> Result Error (Doc a)
setText path value doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case node of
                    Node.Txt rga ->
                        Ok (applyTextDiff target rga value doc)

                    _ ->
                        Err (WrongNodeType "expected text node for setText")
            )


{-| Edit the character content of a **rich-text** field so its plain text reads as
`value` (a minimal insert/delete diff, like `setText`). Marks are untouched; because
they anchor to surviving characters, formatting follows the edited text.
-}
setRichText : Path -> String -> Doc a -> Result Error (Doc a)
setRichText path value doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case node of
                    Node.Rich r ->
                        Ok (applyRichTextDiff target r.text value doc)

                    _ ->
                        Err (WrongNodeType "expected rich-text node for setRichText")
            )


{-| Edit the text of **block `blockIndex`** so that block reads as `value`, diffing
only that block's characters. This is the per-block edit an editor should use: it
anchors inserts inside the block (so text can't leak across a block marker into the
wrong block — the whole-document `setRichText` can't express that). Marks/other
blocks are untouched.
-}
setBlockText : Path -> Int -> String -> Doc a -> Result Error (Doc a)
setBlockText path blockIndex value doc =
    withRich path
        doc
        (\target r ->
            let
                -- char elements grouped by block (every marker opens the next block;
                -- block 0 is the run before the first marker), plus the markers bounding
                -- this block: its own marker (left) and the next block's marker (right).
                block =
                    charElemsOfBlock r blockIndex
            in
            applyCharDiff target r.text block.chars block.leftAnchor block.rightAnchor value doc
        )


{-| The char elements belonging to block `blockIndex` (in order), plus the markers that
bound it: `leftAnchor` = the block's own marker (or `Nothing` for block 0, which has no
marker → document head), `rightAnchor` = the marker opening the **next** block
(or `Nothing` if this is the last block → document end). Both anchors are needed so a
text insert into an _empty_ block is bounded on both sides and lands strictly between
the two markers, instead of floating past the next marker into the following block.
Walks the live element stream tracking the block index: every marker opens the next
block, so block 0 is the run before the first one.
-}
charElemsOfBlock :
    Node.RichNode
    -> Int
    -> { chars : List ( OpId, String ), leftAnchor : Maybe OpId, rightAnchor : Maybe OpId }
charElemsOfBlock r blockIndex =
    let
        live =
            Rga.toElementsInOrder r.text |> List.filter (not << .deleted)

        -- fold state: current block index `bi`, the target block's marker (`marker`,
        -- Nothing for block 0 which has none), the next block's marker (`nextMarker`),
        -- and the target block's chars (reversed). Each separator marker opens the next
        -- block.
        step el acc =
            if RichText.isMarker el.content then
                let
                    nextBi =
                        acc.bi + 1
                in
                { acc
                    | bi = nextBi
                    , marker =
                        if nextBi == blockIndex then
                            Just el.id

                        else
                            acc.marker
                    , nextMarker =
                        if nextBi == blockIndex + 1 then
                            Just el.id

                        else
                            acc.nextMarker
                }

            else
                case richCharOf el of
                    Just ch ->
                        if acc.bi == blockIndex then
                            { acc | charsRev = ( el.id, ch ) :: acc.charsRev }

                        else
                            acc

                    Nothing ->
                        acc

        result =
            List.foldl step { bi = 0, marker = Nothing, nextMarker = Nothing, charsRev = [] } live
    in
    { chars = List.reverse result.charsRev
    , leftAnchor = result.marker
    , rightAnchor = result.nextMarker
    }


{-| Apply a formatting mark of kind `type_` (value `PBool True` for a boolean mark,
`PString _` for a value mark like a link) over the **visible character range**
`[from, to)` of a rich-text field. The range is resolved to character identities, so
the mark is stable under concurrent edits.
-}
mark : Path -> Int -> Int -> String -> Prim -> Doc a -> Result Error (Doc a)
mark path from to type_ value doc =
    markRange path from to type_ value doc


{-| Clear mark `type_` over the visible range `[from, to)` (an `AddMark` with value
`PNull`, which competes by LWW with any covering set-op).
-}
clearMark : Path -> Int -> Int -> String -> Doc a -> Result Error (Doc a)
clearMark path from to type_ doc =
    markRange path from to type_ PNull doc


markRange : Path -> Int -> Int -> String -> Prim -> Doc a -> Result Error (Doc a)
markRange path from to type_ value doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case node of
                    Node.Rich r ->
                        let
                            -- CHARACTER ids only. `from`/`to` are char offsets (block
                            -- markers and nest tokens are not characters — the editor's
                            -- offsets skip them), so the id array must skip them too, or
                            -- a mark on a later block lands one char early per preceding
                            -- marker.
                            ids =
                                liveChars richCharOf r.text
                                    |> List.map Tuple.first
                                    |> Array.fromList

                            -- start anchor: before the first char in range; end
                            -- anchor: after the last char in range. `Nothing` refs
                            -- fall to text start/end when the range hits an edge.
                            startAnchor =
                                case Array.get from ids of
                                    Just cid ->
                                        { ref = Just cid, side = Node.Before }

                                    Nothing ->
                                        { ref = Nothing, side = Node.Before }

                            endAnchor =
                                case Array.get (to - 1) ids of
                                    Just cid ->
                                        { ref = Just cid, side = Node.After }

                                    Nothing ->
                                        { ref = Nothing, side = Node.After }
                        in
                        if from >= to then
                            Ok doc

                        else
                            Ok (emitMark target type_ value startAnchor endAnchor doc)

                    _ ->
                        Err (WrongNodeType "expected rich-text node for mark")
            )


{-| Split at a **block-relative** caret: `blockIndex` (0 = the leading block) and
`charOffset` characters into that block. A block-boundary marker is inserted at that
point (Fugue-placed between the surrounding elements). The characters keep their ids,
so cursors and marks survive; the run after the split becomes a new block. This is
the position an editor naturally reports (which block the caret is in + offset within
it); the library resolves it to the underlying element position. See `design-docs/11`.

The new block **inherits the split block's type and depth** — pressing Enter in a list
item yields another list item at the same indent, an indented paragraph stays indented,
etc. (This copies an opaque string/count, so it defines no block vocabulary — an app
that wants a different rule, e.g. heading → paragraph, applies it on top with a
follow-up `setBlockType`.)

-}
splitBlock : Path -> Int -> Int -> Doc a -> Result Error (Doc a)
splitBlock path blockIndex charOffset doc =
    withRich path
        doc
        (\target r ->
            let
                ( parent, side ) =
                    blockSplitPlacement r.text blockIndex charOffset

                srcBlock =
                    RichText.toBlocks r |> List.drop blockIndex |> List.head

                srcType =
                    srcBlock |> Maybe.map .type_ |> Maybe.withDefault ""

                srcDepth =
                    srcBlock |> Maybe.map .depth |> Maybe.withDefault 0

                ( markerId, doc1 ) =
                    mint doc

                withMarker =
                    emitBlockElemWithId markerId target parent side Node.Marker doc1

                withType =
                    if srcType == "" then
                        withMarker

                    else
                        emitMark target
                            RichText.blockTypeMark
                            (PString srcType)
                            { ref = Just markerId, side = Node.Before }
                            { ref = Just markerId, side = Node.After }
                            withMarker
            in
            -- inherit depth: one nest token per level, right-children of the new marker
            -- (as repeated `indentBlock` would place them)
            List.range 1 srcDepth
                |> List.foldl
                    (\_ acc -> emitBlockElem target (Just markerId) Rga.Right Node.Nest acc)
                    withType
        )


{-| Merge block `blockIndex` into the previous one: tombstone that block's marker
(`DeleteElem`). The two runs coalesce; no characters move. No-op on block 0 (it has
no separator before it) or an out-of-range index.
-}
mergeBlock : Path -> Int -> Doc a -> Result Error (Doc a)
mergeBlock path blockIndex doc =
    withRich path
        doc
        (\target r ->
            -- only a *separator* marker can be merged; the leading marker (block 0)
            -- is not a separator, so merging block 0 is a no-op.
            case separatorMarkerAt r blockIndex of
                Just markerId ->
                    let
                        ( id, doc1 ) =
                            mint doc
                    in
                    commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = markerId }) ] doc1

                Nothing ->
                    doc
        )


{-| Set (or clear, with `Nothing`) the app-defined type of block `blockIndex`: an
`AddMark` of `RichText.blockTypeMark`. For a separator block (index ≥ 1) the mark
covers that block's marker element. **Block 0 has no marker element** — its type is a
`block` mark anchored to the document **head** (both endpoints `ref = Nothing`), read
back by `RichText.leadingTypeMark`. Either way the mark uses a freshly minted id and
converges by per-target LWW; there is no reserved element or constant id.
-}
setBlockType : Path -> Int -> Maybe String -> Doc a -> Result Error (Doc a)
setBlockType path blockIndex maybeType doc =
    withRich path
        doc
        (\target r ->
            let
                value =
                    case maybeType of
                        Just t ->
                            PString t

                        Nothing ->
                            PNull

                ( start, end ) =
                    if blockIndex <= 0 then
                        -- head-anchored sentinel range (covers no character): block 0
                        ( { ref = Nothing, side = Node.Before }
                        , { ref = Nothing, side = Node.Before }
                        )

                    else
                        case blockMarkerId r blockIndex of
                            Just markerId ->
                                ( { ref = Just markerId, side = Node.Before }
                                , { ref = Just markerId, side = Node.After }
                                )

                            Nothing ->
                                -- out of range: a no-op sentinel that covers nothing
                                ( { ref = Nothing, side = Node.After }
                                , { ref = Nothing, side = Node.After }
                                )
            in
            if blockIndex >= 1 && blockMarkerId r blockIndex == Nothing then
                doc

            else
                emitMark target RichText.blockTypeMark value start end doc
        )


{-| Indent block `blockIndex`: insert one nest token (a freshly minted element).
Accretive — concurrent indent/outdent commute. For block 0 the token is placed at the
document head (before the first char); for a separator block, right after its marker.
-}
indentBlock : Path -> Int -> Doc a -> Result Error (Doc a)
indentBlock path blockIndex doc =
    withRich path
        doc
        (\target r ->
            if blockIndex <= 0 then
                -- head-anchored nest token: a left-child of the head, so it precedes
                -- block 0's text (which lives in the head's right subtree). Counted as
                -- block-0 depth by `toBlocks` (it appears before any marker).
                emitBlockElem target Nothing Rga.Left Node.Nest doc

            else
                case blockMarkerId r blockIndex of
                    Just markerId ->
                        emitBlockElem target (Just markerId) Rga.Right Node.Nest doc

                    Nothing ->
                        doc
        )


{-| Outdent block `blockIndex`: tombstone its highest-`OpId` live nest token (no-op at
depth 0). Two concurrent outdents hit the same token → one net outdent. For block 0 the
tokens are the head nest tokens (before the first marker).
-}
outdentBlock : Path -> Int -> Doc a -> Result Error (Doc a)
outdentBlock path blockIndex doc =
    withRich path
        doc
        (\target r ->
            let
                tokenId =
                    if blockIndex <= 0 then
                        leadingNestToken r.text

                    else
                        blockMarkerId r blockIndex |> Maybe.andThen (highestNestToken r.text)
            in
            case tokenId of
                Just tid ->
                    let
                        ( id, doc1 ) =
                            mint doc
                    in
                    commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = tid }) ] doc1

                Nothing ->
                    doc
        )


{-| The marker `OpId` of block `blockIndex` from the read model (`Nothing` if the
index is out of range, or for block 0, which is marker-less by construction).
-}
blockMarkerId : Node.RichNode -> Int -> Maybe OpId
blockMarkerId r blockIndex =
    RichText.toBlocks r
        |> List.drop blockIndex
        |> List.head
        |> Maybe.andThen .marker


{-| The marker of block `blockIndex` only if it is a **separator** (not the leading
marker of block 0). Used by merge, which can't merge block 0 into a previous block.
-}
separatorMarkerAt : Node.RichNode -> Int -> Maybe OpId
separatorMarkerAt r blockIndex =
    if blockIndex <= 0 then
        Nothing

    else
        blockMarkerId r blockIndex


{-| The highest-`OpId` live nest token appearing **before the first marker** — i.e. a
block-0 nest token, placed at the document head by `indentBlock 0`. `Nothing` if block
0 has depth 0. (Outdent deletes the highest-`OpId` one, the merge-friendly rule shared
with `highestNestToken`.)
-}
leadingNestToken : Rga.Rga Node.RichElem -> Maybe OpId
leadingNestToken rga =
    let
        -- walk visible elements; collect nest tokens seen before the first marker.
        collect els acc =
            case els of
                [] ->
                    acc

                el :: rest ->
                    if RichText.isMarker el.content then
                        acc

                    else if RichText.isNestToken el.content then
                        collect rest (el.id :: acc)

                    else
                        collect rest acc
    in
    Rga.toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> (\els -> collect els [])
        |> List.sortWith (\x y -> Id.compareOpId y x)
        |> List.head


{-| Resolve a rich-text field and run `f target richNode`, or a `WrongNodeType` error.
-}
withRich : Path -> Doc a -> (List TargetStep -> Node.RichNode -> Doc a) -> Result Error (Doc a)
withRich path doc f =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case node of
                    Node.Rich r ->
                        Ok (f target r)

                    _ ->
                        Err (WrongNodeType "expected rich-text node")
            )


{-| Emit an `InsertElem` for a block-structure element (marker or nest token) at a
Fugue placement. Distinct from `emitInsert` only in that the caller supplies the
`parent`/`side` directly (block elements are placed by us, not appended after).
-}
emitBlockElem : List TargetStep -> Maybe OpId -> Rga.Side -> Node.BlockToken -> Doc a -> Doc a
emitBlockElem target parent side token doc =
    let
        ( elemId, doc1 ) =
            mint doc
    in
    emitBlockElemWithId elemId target parent side token doc1


{-| Like `emitBlockElem` but with the element id supplied by the caller, who minted it
first because it needs the id _before_ the insert: `splitBlock` mints its marker id up
front so it can anchor the block-type mark to that marker and hang the inherited nest
tokens off it in the same batch.
-}
emitBlockElemWithId : OpId -> List TargetStep -> Maybe OpId -> Rga.Side -> Node.BlockToken -> Doc a -> Doc a
emitBlockElemWithId elemId target parent side token doc =
    commit [ op elemId (frontierOf doc) (InsertToken { container = target, elemId = elemId, parent = parent, side = side, token = token }) ] doc


{-| The Fugue `(parent, side)` for a marker inserted at a **block-relative** caret:
`charOffset` characters into block `blockIndex` (blocks are marker-delimited; block 0
is the leading block). We walk the live visible elements tracking the current block
index and, within it, how many characters we've passed; the split element is the one
just after the target char (its Fugue left neighbor is that char). Placing the marker
there makes the run from the caret onward a new block.
-}
blockSplitPlacement : Rga.Rga Node.RichElem -> Int -> Int -> ( Maybe OpId, Rga.Side )
blockSplitPlacement rga blockIndex charOffset =
    let
        live =
            Rga.toElementsInOrder rga |> List.filter (not << .deleted)

        -- find the element id immediately to the LEFT of the split point, and the one
        -- to the RIGHT, by scanning while tracking (block, chars-into-block).
        scan els bi charsIn prevId =
            case els of
                [] ->
                    ( prevId, Nothing )

                el :: rest ->
                    if bi == blockIndex && charsIn == charOffset then
                        -- split point reached: prevId is left, el.id is right
                        ( prevId, Just el.id )

                    else if RichText.isMarker el.content then
                        -- a separator marker opens the next block
                        scan rest (bi + 1) 0 (Just el.id)

                    else if RichText.isNestToken el.content then
                        scan rest bi charsIn (Just el.id)

                    else
                        -- a character
                        scan rest bi (charsIn + 1) (Just el.id)

        ( left, right ) =
            scan live 0 0 Nothing
    in
    fuguePlacement rga left right


{-| The highest-`OpId` live nest token that belongs to the block opened by
`markerId`: the live nest-token elements appearing after that marker and before the
next marker, in element order. `Nothing` if the block has no tokens (depth 0).
-}
highestNestToken : Rga.Rga Node.RichElem -> OpId -> Maybe OpId
highestNestToken rga markerId =
    let
        markerKey =
            Id.opIdToString markerId

        -- walk visible elements; collect nest tokens that fall in this marker's block
        -- (after this marker, before the next marker).
        collect els inThisBlock acc =
            case els of
                [] ->
                    acc

                el :: rest ->
                    if RichText.isMarker el.content then
                        collect rest (Id.opIdToString el.id == markerKey) acc

                    else if inThisBlock && RichText.isNestToken el.content then
                        collect rest inThisBlock (el.id :: acc)

                    else
                        collect rest inThisBlock acc
    in
    Rga.toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> (\els -> collect els False [])
        |> List.sortWith (\x y -> Id.compareOpId y x)
        |> List.head


{-| Diff the whole character sequence of a **plain text** node to read as `value`.
-}
applyTextDiff : List TargetStep -> Rga.Rga String -> String -> Doc a -> Doc a
applyTextDiff target rga value doc =
    applyCharDiff target rga (liveChars (\el -> Just el.content) rga) Nothing Nothing value doc


{-| Diff the whole character sequence of a **rich text** node to read as `value`.

Same operation, different element vocabulary: a rich sequence also holds block markers and
nest tokens, which are not text and must not shift a character index (`design-docs/11`).
Dropping them is a `case` here rather than a `PString` guess, because `RichElem` says which
is which.

-}
applyRichTextDiff : List TargetStep -> Rga.Rga Node.RichElem -> String -> Doc a -> Doc a
applyRichTextDiff target rga value doc =
    applyCharDiff target rga (liveChars richCharOf rga) Nothing Nothing value doc


{-| The live `(id, char)` pairs of a sequence, in order, per `toChar`.
-}
liveChars : (Rga.Element c -> Maybe String) -> Rga.Rga c -> List ( OpId, String )
liveChars toChar rga =
    Rga.toElementsInOrder rga
        |> List.filter (not << .deleted)
        |> List.filterMap (\el -> toChar el |> Maybe.map (Tuple.pair el.id))


{-| The character a rich-text element holds, or `Nothing` for a structural token.
-}
richCharOf : Rga.Element Node.RichElem -> Maybe String
richCharOf el =
    case el.content of
        Node.TextChar ch ->
            Just ch

        Node.Token _ ->
            Nothing


{-| Diff a specific list of char elements (`charElems`, in order) to read as `value`,
emitting the minimal insert/delete run scoped to those chars. `fallbackLeft` /
`fallbackRight` bound the range when it has no char of its own on that side: for a
block edit they are the block's own marker (left) and the next block's marker (right),
so text typed into an _empty_ block lands strictly **between** the two markers instead
of floating past the next marker into the following block. `Nothing` on a side means
the document edge (head / end). `rga` is the full text sequence (for Fugue placement).
Used by both `applyTextDiff` (whole sequence, both fallbacks `Nothing`) and
`setBlockText` (one block's chars).
-}
applyCharDiff : List TargetStep -> Rga.Rga c -> List ( OpId, String ) -> Maybe OpId -> Maybe OpId -> String -> Doc a -> Doc a
applyCharDiff target rga charElems fallbackLeft fallbackRight value doc =
    let
        -- each char element holds exactly one char (they are minted per-char), so
        -- `ids` and `current` stay index-aligned one-to-one.
        ids =
            charElems |> List.map Tuple.first |> Array.fromList

        current =
            charElems |> List.filterMap (\( _, s ) -> String.uncons s |> Maybe.map Tuple.first)

        target_ =
            String.toList value

        prefix =
            commonPrefix current target_ 0

        maxSuffix =
            min (List.length current - prefix) (List.length target_ - prefix)

        suffix =
            commonSuffix (List.reverse current) (List.reverse target_) 0 maxSuffix

        -- visible indices [prefix .. len-suffix-1] of the CURRENT text are deleted
        deleteIds =
            List.range prefix (List.length current - suffix - 1)
                |> List.filterMap (\i -> Array.get i ids)

        -- characters to insert at the prefix boundary
        insertChars =
            target_
                |> List.drop prefix
                |> List.take (List.length target_ - prefix - suffix)

        -- Fugue anchoring for the FIRST inserted char. `leftAnchor` = the surviving
        -- visible element just before the gap (index prefix-1, which survives since
        -- deletions start at prefix); `rightAnchor` = the first surviving visible
        -- element after the gap (index len-suffix, first past the deleted range).
        leftAnchor =
            if prefix <= 0 then
                -- no char to the left within this range: anchor after the block's
                -- marker (fallbackLeft) so an insert lands inside the block, not at
                -- the document head. Nothing for the whole-sequence / leading case.
                fallbackLeft

            else
                Array.get (prefix - 1) ids

        rightAnchor =
            case Array.get (List.length current - suffix) ids of
                Just cid ->
                    Just cid

                Nothing ->
                    -- no surviving char after the gap within this range: bound the
                    -- insert by the next block's marker (fallbackRight) so it can't
                    -- float past it into the following block. Nothing = document end.
                    fallbackRight

        startPlacement =
            fuguePlacement rga leftAnchor rightAnchor

        -- The batch is CHAINED, not stamped with a shared frontier: the first op
        -- depends on the pre-edit frontier, and each subsequent op depends only on
        -- the previous op in the batch (`prevDep`). This keeps the causal frontier
        -- O(1) after the run — otherwise every char op is a fresh tip and the *next*
        -- edit's delta must enumerate the whole run as `deps` (measured: ~979 deps/op
        -- for a 5-char edit on a 1000-char doc, 99% of the delta). Chaining preserves
        -- ordering: each op still transitively follows all prior history.
        startDeps =
            frontierOf doc

        -- delete ops (chained; `prevDep` threads the last-minted op id forward)
        ( afterDeletes, deleteOps, depsAfterDeletes ) =
            List.foldl
                (\elemId ( d, acc, prevDep ) ->
                    let
                        ( id, d1 ) =
                            mint d
                    in
                    ( d1, op id prevDep (DeleteElem { container = target, elem = elemId }) :: acc, [ id ] )
                )
                ( doc, [], startDeps )
                deleteIds

        -- insert op. The whole inserted run is ONE `InsertText` op carrying the string,
        -- rather than one `InsertElem` per character. `applyOp` explodes it into the same
        -- per-char right-spine (char 0 at the Fugue placement, each subsequent char a
        -- right-child of the previous — a contiguous subtree that won't interleave with a
        -- concurrent run at the same gap). We reserve the whole counter span up front by
        -- minting `len` ids, so the derived char ids (start .. start+len-1) can never
        -- collide with a later local mint; only the first (`start`) is the op's own id.
        insertLen =
            List.length insertChars

        ( runStart, docAfterReserve ) =
            reserveIds insertLen afterDeletes

        ( parent0, side0 ) =
            startPlacement

        insertOps =
            if insertLen == 0 then
                []

            else
                [ op runStart
                    depsAfterDeletes
                    (InsertText
                        { container = target
                        , start = runStart
                        , text = String.fromList insertChars
                        , parent = parent0
                        , side = side0
                        }
                    )
                ]

        finalDoc =
            docAfterReserve
    in
    commit (deleteOps ++ insertOps) finalDoc


{-| Reserve `n` consecutive op ids by advancing the clock `n` steps, returning the
**first** id of the span (`start`) and the advanced doc. The run op uses `start` as its
own id and derives the rest (`start+1 .. start+n-1`) implicitly; reserving them here keeps
a later local mint from reusing a counter a run char already claimed. `n <= 0` mints one
throwaway id (harmless) — callers guard the empty-run case before emitting.
-}
reserveIds : Int -> Doc a -> ( OpId, Doc a )
reserveIds n doc =
    let
        ( start, doc1 ) =
            mint doc

        advanced =
            List.foldl (\_ d -> mint d |> Tuple.second) doc1 (List.range 2 n)
    in
    ( start, advanced )


{-| Choose a Fugue `(parent, side)` for a new element inserted between visible
neighbors `left` and `right` (either may be absent at the ends of the text).

The rule keeps the element strictly between L and R while making concurrent
insertions at the same gap attach to the **same** anchor (so they order as whole
blocks, not interleaved):

  - if `right` exists and descends from `left` (R sits in L's right subtree — the
    common case, since L and R are adjacent) → attach as a **left-child of R**;
  - else if `left` exists → attach as a **right-child of L** (e.g. appending at the
    end, where R is absent);
  - else if `right` exists → attach as a **left-child of R** (inserting at the head);
  - else (empty sequence) → a root (`parent = Nothing, side = Right`).

-}
fuguePlacement : Rga.Rga c -> Maybe OpId -> Maybe OpId -> ( Maybe OpId, Rga.Side )
fuguePlacement rga left right =
    case ( left, right ) of
        ( Just l, Just r ) ->
            if descendsFrom rga r l then
                ( Just r, Rga.Left )

            else
                ( Just l, Rga.Right )

        ( Just l, Nothing ) ->
            ( Just l, Rga.Right )

        ( Nothing, Just r ) ->
            ( Just r, Rga.Left )

        ( Nothing, Nothing ) ->
            ( Nothing, Rga.Right )


{-| Is element `node` a descendant of `ancestor` in the Fugue parent tree? Walks
parent pointers up from `node`; cycle-safe via a visited set (adversarial input).
-}
descendsFrom : Rga.Rga c -> OpId -> OpId -> Bool
descendsFrom rga node ancestor =
    let
        ancestorKey =
            Id.opIdToString ancestor

        climb : Maybe OpId -> Set.Set String -> Bool
        climb current visited =
            case current of
                Nothing ->
                    False

                Just id ->
                    let
                        key =
                            Id.opIdToString id
                    in
                    if key == ancestorKey then
                        True

                    else if Set.member key visited then
                        False

                    else
                        climb (Rga.get id rga |> Maybe.andThen .parent) (Set.insert key visited)
    in
    -- start from `node`'s parent (a node is not its own descendant)
    climb (Rga.get node rga |> Maybe.andThen .parent) Set.empty



-- LIST -----------------------------------------------------------------------


{-| Append a fresh subtree (built by a `Seed`) to the end of a list.

Fast path: if the previous edit was an append to this same list, we already know
the last element's id (`lastAppend`) and skip `appendAnchor`'s O(n) walk of the
element order — so a run of appends to one list is O(1) each instead of O(n²)
overall. Otherwise we compute it once and start the run.

-}
listAppend : Path -> Seed -> Doc a -> Result Error (Doc a)
listAppend path seed doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                if isOrdered node then
                    let
                        after =
                            case appendCacheFor target doc of
                                Just cachedLast ->
                                    Just cachedLast

                                Nothing ->
                                    appendAnchor node
                    in
                    Ok (emitAppend target after seed doc)

                else
                    Err (WrongNodeType "expected list node for listAppend")
            )


{-| Insert a new element at **visible index `i`** of a list (`i = 0` prepends; `i >=
length` is equivalent to `listAppend`). Works on plain `Seq`/`Txt` lists and on `Mov`
(movable) lists.

Placement is by the two visible neighbors — the element at `i-1` (left) and the one at `i`
(right). For `Seq`/`Txt` we hand those to `fuguePlacement`, the same rule text insertion
uses, so a concurrent insert at the same gap converges deterministically and doesn't
interleave. For `Mov`, cells are structural right-children, so we anchor after the home
cell of the element at `i-1` (the head when `i = 0`), exactly like `listMove`'s anchor.

-}
listInsert : Path -> Int -> Seed -> Doc a -> Result Error (Doc a)
listInsert path i seed doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case Node.asMov node of
                    Just ml ->
                        -- movable list: anchor after the home cell of the element before `i`.
                        -- Clamp `i` to `[0, length]` so an out-of-range index appends (its
                        -- predecessor is the last element) rather than falling through to a
                        -- head insert.
                        let
                            { order, homes } =
                                MoveList.resolveOrder ml

                            clamped =
                                clamp 0 (List.length order) i

                            after =
                                if clamped <= 0 then
                                    Nothing

                                else
                                    List.drop (clamped - 1) order
                                        |> List.head
                                        |> Maybe.andThen (\vid -> Dict.get (Id.opIdToString vid) homes)
                        in
                        Ok (emitElemAt target after Rga.Right seed doc)

                    Nothing ->
                        case Node.asSeq node of
                            Just rga ->
                                -- Fugue placement between the neighbors at `i-1` (left) and `i`
                                -- (right). Clamp `i` to `[0, length]`: at/past the end `right`
                                -- is absent and `left` is the last element (so it appends); at 0
                                -- `left` is absent (so it prepends).
                                let
                                    ids =
                                        orderedIds node |> Maybe.withDefault []

                                    clamped =
                                        clamp 0 (List.length ids) i

                                    left =
                                        if clamped <= 0 then
                                            Nothing

                                        else
                                            List.drop (clamped - 1) ids |> List.head

                                    right =
                                        List.drop clamped ids |> List.head

                                    ( parent, side ) =
                                        fuguePlacement rga left right
                                in
                                Ok (emitElemAt target parent side seed doc)

                            Nothing ->
                                Err (WrongNodeType "expected list node for listInsert")
            )


{-| Emit an `InsertElem` at an explicit Fugue `(parent, side)`, running `seed` for the new
element's content (the seed's stamp owns the fresh element id, keeping the clock
consistent). The append fast-path is invalidated (`commit` clears `lastAppend`), since this
is not necessarily a tail insert. Shared by `listInsert`.
-}
emitElemAt : List TargetStep -> Maybe OpId -> Rga.Side -> Seed -> Doc a -> Doc a
emitElemAt target parent side seed (Doc d) =
    let
        ( elemId, ctx1 ) =
            Id.nextId d.ctx

        ( seedNode, ctx2 ) =
            SchemaI.runSeed seed ctx1

        doc1 =
            Doc { d | ctx = ctx2 }
    in
    commit
        [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, parent = parent, side = side, seed = seedNode }) ]
        doc1


{-| Whether a node is an ordered, id-addressed sequence (`Seq`/`Txt`/`Mov`). O(1): a
constructor test — never materialize the element order just to answer this (that made
`listAppend` O(n) per append → O(n²) to build a list).
-}
isOrdered : Node -> Bool
isOrdered node =
    (Node.asSeq node /= Nothing)
        || (Node.asTxt node /= Nothing)
        || (Node.asMov node /= Nothing)


{-| The anchor to append after: the last visible element's id for `Seq`/`Txt`, or
the last value's **home cell** for `Mov` (inserts/moves anchor after a cell).
-}
appendAnchor : Node -> Maybe OpId
appendAnchor node =
    case Node.asMov node of
        Just ml ->
            lastElemId node |> Maybe.andThen (\vid -> MoveList.homeCell vid ml)

        Nothing ->
            lastElemId node


{-| The cached last-appended id for `target`, if the append fast-path is live for
exactly this list.
-}
appendCacheFor : List TargetStep -> Doc a -> Maybe OpId
appendCacheFor target (Doc d) =
    case d.lastAppend of
        Just ( cachedTarget, lastId ) ->
            if cachedTarget == target then
                Just lastId

            else
                Nothing

        Nothing ->
            Nothing


{-| Tombstone the element at a visible index in a list.
-}
listRemove : Path -> Int -> Doc a -> Result Error (Doc a)
listRemove path i doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                if isOrdered node then
                    case elemIdAt i node of
                        Just elemId ->
                            let
                                ( id, doc1 ) =
                                    mint doc
                            in
                            Ok (commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = elemId }) ] doc1)

                        Nothing ->
                            Err (PathNotFound ("list index " ++ String.fromInt i))

                else
                    Err (WrongNodeType "expected list node for listRemove")
            )


{-| Move the item at visible index `from` to sit at visible index `to`, on a
`movableList`. The item keeps its identity (nested edits and cursors follow it).
On a plain `list` (`Seq`) this fails — only `movableList` supports moves.
-}
listMove : Path -> Int -> Int -> Doc a -> Result Error (Doc a)
listMove path from to doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case Node.asMov node of
                    Just ml ->
                        let
                            -- Resolve the list order ONCE (a single Fugue walk +
                            -- home-cell pass) and answer every index/anchor lookup
                            -- below from these in-memory lists, rather than
                            -- re-walking the cell order per lookup.
                            { order, homes } =
                                MoveList.resolveOrder ml

                            idAt i =
                                List.drop i order |> List.head

                            -- The anchor to insert *after* the item at visible index
                            -- `i - 1` (Nothing at the head): its home cell id.
                            anchorAt i =
                                if i <= 0 then
                                    Nothing

                                else
                                    idAt (i - 1)
                                        |> Maybe.andThen
                                            (\vid -> Dict.get (Id.opIdToString vid) homes)
                        in
                        case idAt from of
                            Just valueId ->
                                let
                                    -- We anchor the moved item *after* the item that
                                    -- should precede it at the destination, computed
                                    -- against the list with the moved item removed.
                                    -- Moving DOWN (to > from): removing the item
                                    -- shifts later indices down by one, so the new
                                    -- predecessor is the item currently at `to`.
                                    -- Moving UP (to <= from): the predecessor is the
                                    -- item currently at `to - 1`.
                                    after =
                                        if to <= 0 then
                                            Nothing

                                        else if to > from then
                                            anchorAt (to + 1)

                                        else
                                            anchorAt to

                                    ( id, doc1 ) =
                                        mint doc
                                in
                                Ok (commit [ op id (frontierOf doc1) (MoveElem { container = target, elem = valueId, after = after }) ] doc1)

                            Nothing ->
                                Err (PathNotFound ("list index " ++ String.fromInt from))

                    Nothing ->
                        Err (WrongNodeType "expected movable list for listMove")
            )


{-| Add a new node to the tree at `path`, as the **last child** of `parent`
(`Nothing` = a new root), seeded from `seed`. The new node's id is minted here.
-}
treeAddChild : Path -> Maybe OpId -> Seed -> Doc a -> Result Error (Doc a)
treeAddChild path parent seed doc =
    treeContainer path doc
        |> Result.map
            (\( target, t ) ->
                let
                    ( childId, ctx1 ) =
                        Id.nextId (ctxOf doc)

                    ( moveOp, ctx2 ) =
                        Id.nextId ctx1

                    ( seedNode, ctx3 ) =
                        SchemaI.runSeed seed ctx2

                    -- fast path: a run of `addChild`s under the same (container, parent)
                    -- places each new child just after the previous one, so we can skip
                    -- the O(n) `resolve` in `endPos` and step off the cached frac.
                    prevFrac =
                        case treeChildCacheFor target parent doc of
                            Just cachedFrac ->
                                Just cachedFrac

                            Nothing ->
                                Tree.lastChildPos parent t

                    pos =
                        Crdt.Frac.between prevFrac Nothing

                    doc1 =
                        withCtx ctx3 doc

                    committed =
                        commit
                            [ op moveOp (frontierOf doc1) (TreeMove { container = target, child = childId, parent = parent, pos = pos, seed = Just seedNode }) ]
                            doc1
                in
                case committed of
                    Doc cd ->
                        Doc { cd | lastTreeChild = Just ( target, parent, pos ) }
            )


{-| The cached last-child frac for `addChild` if the fast-path is live for exactly this
(container, parent).
-}
treeChildCacheFor : List TargetStep -> Maybe OpId -> Doc a -> Maybe Crdt.Frac.Frac
treeChildCacheFor target parent (Doc d) =
    case d.lastTreeChild of
        Just ( cachedTarget, cachedParent, frac ) ->
            if cachedTarget == target && sameParent cachedParent parent then
                Just frac

            else
                Nothing

        Nothing ->
            Nothing


sameParent : Maybe OpId -> Maybe OpId -> Bool
sameParent a b =
    case ( a, b ) of
        ( Just x, Just y ) ->
            Id.opIdToString x == Id.opIdToString y

        ( Nothing, Nothing ) ->
            True

        _ ->
            False


{-| Re-parent `child` to be the **last child** of `parent` (`Nothing` = a root).
Cycle-forming moves are skipped at read (the node stays put), so this always
converges. No seed — the node keeps its content.
-}
treeMoveInto : Path -> OpId -> Maybe OpId -> Doc a -> Result Error (Doc a)
treeMoveInto path child parent doc =
    treeMoveTo path child parent (\t -> endPos parent t) doc


{-| Move `child` to sit immediately **before** `sibling` (same parent as sibling).
-}
treeMoveBefore : Path -> OpId -> OpId -> Doc a -> Result Error (Doc a)
treeMoveBefore path child sibling doc =
    treeMoveTo path child (currentParent path sibling doc) (\t -> beforePos sibling t) doc


{-| Move `child` to sit immediately **after** `sibling` (same parent as sibling).
-}
treeMoveAfter : Path -> OpId -> OpId -> Doc a -> Result Error (Doc a)
treeMoveAfter path child sibling doc =
    treeMoveTo path child (currentParent path sibling doc) (\t -> afterPos sibling t) doc


{-| Delete a tree node (and its subtree, at read) at `path`.
-}
treeRemove : Path -> OpId -> Doc a -> Result Error (Doc a)
treeRemove path child doc =
    treeContainer path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit [ op id (frontierOf doc1) (DeleteElem { container = target, elem = child }) ] doc1
            )


{-| Shared move emitter: resolve the tree container, compute the position with
`posOf`, emit a seedless `TreeMove`.
-}
treeMoveTo : Path -> OpId -> Maybe OpId -> (Tree.Tree Node -> Crdt.Frac.Frac) -> Doc a -> Result Error (Doc a)
treeMoveTo path child parent posOf doc =
    treeContainer path doc
        |> Result.map
            (\( target, t ) ->
                let
                    ( moveOp, doc1 ) =
                        mint doc
                in
                commit
                    [ op moveOp (frontierOf doc1) (TreeMove { container = target, child = child, parent = parent, pos = posOf t, seed = Nothing }) ]
                    doc1
            )


{-| Resolve a path to a tree node: its id-target plus the `Tree` value.
-}
treeContainer : Path -> Doc a -> Result Error ( List TargetStep, Tree.Tree Node )
treeContainer path doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                case Node.asTree node of
                    Just t ->
                        Ok ( target, t )

                    Nothing ->
                        Err (WrongNodeType "expected tree")
            )


{-| The current parent of `sibling` in the tree at `path` (for before/after moves).
-}
currentParent : Path -> OpId -> Doc a -> Maybe OpId
currentParent path sibling doc =
    treeContainer path doc
        |> Result.toMaybe
        |> Maybe.andThen (\( _, t ) -> Tree.parentOf sibling t)


{-| A fractional position after the last child of `parent` (append at end).
-}
endPos : Maybe OpId -> Tree.Tree Node -> Crdt.Frac.Frac
endPos parent t =
    -- position after the current last child; `lastChildPos` resolves the move-set once
    -- (the append hot path — `childIds` + `siblingPos` resolved it twice).
    Crdt.Frac.between (Tree.lastChildPos parent t) Nothing


{-| A position immediately before `sibling`.
-}
beforePos : OpId -> Tree.Tree Node -> Crdt.Frac.Frac
beforePos sibling t =
    let
        siblings =
            childIds (Tree.parentOf sibling t) t

        prev =
            precedingSibling sibling siblings
    in
    Crdt.Frac.between (prev |> Maybe.andThen (\p -> Tree.siblingPos p t)) (Tree.siblingPos sibling t)


{-| A position immediately after `sibling`.
-}
afterPos : OpId -> Tree.Tree Node -> Crdt.Frac.Frac
afterPos sibling t =
    let
        siblings =
            childIds (Tree.parentOf sibling t) t

        next =
            followingSibling sibling siblings
    in
    Crdt.Frac.between (Tree.siblingPos sibling t) (next |> Maybe.andThen (\n -> Tree.siblingPos n t))


childIds : Maybe OpId -> Tree.Tree Node -> List OpId
childIds parent t =
    case parent of
        Just p ->
            Tree.childrenOf p t

        Nothing ->
            Tree.roots t


precedingSibling : OpId -> List OpId -> Maybe OpId
precedingSibling target ids =
    List.foldl
        (\id ( prev, found ) ->
            if found then
                ( prev, found )

            else if Id.opIdToString id == Id.opIdToString target then
                ( prev, True )

            else
                ( Just id, False )
        )
        ( Nothing, False )
        ids
        |> Tuple.first


followingSibling : OpId -> List OpId -> Maybe OpId
followingSibling target ids =
    case ids of
        x :: rest ->
            if Id.opIdToString x == Id.opIdToString target then
                List.head rest

            else
                followingSibling target rest

        [] ->
            Nothing


{-| Emit an append op after `after`, then record `elemId` as the new last id for
`target` so the next append to this list is O(1). `commit` clears `lastAppend`
first (any edit invalidates it), so we re-establish it here afterwards.
-}
emitAppend : List TargetStep -> Maybe OpId -> Seed -> Doc a -> Doc a
emitAppend target after seed (Doc d) =
    let
        ( elemId, ctx1 ) =
            Id.nextId d.ctx

        ( seedNode, ctx2 ) =
            SchemaI.runSeed seed ctx1

        doc1 =
            Doc { d | ctx = ctx2 }

        committed =
            commit
                [ op elemId (frontierOf doc1) (InsertElem { container = target, elemId = elemId, parent = after, side = Rga.Right, seed = seedNode }) ]
                doc1
    in
    case committed of
        Doc cd ->
            Doc { cd | lastAppend = Just ( target, elemId ) }



-- DICT -----------------------------------------------------------------------


{-| Set (or overwrite) a dictionary key, marking it present.

**The value always arrives as ordinary ops through the value-diff path** (`seedNodeAt`),
never as a subtree carried by the presence op. A `SetKeyPresence`'s seed is only consulted when
it _creates_ the entry (see `setKeyPresenceAt`) — a second seed for a key that is already there
is silently dropped, which made `setKey` over an existing key a no-op and made a re-`setKey`
after a `removeKey` read back the old value. Diffing instead also merges properly: a dict
_of text_ overwrites by character diff, so a peer's concurrent edit to the same value
survives, and a record value only emits ops for the fields that differ.

That leaves the seed with exactly one job — bringing the entry into being — and it does it
with the value's **canonical skeleton** (`Node.vacate`): the right kind of node, holding
nothing. Two replicas that create the same absent key therefore emit the identical seed, so
which one a replica folds first no longer decides what the entry holds; the values race
afterwards as plain LWW/RGA ops, like every other edit. Carrying the real value here instead
is what made concurrent creation of one key non-commutative, and an incremental merge (which
folds only the ops it just added) cannot re-decide that later.

The cost is that a freshly created key is briefly _shaped but empty_ if a delivery is torn
between the creation and the ops that fill it — the same intermediate state any multi-op
edit has — and that text nested inside a created record value arrives per character rather
than as a run.

-}
setKey : Path -> String -> Seed -> Doc a -> Result Error (Doc a)
setKey path k seed doc =
    resolve path doc
        |> Result.andThen
            (\( target, node ) ->
                let
                    keyTarget =
                        target ++ [ IntoKey k ]

                    ( skeleton, ctx1 ) =
                        SchemaI.runSeed seed (ctxOf doc)
                            |> Tuple.mapFirst (\n -> Node.vacate n |> Maybe.withDefault n)
                in
                (case entryAt k node of
                    Nothing ->
                        emit (SetKeyPresence { target = keyTarget, present = True, seed = skeleton }) (withCtx ctx1 doc)

                    Just existing ->
                        -- The entry is there (possibly tombstoned): make it present if it
                        -- isn't. The seed rides along in case a peer merges this op without
                        -- the creation that preceded it, so every presence op that can
                        -- create the key creates the same thing.
                        if existing.present then
                            doc

                        else
                            emit (SetKeyPresence { target = keyTarget, present = True, seed = skeleton }) (withCtx ctx1 doc)
                )
                    |> seedNodeAt (Path.key k path) seed
            )


{-| Remove a dictionary key (LWW presence tombstone).

Removing a key this replica has never seen **emits nothing**. There would be no value to
tombstone, so the op could only create the entry, and the seed it would have to carry is a
placeholder of the wrong shape: a concurrent `setKey` of that key then finds the entry
already present, keeps the placeholder as the value, and the document stops reading
(`MissingField`/type mismatch) on every replica. "You cannot delete what you have not
observed" is also the ordinary semantics for a map CRDT — a delete races only against
writes it has seen, and a concurrent create of an unseen key wins by default.

The **apply** side holds the same rule (`OpLog.setKeyPresenceAt`): a removal that reaches a
replica ahead of the creation it followed does not create the entry either, it waits
(`canApply`) until the creation lands. So the seed below is never installed anywhere; it
stays `emptyMap` because the op's type demands a node, not because anything reads it.

-}
removeKey : Path -> String -> Doc a -> Result Error (Doc a)
removeKey path k doc =
    resolve path doc
        |> Result.map
            (\( target, node ) ->
                case entryAt k node of
                    Nothing ->
                        doc

                    Just _ ->
                        let
                            ( id, doc1 ) =
                                mint doc
                        in
                        commit
                            [ op id (frontierOf doc1) (SetKeyPresence { target = target ++ [ IntoKey k ], present = False, seed = emptyMap }) ]
                            doc1
            )


{-| The map entry stored under `k`, present or tombstoned.
-}
entryAt : String -> Node -> Maybe Node.Entry
entryAt k node =
    Node.asMap node |> Maybe.andThen (Dict.get k)


{-| Add a **contribution** to a user-defined op-set CRDT (`Crdt.opSet`): write the
seeded contribution node under a freshly-minted **op-id key**, so every contribution has a
unique identity and concurrent contributions from any replicas all survive a merge (they
union by distinct key). This is `setKey` with the key being the op's own id — the one new
primitive extensibility needs, reusing the existing presence op. Returns the contribution's
key (its op-id string) so a caller can later `retract` exactly it.
-}
contribute : Path -> Seed -> Doc a -> Result Error ( String, Doc a )
contribute path seed doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    (Doc d) =
                        doc

                    ( id, ctx1 ) =
                        Id.nextId d.ctx

                    key =
                        Id.opIdToString id

                    ( seedNode, ctx2 ) =
                        SchemaI.runSeed seed ctx1

                    doc1 =
                        Doc { d | ctx = ctx2 }
                in
                ( key
                , commit
                    [ op id (frontierOf doc1) (SetKeyPresence { target = target ++ [ IntoKey key ], present = True, seed = seedNode }) ]
                    doc1
                )
            )


{-| Tombstone a single contribution of an op-set (by its `key`, the op-id string
`contribute` returned) — an LWW presence flip, so a removed contribution no longer folds
into the read. Turns a grow-only op-set into a two-phase / removable one. A retract that
arrives _before_ its contribution still sticks: rather than pre-recording an absent entry
(which would then swallow the contribution's own seed), it is held as pending until the
contribution lands and applied to it (see `OpLog.setKeyPresenceAt`).
-}
retract : Path -> String -> Doc a -> Result Error (Doc a)
retract path key doc =
    resolve path doc
        |> Result.map
            (\( target, _ ) ->
                let
                    ( id, doc1 ) =
                        mint doc
                in
                commit
                    [ op id (frontierOf doc1) (SetKeyPresence { target = target ++ [ IntoKey key ], present = False, seed = emptyMap }) ]
                    doc1
            )



-- PATH RESOLUTION ------------------------------------------------------------


{-| Resolve a visible-index `Path` against the current materialized state into a
stable, id-based `Target` plus the node found there. This is the bridge from the
index-addressed public API to the identity-addressed op model — list indices
become element `OpId`s, so the emitted op is position-independent.
-}
resolve : Path -> Doc a -> Result Error ( List TargetStep, Node )
resolve path doc =
    walk (Path.segments path) (state doc) []



-- REF PRIMITIVES -------------------------------------------------------------
-- Node-free entry points that `Crdt.Edit` builds its typed `set`/`over` on.
-- They keep the `Node` type internal: callers pass a `Seed` (opaque) or a
-- sub-schema, never a `Node`.


{-| Overwrite whatever is at `path` so it reads as the value the `Seed` builds,
emitting the **minimal** ops to get there (so concurrent edits elsewhere survive).
The seed's node is compared against the current node with the same diff engine
`restoreTo` uses; a text target additionally gets a character-level diff so
collaborative text still merges by character. Used by `Crdt.Edit.set`.
-}
seedNodeAt : Path -> Seed -> Doc a -> Result Error (Doc a)
seedNodeAt path seed doc =
    case resolve path doc of
        Ok ( target, current ) ->
            let
                ( seeded, ctx1 ) =
                    SchemaI.runSeed seed (ctxOf doc)

                doc1 =
                    withCtx ctx1 doc
            in
            Ok <|
                case ( current, seeded ) of
                    ( Node.Txt rga, Node.Txt _ ) ->
                        -- preserve character-wise merge for text leaves
                        applyTextDiff target rga (textOfNode seeded) doc1

                    _ ->
                        -- general case: emit the diff ops (registers, counters,
                        -- maps/records, sum-type $tag switches, sequences)
                        restoreNode target seeded current doc1

        Err err ->
            -- The path didn't resolve. If it's a record/dict field that is simply ABSENT
            -- (an `optional`/`withDefault`/aliased field not seeded into the base — see
            -- design-docs/13), create it: resolve the PARENT map and emit a `SetKeyPresence`
            -- that adds the key. This is the same op dicts use for a new key; it's what
            -- lets `Crdt.Edit.set` write a field the base never seeded. The key exists
            -- once that lands, so the retry takes the branch above and writes the value.
            case seedAbsentField path seed doc of
                Just doc1 ->
                    seedNodeAt path seed doc1

                Nothing ->
                    Err err


{-| If `path` ends in a field/key whose parent map resolves but the key is absent, emit
a `SetKeyPresence` creating it (empty); else `Nothing` (the caller keeps the original resolve
error). Enables writing an unseeded (migration-tolerant) field.
-}
seedAbsentField : Path -> Seed -> Doc a -> Maybe (Doc a)
seedAbsentField path seed doc =
    case List.reverse (Path.segments path) of
        (Path.Field name) :: revParent ->
            createKeyAt (pathOfSegments (List.reverse revParent)) name seed doc

        (Path.Key name) :: revParent ->
            createKeyAt (pathOfSegments (List.reverse revParent)) name seed doc

        _ ->
            Nothing


{-| Rebuild a `Path` from a segment list (via the `Path` builders).
-}
pathOfSegments : List Path.Seg -> Path
pathOfSegments segs =
    List.foldl
        (\seg p ->
            case seg of
                Path.Field n ->
                    Path.field n p

                Path.Key n ->
                    Path.key n p

                Path.Index i ->
                    Path.index i p

                Path.NodeId id ->
                    Path.node id p
        )
        Path.root
        segs


{-| Resolve `parentPath` to a map and emit a `SetKeyPresence` adding `name`, then write `seed`
into it with ordinary ops — the same two steps (canonical skeleton, then value) `setKey`
takes, for the same reason. `Nothing` if the parent doesn't resolve to a map or the key
already exists (then the normal path should have handled it).
-}
createKeyAt : Path -> String -> Seed -> Doc a -> Maybe (Doc a)
createKeyAt parentPath name seed doc =
    case resolve parentPath doc of
        Ok ( parentTarget, parentNode ) ->
            case Node.asMap parentNode of
                Just entries ->
                    if Dict.member name entries then
                        -- key present (possibly tombstoned) — let the normal diff path
                        -- handle it rather than a blind create.
                        Nothing

                    else
                        let
                            ( skeleton, ctx1 ) =
                                SchemaI.runSeed seed (ctxOf doc)
                                    |> Tuple.mapFirst (\n -> Node.vacate n |> Maybe.withDefault n)
                        in
                        Just
                            (emit
                                (SetKeyPresence { target = parentTarget ++ [ IntoKey name ], present = True, seed = skeleton })
                                (withCtx ctx1 doc)
                            )

                Nothing ->
                    Nothing

        Err _ ->
            Nothing


{-| Read the typed value at `path` through a sub-schema. `Crdt.Edit.over` uses this to
fetch the current value, apply a function, and write it back with `seedNodeAt`. Keeps
`Node` internal (the sub-schema decodes it).
-}
subValue : Crdt kind sub -> Path -> Doc a -> Result Error sub
subValue schema path doc =
    resolve path doc
        |> Result.andThen
            (\( _, node ) ->
                SchemaI.decodeNode schema node
                    |> Result.mapError (\e -> WrongNodeType (SchemaI.errorToString e))
            )


{-| Read the typed value at `path` through a sub-schema **as of a past `Version`** — the
time-travel counterpart of `subValue`, mirroring `readAt` for a sub-ref. Walks `path`
against the state checked out at `v`, so a UI previewing history can read a single field's
past value. The live document is unchanged.
-}
subValueAt : Version -> Crdt kind sub -> Path -> Doc a -> Result Error sub
subValueAt v schema path doc =
    walk (Path.segments path) (stateAt v doc) []
        |> Result.andThen
            (\( _, node ) ->
                SchemaI.decodeNode schema node
                    |> Result.mapError (\e -> WrongNodeType (SchemaI.errorToString e))
            )


{-| Read a rich-text field as **blocks** (type + depth + spans), rather than the flat
`List Span` the `Crdt.richText` schema decodes to. A rich field supports both views;
block edits (`splitBlock` etc.) address markers by the `marker` id these carry. See
`Crdt.RichText.Internal.toBlocks` / `design-docs/11`.
-}
readBlocks : Path -> Doc a -> Result Error (List Block)
readBlocks path doc =
    blocksOfNode (state doc) path


{-| Read a rich-text ref as blocks **as of a past `Version`** — the time-travel counterpart
of `readBlocks`. Materializes the state at `v` and walks the path against it, so a UI
previewing history can feed the editor the blocks it had then. The live document is unchanged.
-}
readBlocksAt : Version -> Path -> Doc a -> Result Error (List Block)
readBlocksAt v path doc =
    blocksOfNode (stateAt v doc) path


{-| Walk `path` against an already-materialized `root` node and read it as rich-text blocks.
Shared by `readBlocks` (live cache) and `readBlocksAt` (a checked-out past state).
-}
blocksOfNode : Node -> Path -> Result Error (List Block)
blocksOfNode root path =
    walk (Path.segments path) root []
        |> Result.andThen
            (\( _, node ) ->
                case Node.asRich node of
                    Just r ->
                        Ok (RichText.toBlocks r)

                    Nothing ->
                        Err (WrongNodeType "expected rich-text node for readBlocks")
            )


{-| The visible string of a `Txt` node. `""` for anything else, which is the right reading of
"nothing to diff against" — and unreachable for the one caller, since `seedNodeAt` has just
matched the node as a `Txt`.
-}
textOfNode : Node -> String
textOfNode node =
    Node.asTxt node |> Maybe.map Text.read |> Maybe.withDefault ""


walk : List Seg -> Node -> List TargetStep -> Result Error ( List TargetStep, Node )
walk segs node acc =
    case segs of
        [] ->
            Ok ( List.reverse acc, node )

        seg :: rest ->
            case seg of
                Field name ->
                    intoKey name rest node acc

                Key name ->
                    intoKey name rest node acc

                Index i ->
                    case node of
                        Node.Seq rga ->
                            intoElem i rest rga acc

                        -- no `Txt`/`Rich`: an index into text addresses a character, and a
                        -- character has no interior for the rest of the path to reach.
                        -- Text is edited as a whole string (`setText`) or by block
                        -- (`setBlockText`), never by descending into one element.
                        Node.Mov ml ->
                            -- descend into the value at visible index `i` by its
                            -- valueId, so nested edits address it stably across moves
                            case elemIdAt i node of
                                Just valueId ->
                                    case MoveList.get valueId ml of
                                        Just content ->
                                            walk rest content (IntoElem valueId :: acc)

                                        Nothing ->
                                            Err (PathNotFound ("index " ++ String.fromInt i))

                                Nothing ->
                                    Err (PathNotFound ("index " ++ String.fromInt i))

                        _ ->
                            Err (WrongNodeType ("expected sequence at index " ++ String.fromInt i))

                NodeId nodeId ->
                    -- descend into a tree node by its stable id
                    case node of
                        Node.Tree t ->
                            case Tree.get nodeId t of
                                Just content ->
                                    walk rest content (IntoElem nodeId :: acc)

                                Nothing ->
                                    Err (PathNotFound ("tree node " ++ Id.opIdToString nodeId))

                        _ ->
                            Err (WrongNodeType "expected tree for node id")


intoKey : String -> List Seg -> Node -> List TargetStep -> Result Error ( List TargetStep, Node )
intoKey name rest node acc =
    case Node.asMap node of
        Just entries ->
            case Dict.get name entries of
                Just entry ->
                    walk rest entry.value (IntoKey name :: acc)

                Nothing ->
                    Err (PathNotFound ("key/field " ++ name))

        Nothing ->
            Err (WrongNodeType ("expected map at " ++ name))


intoElem : Int -> List Seg -> Rga.Rga Node -> List TargetStep -> Result Error ( List TargetStep, Node )
intoElem i rest rga acc =
    case Rga.idAtVisibleIndex i rga of
        Just elemId ->
            case Rga.get elemId rga of
                Just el ->
                    walk rest el.content (IntoElem elemId :: acc)

                Nothing ->
                    Err (PathNotFound ("index " ++ String.fromInt i))

        Nothing ->
            Err (PathNotFound ("index " ++ String.fromInt i))



-- HELPERS --------------------------------------------------------------------


op : OpId -> OpLog.Frontier -> Action -> Op
op id deps action =
    { id = id, deps = deps, action = action }


frontierOf : Doc a -> OpLog.Frontier
frontierOf (Doc d) =
    d.frontier


emptyMap : Node
emptyMap =
    Node.mapFromEntries Dict.empty


commonPrefix : List Char -> List Char -> Int -> Int
commonPrefix a b acc =
    case ( a, b ) of
        ( x :: xs, y :: ys ) ->
            if x == y then
                commonPrefix xs ys (acc + 1)

            else
                acc

        _ ->
            acc


commonSuffix : List Char -> List Char -> Int -> Int -> Int
commonSuffix ra rb acc cap =
    if acc >= cap then
        acc

    else
        case ( ra, rb ) of
            ( x :: xs, y :: ys ) ->
                if x == y then
                    commonSuffix xs ys (acc + 1) cap

                else
                    acc

            _ ->
                acc
