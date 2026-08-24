module Crdt.OpLog exposing
    ( Op, Action(..), Target, TargetStep(..), Frontier
    , OpStore, empty, insert, ops, size, member, merge
    , causalOrder, applyOps, applyOpsWithPending, actionTarget, opsMaxCounter, materialize, materializeWithPending, addedOpsInOrder, addedFromCandidates, checkout
    , frontier, frontierOfOps, advanceFrontier, frontierCovers, opsAfter, ancestorsOf, compact, ancestorKeys, stableFrontier
    )

{-| The operation log: the source of truth for an op-log-based document.

A document's state is _derived_ by folding the ops into a `Node`
(`materialize`), which the typed `Crdt.Schema` layer then reads. Conflict
resolution lives in that derivation, not in a structural `merge`: LWW-by-stamp as
registers fold, and Fugue sequence order computed at read from the element set.
Merging two documents is set-union of their op stores — trivially commutative,
associative and idempotent.

Each `Op` records its causal `deps` (the frontier it was created against), so the
store forms a DAG. `materialize` folds ops in a **causal order** (every op after
all its dependencies); the result is independent of which valid causal
linearization is chosen. Causal order matters: a `DeleteElem` applied before its
target's `InsertElem` would be a silent no-op, so only an order that honours
`deps` is correct.

The op-log core: real DAG + frontiers + causal materialize + checkout. `Crdt.Doc.Internal`
drives it — every document is materialized from an `OpStore`.

@docs Op, Action, Target, TargetStep, Frontier
@docs OpStore, empty, insert, ops, size, member, merge
@docs causalOrder, applyOps, applyOpsWithPending, actionTarget, opsMaxCounter, materialize, materializeWithPending, addedOpsInOrder, addedFromCandidates, checkout
@docs frontier, frontierOfOps, advanceFrontier, frontierCovers, opsAfter, ancestorsOf, compact, ancestorKeys, stableFrontier

-}

import Crdt.Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node(..), Prim)
import Crdt.Rga as Rga
import Crdt.Tree.Internal as Tree
import Dict exposing (Dict)
import Set exposing (Set)


{-| A point in the causal DAG: the set of op ids that are the current "tips"
(ops not yet depended upon). An op created against a frontier lists it as `deps`.

**Unordered** — semantically a set, and every operation over it here treats it as one.
It stays a `List` for two reasons. `Set OpId` is not expressible (`OpId` is an opaque
custom type, not `comparable`), so the alternative is a `Set String` of `opIdToString`
keys plus a table to get the ids back — which every `deps` read and the wire encoder
would have to undo. And it is **tiny**: one element in the common case (the last op the
replica saw), bounded above by the number of concurrently diverged branches, i.e. active
replicas. Nothing here scales with the frontier; the set work that scales with the
_store_ already runs over `Set String` (`frontierKeys`, `ancestorKeys`).

-}
type alias Frontier =
    List OpId


{-| A single operation: its unique id, the frontier it causally follows, and what
it does.
-}
type alias Op =
    { id : OpId
    , deps : Frontier
    , action : Action
    }


{-| What an op does. `Target` addresses state by stable identity, so every action
is position-independent.
-}
type Action
    = SetReg Target Prim
    | SetKeyPresence { target : Target, present : Bool, seed : Node }
    | InsertElem { container : Target, elemId : OpId, parent : Maybe OpId, side : Rga.Side, seed : Node }
    | InsertText { container : Target, start : OpId, text : String, parent : Maybe OpId, side : Rga.Side }
    | InsertToken { container : Target, elemId : OpId, parent : Maybe OpId, side : Rga.Side, token : Node.BlockToken }
    | DeleteElem { container : Target, elem : OpId }
    | MoveElem { container : Target, elem : OpId, after : Maybe OpId }
    | Increment { target : Target, delta : Int }
    | TreeMove { container : Target, child : OpId, parent : Maybe OpId, pos : Frac, seed : Maybe Node }
    | AddMark { container : Target, markId : OpId, type_ : String, value : Prim, start : Node.MarkAnchor, end : Node.MarkAnchor }


{-| A path into the document by stable identity: map keys by name, sequence/text
elements by their `OpId`.
-}
type alias Target =
    List TargetStep


{-| One navigation step of a `Target`.
-}
type TargetStep
    = IntoKey String
    | IntoElem OpId



-- STORE ----------------------------------------------------------------------


{-| A set of ops, keyed by id. The DAG; ops are immutable and ids unique, so a
shared id always maps to an identical op.
-}
type OpStore
    = OpStore (Dict String Op)


{-| The empty store.
-}
empty : OpStore
empty =
    OpStore Dict.empty


{-| Add an op (idempotent — re-adding an existing id is a no-op).
-}
insert : Op -> OpStore -> OpStore
insert op (OpStore d) =
    OpStore (Dict.insert (Id.opIdToString op.id) op d)


{-| All ops, unordered.
-}
ops : OpStore -> List Op
ops (OpStore d) =
    Dict.values d


{-| How many ops the store holds. `O(1)` — for a count, never build the op list (let
alone a `causalOrder`, which sorts a list only to measure it).
-}
size : OpStore -> Int
size (OpStore d) =
    Dict.size d


{-| Whether an op id is present.
-}
member : OpId -> OpStore -> Bool
member id (OpStore d) =
    Dict.member (Id.opIdToString id) d


{-| Merge two stores: set-union by id. Commutative, associative, idempotent.
-}
merge : OpStore -> OpStore -> OpStore
merge (OpStore a) (OpStore b) =
    OpStore (Dict.union a b)



-- FRONTIERS ------------------------------------------------------------------
-- Everything that computes with a `Frontier` lives here, so the one type's rules
-- (it is a set; membership goes through `Id.opIdToString`) are stated in one place.
-- A frontier is a fact about the op DAG, so this stays in the op-log module rather
-- than becoming its own: a separate module could hold only the id-keying helpers,
-- since `frontier`/`ancestorKeys`/`stableFrontier` all need `OpStore` and would have
-- to stay behind — splitting the concept in two rather than isolating it.


{-| The ids of a frontier as the `Set String` every membership test wants.
-}
frontierKeys : Frontier -> Set String
frontierKeys =
    List.foldl (\id acc -> Set.insert (Id.opIdToString id) acc) Set.empty


{-| Every id any of these ops depends on. The complement of this within a set of ops
is that set's frontier, which is what `frontier`/`frontierOfOps`/`advanceFrontier` all
compute — hence one helper.
-}
depKeys : List Op -> Set String
depKeys =
    List.foldl (\op acc -> List.foldl (\dep s -> Set.insert (Id.opIdToString dep) s) acc op.deps) Set.empty


tipsOf : Set String -> List OpId -> Frontier
tipsOf depended =
    List.filter (\id -> not (Set.member (Id.opIdToString id) depended))


{-| The current frontier: ops that no other op depends on (the DAG tips).
-}
frontier : OpStore -> Frontier
frontier (OpStore d) =
    let
        allOps =
            Dict.values d
    in
    tipsOf (depKeys allOps) (List.map .id allOps)


{-| The causal tips of a **set of ops** rather than a whole store: those no other op in
the set depends on. For a causal-order prefix this is the frontier that checks out
exactly the prefix, which is what linear-history scrubbing needs.
-}
frontierOfOps : List Op -> Frontier
frontierOfOps batch =
    tipsOf (depKeys batch) (List.map .id batch)


{-| Advance a known frontier by a batch of newly-added ops, **without** rescanning the
whole store: the new tips are the old ones plus the new ops' ids, minus everything any
of the new ops depends on. `O(batch + |frontier|)` instead of `frontier`'s `O(store)`.

Correct only when `newOps` are genuinely new (their ids aren't already tips) and their
`deps` reference existing ops — which holds for both local `commit` (deps = the current
frontier) and applying a delta. For a wholesale store rebuild use `frontier`.

-}
advanceFrontier : Frontier -> List Op -> Frontier
advanceFrontier current newOps =
    let
        newDepended =
            depKeys newOps
    in
    tipsOf newDepended current ++ tipsOf newDepended (List.map .id newOps)


{-| Whether `have` (e.g. a peer's frontier) already includes every op of `needed` (e.g.
our base frontier) — i.e. the peer is not behind our compaction boundary. Each needed id
must appear in `have`; since frontiers are causal tips and ids are minted once, set
membership is the right check.
-}
frontierCovers : Frontier -> Frontier -> Bool
frontierCovers have needed =
    let
        haveKeys =
            frontierKeys have
    in
    List.all (\id -> Set.member (Id.opIdToString id) haveKeys) needed



-- CAUSAL ORDER ---------------------------------------------------------------


{-| Linearize the store into a causal order: every op appears after all of its
`deps`. Among ops that are simultaneously ready, ties break by `OpId` so the
order is deterministic.

Implementation is a plain ascending sort by `OpId` (`O(n log n)`): Lamport ids
guarantee every dep has a strictly smaller id than the op depending on it, so
id order _is_ a causal order — see the comment in the body. A dep that points
outside the store (e.g. a delta that hasn't brought in an ancestor) therefore
costs nothing, and a partial store still materializes.

-}
causalOrder : OpStore -> List Op
causalOrder (OpStore d) =
    -- Sorting ascending by `OpId` (counter-major) IS a valid causal linearization, in
    -- O(n log n). An op's `deps` are the frontier it was minted against, so by Lamport's
    -- rule (`Id.nextId` takes a counter strictly greater than everything observed, which
    -- includes all deps) **every dep has a strictly smaller counter than the op** — hence a
    -- smaller `OpId`. So in ascending-id order every op appears after all its deps, exactly
    -- what a causal order requires; and among concurrent ops it ties by id, deterministically.
    --
    -- This equals the old topological sort's output for well-formed input: that sort emitted
    -- the lowest-id *ready* op each step, and by the invariant the lowest-id unemitted op is
    -- always ready (its deps, all smaller-id, are already emitted). Its own fallback for the
    -- "impossible under Lamport deps" cycle case was likewise "emit remaining by id". So the
    -- id sort is faithful in every case and drops the O(n²) per-step rescans.
    Dict.values d
        |> List.sortWith (\x y -> Id.compareOpId x.id y.id)



-- MATERIALIZE ----------------------------------------------------------------


{-| Fold ops over a base node **in the given order** (no sorting). Callers are
responsible for passing a causal order; `materialize` does that for you.
-}
applyOps : Node -> List Op -> Node
applyOps base orderedOps =
    List.foldl applyOp base orderedOps


{-| The largest Lamport counter referenced by a batch of ops: each op's own id, plus
any stamps buried in an insert/presence/tree seed, a text run's derived char ids, or a
mark's range anchors (a seed register can carry a counter _higher_ than the op's own id
— it was minted by a later edit on the source replica).

This is the O(batch) clock catch-up for ingesting a delta of untrusted wire ops, where
no incoming `Ctx` is available — replacing an O(whole-tree) `Node.maxCounter` scan of the
materialized result. (Merging two in-memory `Doc`s needs neither: each already keeps its
`Ctx` past its own stamps, so `max` of the two counters suffices.)

-}
opsMaxCounter : List Op -> Int
opsMaxCounter batch =
    List.foldl (\op acc -> max acc (opMaxCounter op)) 0 batch


{-| Enumerated exhaustively rather than with a `_ ->` fallback. The four actions that
contribute nothing are the ones that carry **no seed** and name only ids the author had
already observed — so by Lamport's rule the op's own id (taken below) already dominates
them. That reasoning is invisible in a wildcard, and a wildcard also swallows the next
action added: a seed-carrying constructor would silently start reporting `0` and let the
clock re-mint an id already in use. Spelling the cases out makes that a compile error.
-}
opMaxCounter : Op -> Int
opMaxCounter op =
    let
        seedMax =
            case op.action of
                InsertElem { seed } ->
                    Node.maxCounter seed

                InsertText { start, text } ->
                    -- a run consumes `len` consecutive counters (start .. start+len-1),
                    -- each a char element id; the clock must clear the whole span.
                    Id.opIdCounter start + max 0 (String.length text - 1)

                InsertToken _ ->
                    -- the element id IS the op id (taken below); a token carries no stamp
                    0

                SetKeyPresence { seed } ->
                    Node.maxCounter seed

                TreeMove { seed } ->
                    seed |> Maybe.map Node.maxCounter |> Maybe.withDefault 0

                AddMark { start, end } ->
                    let
                        anchorMax a =
                            a.ref |> Maybe.map Id.opIdCounter |> Maybe.withDefault 0
                    in
                    max (anchorMax start) (anchorMax end)

                -- seedless, and every id they name was observed before `op.id` was minted
                SetReg _ _ ->
                    0

                DeleteElem _ ->
                    0

                MoveElem _ ->
                    0

                Increment _ ->
                    0
    in
    max (Id.opIdCounter op.id) seedMax


{-| Materialize the whole store onto a base node (the schema's empty structure),
folding in causal order — holding back and retrying any op whose subject hasn't arrived
(see `applyOpsWithPending`).
-}
materialize : Node -> OpStore -> Node
materialize base store =
    materializeWithPending base store |> Tuple.first


{-| `materialize`, also reporting the ops it had to hold back. `[]` for any causally
closed store — which is every store this library produces on its own.
-}
materializeWithPending : Node -> OpStore -> ( Node, List Op )
materializeWithPending base store =
    applyOpsWithPending base (causalOrder store)


{-| Fold a batch like `applyOps`, but **hold back an op whose subject isn't there yet**,
retry it once the rest of the batch has landed, and return whatever is still unsatisfied.

Almost every action is a pure function of the op _set_: registers resolve LWW by stamp,
sequence order is derived from the element set at read, counters/marks/tree-moves are keyed
and resolved at read. Order is already irrelevant to those, and this changes nothing for
them. But four mutators edit **in place** and silently evaporate when their subject is
absent — `Rga.delete`, `Rga.updateElement`, `MoveList.updateValue`, `Tree.updateValue` — so
an op naming an element whose insert hasn't been folded is not delayed by them, it is
_lost_: the delete never happens (deleted content reappears, permanently), the nested edit
into a list item or tree node vanishes for good.

A causal order rules that out — an op's `deps` include the insert it names, and Lamport ids
put every dep first — so this never fires on a store the library assembled itself. It is
the safety net for the stores it can be _handed_: a filtering intermediary answering delta
queries out of an op table, a torn or truncated persisted log, a damaged payload. `canApply`
decides what would land; anything that wouldn't is deferred and retried, looping while a
pass makes progress. What is still unsatisfied comes back for the caller to hold and
re-offer when more ops arrive (`Crdt.Doc.Internal` keeps it on the document).

Iterating to a fixpoint is sound because **satisfaction is monotone**: elements, keys,
values and tree payloads are only ever added — a delete tombstones in place, it never
removes — so an op that can apply now could not have failed later, and the fixpoint depends
only on the op set, not on arrival order. Retrying is sound because applying an op twice is
a no-op: keyed dicts overwrite with the same value, equal stamps lose the LWW comparison,
and `Rga.put` keeps an existing tombstone (which is exactly why it must).

-}
applyOpsWithPending : Node -> List Op -> ( Node, List Op )
applyOpsWithPending base batch =
    let
        pass : List Op -> Node -> ( Node, List Op )
        pass remaining node =
            let
                ( node1, deferred ) =
                    List.foldl
                        (\op ( acc, held ) ->
                            if canApply acc op then
                                ( applyOp op acc, held )

                            else
                                ( acc, op :: held )
                        )
                        ( node, [] )
                        remaining

                stillPending =
                    List.reverse deferred
            in
            if List.isEmpty stillPending || List.length stillPending == List.length remaining then
                -- nothing held back, or a whole pass unblocked nothing: fixpoint.
                ( node1, stillPending )

            else
                pass stillPending node1
    in
    pass batch base


{-| The ops present in `merged` but **not** in `prior`, in `merged`'s causal order.

This is what lets a merge apply **incrementally**: a document whose cache already
reflects `prior` can fold just these added ops onto that cache instead of
re-materializing the whole store from base — the result is identical because every
`Action` is a commutative, idempotent function of the op _set_ with resolution deferred
to read (LWW-by-stamp, RGA/Fugue order, accretive counters/marks, tree/move re-fold).

They are causally ordered among **themselves only** — we sort a store of just the added
ops, not the whole merged store. `causalOrder` treats a dep absent from the store it is
sorting as already satisfied; the deps we drop this way are exactly the `prior` ops that
are already folded into the cache, so the order is valid **relative to that cache**.
Sorting only the added ops keeps a steady-state merge `O(k log k)` in the small delta
`k` rather than `O(n log n)` in the whole document — the difference between "merge cost
scales with the delta" and "with the doc".

-}
addedOpsInOrder : OpStore -> OpStore -> List Op
addedOpsInOrder prior (OpStore merged) =
    let
        added =
            Dict.foldl
                (\key op acc ->
                    if member op.id prior then
                        acc

                    else
                        Dict.insert key op acc
                )
                Dict.empty
                merged
    in
    causalOrder (OpStore added)


{-| The genuinely-new ops among `candidates` (those not already in `prior`), in causal
order. Like `addedOpsInOrder` but scanning only the **candidate list** rather than the
whole merged store — so ingesting a delta of `k` ops into an `n`-op store is `O(k)` to
find the new ops instead of `O(n)`. Use when the incoming ops are already in hand (the
`decodeInto`/delta path); `addedOpsInOrder` is for when only the merged store is known
(the `merge` path, which unions two full stores).

Candidates are deduped by id first (idempotent re-delivery of the same op is harmless),
then causally ordered among themselves — valid relative to the cache for the same reason
`addedOpsInOrder` is: the deps we drop are exactly the `prior` ops already folded in.

-}
addedFromCandidates : OpStore -> List Op -> List Op
addedFromCandidates prior candidates =
    candidates
        |> List.foldl
            (\op acc ->
                if member op.id prior then
                    acc

                else
                    Dict.insert (Id.opIdToString op.id) op acc
            )
            Dict.empty
        |> OpStore
        |> causalOrder


{-| Materialize the state as of a `Frontier`: only ops that are causal ancestors
of (or equal to) the frontier are folded. This is time-travel / checkout.
-}
checkout : Frontier -> Node -> OpStore -> Node
checkout target base store =
    materialize base (ancestorsOf target store)


{-| The set of op-id keys that are causal ancestors of (or equal to) the frontier
ops. Walks `deps` transitively.
-}
ancestorKeys : Frontier -> OpStore -> Set String
ancestorKeys target store =
    let
        collect : List OpId -> Set String -> Set String
        collect queue seen =
            case queue of
                [] ->
                    seen

                id :: rest ->
                    let
                        key =
                            Id.opIdToString id
                    in
                    if Set.member key seen then
                        collect rest seen

                    else
                        case lookup id store of
                            Just op ->
                                collect (op.deps ++ rest) (Set.insert key seen)

                            Nothing ->
                                collect rest seen
    in
    collect target Set.empty


{-| The sub-store of all causal ancestors of the frontier ops, plus the frontier
ops themselves.
-}
ancestorsOf : Frontier -> OpStore -> OpStore
ancestorsOf target store =
    let
        keep =
            ancestorKeys target store
    in
    ops store
        |> List.filter (\op -> Set.member (Id.opIdToString op.id) keep)
        |> List.foldl insert empty


{-| The ops a peer at `known` (their frontier) has **not** yet seen — every op
that is not a causal ancestor of `known`. This is the delta to send them.

Correct regardless of delivery order or counter gaps: it is defined purely by the
causal DAG, not by comparing Lamport counters (which a version-vector would do,
and which is unsafe under our gappy `Id.observe` clock).

-}
opsAfter : Frontier -> OpStore -> List Op
opsAfter known store =
    let
        seen =
            ancestorKeys known store
    in
    ops store
        |> List.filter (\op -> not (Set.member (Id.opIdToString op.id) seen))


{-| The **stable frontier** across a set of peer frontiers: the causal cut that _every_
peer has delivered past. It is the frontier (tips) of the **intersection** of all the
peers' ancestor sets — the ops that every listed peer holds — computed against this store.

Compacting below it is safe across exactly those peers: any op missing from even one peer
is absent from the intersection (so it survives), and concurrent work a peer hasn't shipped
yet is nobody's ancestor (so it is never in the intersection). A peer **not** in the list
(disconnected/behind) is not protected — it must be caught up by a snapshot transfer on
reconnect, which the wire layer already does when a peer is behind `baseFrontier`.

An empty list yields the empty frontier (nothing is common → compact nothing).

-}
stableFrontier : List Frontier -> OpStore -> Frontier
stableFrontier peers ((OpStore d) as store) =
    case peers of
        [] ->
            []

        first :: rest ->
            let
                -- ops every peer has: intersect the ancestor-key sets.
                common =
                    List.foldl
                        (\f acc -> Set.intersect acc (ancestorKeys f store))
                        (ancestorKeys first store)
                        rest

                -- keep only ops in the common set, then take that sub-DAG's frontier.
                commonStore =
                    OpStore (Dict.filter (\k _ -> Set.member k common) d)
            in
            frontier commonStore


{-| Compact the store at a causal `cut`: fold every op at-or-below `cut` into
`base` (producing a new base that already incorporates them), and return that new
base together with the store of the ops **not** below `cut`.

The defining guarantee (the read model is unchanged):

    materialize base store
        ==  let ( base', store' ) = compact base cut store
            in materialize base' store'

for **any** `cut`. This holds because folding is associative over a causal order
and `cut` is a causal cut: the ≤cut ops fold into `base'` exactly as they would
during a full materialize, and the remaining ops — whose `deps` may now point
into `base'` — still linearize correctly (`causalOrder` already treats deps
absent from the store as satisfied, and `base'` contains their effect).

`compact` is a pure, equivalence-preserving rewrite of `(base, store)`. It is
**not** a soundness decision: whether a given `cut` is safe to _discard_ across
merges is the caller's policy (see `design-docs/04-gc.md`) — `compact` itself never
loses information that `materialize` would have used.

An op below the cut that `materialize` would have **held back** (`applyOpsWithPending`)
is therefore kept in the returned store instead of being folded into the base: folding it
would discard it as a no-op, and it may yet become applicable when the op it waits on
arrives.

-}
compact : Node -> Frontier -> OpStore -> ( Node, OpStore )
compact base cut store =
    let
        belowCut =
            ancestorKeys cut store

        ( below, above ) =
            ops store
                |> List.partition (\op -> Set.member (Id.opIdToString op.id) belowCut)

        ( base1, stillPending ) =
            applyOpsWithPending base (causalOrder (List.foldl insert empty below))

        store1 =
            List.foldl insert empty (above ++ stillPending)
    in
    ( base1, store1 )


lookup : OpId -> OpStore -> Maybe Op
lookup id (OpStore d) =
    Dict.get (Id.opIdToString id) d



-- APPLY ONE OP ---------------------------------------------------------------


applyOp : Op -> Node -> Node
applyOp { id, action } root =
    case action of
        SetReg target prim ->
            updateAt target (setRegLww id prim) root

        SetKeyPresence { target, present, seed } ->
            setKeyPresenceAt target id present seed root

        InsertElem { container, elemId, parent, side, seed } ->
            updateAt container (insertElem elemId parent side seed) root

        InsertText { container, start, text, parent, side } ->
            updateAt container (insertTextRun start text parent side) root

        InsertToken { container, elemId, parent, side, token } ->
            updateAt container (insertToken elemId parent side token) root

        DeleteElem { container, elem } ->
            updateAt container (deleteElem elem) root

        MoveElem { container, elem, after } ->
            updateAt container (moveElem id elem after) root

        Increment { target, delta } ->
            updateAt target (incrementBy id delta) root

        TreeMove { container, child, parent, pos, seed } ->
            updateAt container (treeMove id child parent pos seed) root

        AddMark { container, markId, type_, value, start, end } ->
            updateAt container (addMark markId type_ value start end) root


{-| The container/target an action addresses — where in the document it changes something.
-}
actionTarget : Action -> Target
actionTarget action =
    case action of
        SetReg target _ ->
            target

        SetKeyPresence { target } ->
            target

        InsertElem { container } ->
            container

        InsertText { container } ->
            container

        InsertToken { container } ->
            container

        DeleteElem { container } ->
            container

        MoveElem { container } ->
            container

        Increment { target } ->
            target

        TreeMove { container } ->
            container

        AddMark { container } ->
            container


{-| Whether `op` would actually **land** on `node` — `False` when it names something that
has not arrived, so applying it now would be a silent no-op that a later op could make
meaningful. `applyOpsWithPending` defers those; this is the whole of its judgement.

Only actions whose present behaviour on an absent subject already _is_ a no-op are gated.
The rest — `SetReg`, a **creating** `SetKeyPresence`, `Increment`, `InsertElem`, `InsertText`,
`TreeMove` — **construct** the container they address when it is missing; that is how a
fresh dict key or a nested structure comes into being, so gating them would turn working
behaviour into a permanently pending op. What is gated:

  - every `IntoElem` step of the target must resolve, mirroring `updateAt`'s descent
    exactly. An `IntoKey` never blocks — `updateAt` creates a missing key — but the empty
    map it would create holds no elements, so an `IntoElem` beneath one correctly fails.
  - `DeleteElem` must find its element: in a sequence that means the element itself, since
    `Rga.delete` tombstones in place. A movable list or tree instead records the deleted id
    in a **grow-only** set that a later insert consults, so there only the container has to
    exist.
  - `MoveElem` needs a movable list and `AddMark` a rich-text node — the two actions that
    no-op on every other kind of node.
  - a **removing** `SetKeyPresence` must find its key. It is the one action that is gated on
    something a sibling op _creates_ rather than something that merely has to have arrived:
    `setKeyPresenceAt` deliberately will not let a removal create the entry it tombstones (that
    would make it order-dependent against a concurrent creation), so a removal delivered
    ahead of the creation it followed has to wait for it. The wait is what keeps it from
    being lost, and monotonicity still holds — a key is only ever added.

-}
canApply : Node -> Op -> Bool
canApply node op =
    let
        target =
            actionTarget op.action

        container =
            if gatedOnContainer op.action || List.any isIntoElem target then
                walkTarget target node

            else
                -- nothing to check, so skip the walk: this is the hot path, taken by
                -- every register/text/insert op in a well-formed batch.
                Just node
    in
    case container of
        Nothing ->
            False

        Just c ->
            case op.action of
                DeleteElem { elem } ->
                    deleteLands elem c

                MoveElem _ ->
                    isMovableList c

                AddMark _ ->
                    isRichText c

                InsertToken _ ->
                    -- a block token only means anything in a rich sequence, and
                    -- `insertToken`'s fallback is a silent no-op, so hold the op until its
                    -- container shows up rather than dropping it (same rule as `AddMark`).
                    isRichText c

                SetKeyPresence presence ->
                    presence.present || keyIsThere presence.target node

                _ ->
                    True


{-| The actions whose own fallback on the wrong (or absent) kind of container is a silent
no-op, so the container itself has to be checked. Deliberately excludes every action that
_creates_ its container — see `canApply`.
-}
gatedOnContainer : Action -> Bool
gatedOnContainer action =
    case action of
        DeleteElem _ ->
            True

        MoveElem _ ->
            True

        AddMark _ ->
            True

        InsertToken _ ->
            True

        _ ->
            False


isIntoElem : TargetStep -> Bool
isIntoElem step =
    case step of
        IntoElem _ ->
            True

        IntoKey _ ->
            False


{-| Follow a target through `node` as `updateAt` would, yielding the container the op would
be applied to — `Nothing` when an `IntoElem` names an element that has not arrived. A
missing map key yields the empty map `updateAt` would create there, so the walk continues
and only a deeper `IntoElem` fails.
-}
walkTarget : Target -> Node -> Maybe Node
walkTarget steps node =
    case steps of
        [] ->
            Just node

        (IntoKey k) :: rest ->
            walkTarget rest (childAtKey k node)

        (IntoElem x) :: rest ->
            childAtElem x node |> Maybe.andThen (walkTarget rest)


{-| Whether the map key a presence op names is already there (present or tombstoned) —
the gate on a removal, which `setKeyPresenceAt` will not let create its own entry.

Unlike `walkTarget`, a missing key on the way down is an answer (`False`), not a synthesized
empty map: if the parent isn't there yet, the key certainly isn't. A target that doesn't end
in a key isn't a shape this action takes, so it is left ungated.

-}
keyIsThere : Target -> Node -> Bool
keyIsThere steps node =
    case steps of
        [ IntoKey k ] ->
            case node of
                Map entries ->
                    Dict.member k entries

                _ ->
                    False

        (IntoKey k) :: rest ->
            keyIsThere rest (childAtKey k node)

        (IntoElem x) :: rest ->
            childAtElem x node |> Maybe.map (keyIsThere rest) |> Maybe.withDefault False

        [] ->
            True


childAtKey : String -> Node -> Node
childAtKey k node =
    case node of
        Map entries ->
            Dict.get k entries |> Maybe.map .value |> Maybe.withDefault emptyMap

        _ ->
            emptyMap


{-| The child `updateAt`'s `IntoElem` branch would descend into, if it is there. Mirrors
that branch case for case — including its omissions, so an op addressing something
`updateAt` cannot descend into stays pending rather than silently evaporating.
-}
childAtElem : OpId -> Node -> Maybe Node
childAtElem x node =
    case node of
        Seq rga ->
            Rga.get x rga |> Maybe.map .content

        Mov ml ->
            Dict.get (Id.opIdToString x) (MoveList.values ml)

        Tree t ->
            Dict.get (Id.opIdToString x) (Tree.payloads t)

        _ ->
            Nothing


{-| Whether a `DeleteElem` would record anything. Sequences tombstone the element in place,
so it has to be present; movable lists and trees keep a grow-only set of deleted ids that
a later insert is filtered through, so the container alone suffices.
-}
deleteLands : OpId -> Node -> Bool
deleteLands elem container =
    case container of
        Seq rga ->
            Rga.get elem rga /= Nothing

        Txt rga ->
            Rga.get elem rga /= Nothing

        Rich r ->
            Rga.get elem r.text /= Nothing

        Mov _ ->
            True

        Tree _ ->
            True

        _ ->
            False


isMovableList : Node -> Bool
isMovableList node =
    case node of
        Mov _ ->
            True

        _ ->
            False


isRichText : Node -> Bool
isRichText node =
    case node of
        Rich _ ->
            True

        _ ->
            False


{-| Apply `f` at `steps`, creating any map entry on the way that isn't there yet.
Sequence/text elements are descended into by id.

A key created this way is stamped `Id.unwrittenStamp` — **not** the id of the op that
happened to create it. The stamp is the key's presence LWW value, and no op here said
anything about presence: the key exists only because something was written _inside_ it. If
the creating op's id went in instead, the stamp would depend on which op reached the key
first, and an incremental fold (arrival order) and a full `materialize` (causal order) reach
it in different orders — so the same op set would produce two different documents. Losing to
every real presence op is also the behaviour we want: whichever `SetKeyPresence` arrives, before
or after, decides presence, and a removal is never silently outranked by a write that merely
passed through.

-}
updateAt : List TargetStep -> (Node -> Node) -> Node -> Node
updateAt steps f node =
    case steps of
        [] ->
            f node

        (IntoKey k) :: rest ->
            let
                entries =
                    case node of
                        Map d ->
                            d

                        _ ->
                            Dict.empty

                existing =
                    Dict.get k entries

                childNode =
                    case existing of
                        Just e ->
                            e.value

                        Nothing ->
                            emptyMap

                newEntry =
                    case existing of
                        Just e ->
                            { e | value = updateAt rest f childNode }

                        Nothing ->
                            Node.entry Id.unwrittenStamp True (updateAt rest f childNode)
            in
            Node.mapFromEntries (Dict.insert k newEntry entries)

        (IntoElem x) :: rest ->
            case node of
                Seq rga ->
                    Seq (Rga.updateElement x (updateAt rest f) rga)

                -- no `Txt`/`Rich` case: a text element is a character, so there is nothing
                -- inside it for a target to address. It used to be a whole `Node`, and this
                -- descent existed only because the type allowed it.
                Mov ml ->
                    -- descend into the value `x` (by valueId) and edit its content
                    Mov (MoveList.updateValue x (updateAt rest f) ml)

                Tree t ->
                    -- descend into tree node `x` (by nodeId) and edit its payload
                    Tree (Tree.updateValue x (updateAt rest f) t)

                _ ->
                    node


{-| Set whether a **map key exists** (LWW by stamp), creating the key if absent. This is
how dict set/remove is expressed as an op. A newly-created key is seeded with the
value sub-schema's empty structure (carried by the op), not a bare map — the same
seeding rule `InsertElem` follows.

"Presence" here is the name of the `Node.Entry` field this writes: a map entry's
existence is itself an LWW boolean with its own stamp, which is what makes key removal
converge against a concurrent write to the same key. It has **nothing to do with
`Crdt.Presence`**, the ephemeral who-is-online side channel — that never reaches the op
log at all. The `Key` in the name is there to keep the two apart.

**Only a `present = True` op creates the key.** A removal that finds no key leaves the map
alone: it has nothing to tombstone, and the seed it carries is not the one a creation would
install, so letting it create the entry would make the pair order-dependent (remove-then-create
would keep the removal's placeholder as the value, and the create's own seed would be dropped
as a second seed for a key that now exists). `canApply` defers such a removal until the key
shows up, so an out-of-order delivery lands it rather than losing it — and the seed a
creation _does_ install is the value's canonical skeleton (`Node.vacate`), identical on every
replica, so which of two concurrent creations wins the race no longer changes the result.
Together those two make this a commutative function of the op set, which is what
`addedOpsInOrder`'s incremental fold assumes of every action.

-}
setKeyPresenceAt : List TargetStep -> OpId -> Bool -> Node -> Node -> Node
setKeyPresenceAt steps stamp present seed node =
    case steps of
        [] ->
            node

        [ IntoKey k ] ->
            let
                entries =
                    case node of
                        Map d ->
                            d

                        _ ->
                            Dict.empty
            in
            case Dict.get k entries of
                Just e ->
                    if Id.compareOpId stamp e.stamp == GT then
                        Node.mapFromEntries (Dict.insert k { e | present = present, stamp = stamp } entries)

                    else
                        Node.mapFromEntries entries

                Nothing ->
                    if present then
                        Node.mapFromEntries (Dict.insert k (Node.entry stamp present seed) entries)

                    else
                        -- nothing to tombstone; `canApply` holds this op until there is
                        Node.mapFromEntries entries

        step :: rest ->
            updateAt [ step ] (\child -> setKeyPresenceAt rest stamp present seed child) node


emptyMap : Node
emptyMap =
    Node.mapFromEntries Dict.empty


setRegLww : OpId -> Prim -> Node -> Node
setRegLww stamp prim current =
    case current of
        Reg r ->
            if Id.compareOpId stamp r.stamp == GT then
                Node.reg prim stamp

            else
                current

        _ ->
            Node.reg prim stamp


{-| Insert an element whose content is a whole document. That is only the
**document-shaped** containers: a `Seq` element and a `Mov` value hold a `Node`, while a
`Txt` element holds a character and a `Rich` element a `RichElem`, so neither can take this
op's seed (`design-docs/16-typed-sequence-content.md`). Text is inserted by `InsertText` (a
run of characters) and block structure by `InsertToken`; the type split is what makes those
separate actions rather than one loosely-typed one.
-}
insertElem : OpId -> Maybe OpId -> Rga.Side -> Node -> Node -> Node
insertElem elemId parent side seed current =
    case current of
        Mov ml ->
            -- movable list: elemId is the valueId, `parent` the cell to follow. A
            -- MoveList cell is always a right-child (structural), so `side` is
            -- irrelevant here — MoveList manages its own cell ordering.
            Mov (MoveList.insert elemId parent seed ml)

        Seq rga ->
            Seq (Rga.put (Rga.element elemId parent side seed False) rga)

        Txt _ ->
            -- The wrong op for this container: the seed is a document and a text element is
            -- a character. Leave it ALONE — falling through to the create case below would
            -- replace the whole text field with a `Seq`, destroying it. Text is inserted by
            -- `InsertText`.
            current

        Rich _ ->
            current

        _ ->
            -- create the sequence: the container is a slot that no op has shaped yet (an
            -- implicitly created map key), which is why this action is not gated on its
            -- container in `canApply`.
            Seq (Rga.put (Rga.element elemId parent side seed False) Rga.empty)


{-| Insert a block-structure token — a boundary marker or one unit of indent — into a rich
text sequence. The only non-character element a rich sequence can hold, and meaningless in
any other container (`design-docs/11-block-structure.md`).
-}
insertToken : OpId -> Maybe OpId -> Rga.Side -> Node.BlockToken -> Node -> Node
insertToken elemId parent side token current =
    case current of
        Rich r ->
            Rich { r | text = Rga.put (Rga.element elemId parent side (Node.Token token) False) r.text }

        _ ->
            current


{-| **Explode a run-length text op** into the same per-character right-spine the store
used to hold one op at a time. Char 0 anchors at `(parent, side)` with id `start`; char
`i` (i≥1) has the derived id `(start.counter + i, start.replica)` and anchors as a
**right-child of char i-1** — byte-for-byte the chain `applyCharDiff` emits.

The derivation is what makes this safe with no split logic: a concurrent insert that
anchors "after char i of this run" references exactly `start + i`, an id this explosion
already materialises, so the two runs order by the ordinary Fugue rule and `==`
convergence is preserved. Each char is a single-char `PString` register stamped with its
own id (same as the old per-char seed), so the materialised `Node` is identical to the
pre-change one.

-}
insertTextRun : OpId -> String -> Maybe OpId -> Rga.Side -> Node -> Node
insertTextRun start text parent side current =
    let
        replica =
            Id.opIdReplica start

        base =
            Id.opIdCounter start

        -- fold the chars left→right into `rga0`, threading the previous char's id as the
        -- next char's parent; the first char uses the op's own (parent, side). `toContent`
        -- says what a character *is* in the target container — the one thing that differs
        -- between the three, now that they no longer share a content type.
        putChars toContent rga0 =
            String.toList text
                |> List.foldl
                    (\ch ( i, prevId, rga ) ->
                        let
                            elemId =
                                if i == 0 then
                                    start

                                else
                                    Id.opId (base + i) replica

                            ( p, s ) =
                                case prevId of
                                    Just prev ->
                                        ( Just prev, Rga.Right )

                                    Nothing ->
                                        ( parent, side )
                        in
                        ( i + 1
                        , Just elemId
                        , Rga.put (Rga.element elemId p s (toContent elemId (String.fromChar ch)) False) rga
                        )
                    )
                    ( 0, Nothing, rga0 )
                |> (\( _, _, rga ) -> rga)
    in
    case current of
        Txt rga ->
            Txt (putChars (\_ ch -> ch) rga)

        Seq rga ->
            -- a plain sequence of one-character registers. Not something the library emits
            -- (text edits target a `Txt`/`Rich`), but honoured rather than dropped: a `Seq`
            -- can hold a character, it just wraps it like any other value.
            Seq (putChars (\elemId ch -> Node.reg (Node.PString ch) elemId) rga)

        Rich r ->
            Rich { r | text = putChars (\_ ch -> Node.TextChar ch) r.text }

        _ ->
            Txt (putChars (\_ ch -> ch) Rga.empty)


deleteElem : OpId -> Node -> Node
deleteElem elem current =
    case current of
        Seq rga ->
            Seq (Rga.delete elem rga)

        Txt rga ->
            Txt (Rga.delete elem rga)

        Mov ml ->
            Mov (MoveList.delete elem ml)

        Tree t ->
            Tree (Tree.delete elem t)

        Rich r ->
            Rich { r | text = Rga.delete elem r.text }

        _ ->
            current


{-| Place `child` under `parent` at `pos` in a tree. On creation the op carries a
`seed` (the new node's content); a re-parent/reorder carries `Nothing` and keeps
the existing payload. `moveOp` (this op's id) keys the move, so the latest move
wins by causal-order fold. No-op on non-tree nodes.
-}
treeMove : OpId -> OpId -> Maybe OpId -> Frac -> Maybe Node -> Node -> Node
treeMove moveOp child parent pos seed current =
    let
        t =
            case current of
                Tree existing ->
                    existing

                _ ->
                    Tree.empty
    in
    case seed of
        Just content ->
            Tree (Tree.move moveOp child parent pos content t)

        Nothing ->
            Tree (Tree.moveOnly moveOp child parent pos t)


{-| Record a mark op in a rich-text node's append-only mark set, keyed by the op's
id (idempotent — re-applying the same op is a no-op). No-op on non-rich nodes.
-}
addMark : OpId -> String -> Prim -> Node.MarkAnchor -> Node.MarkAnchor -> Node -> Node
addMark markId type_ value start end current =
    case current of
        Rich r ->
            let
                markOp =
                    { id = markId, type_ = type_, value = value, start = start, end = end }
            in
            Rich { r | marks = Dict.insert (Id.opIdToString markId) markOp r.marks }

        _ ->
            current


{-| Move value `elem` after cell `after` in a movable list (no-op on other nodes).
The move op's id (`moveOp`) becomes the new cell id, so it wins by max-id LWW.
-}
moveElem : OpId -> OpId -> Maybe OpId -> Node -> Node
moveElem moveOp elem after current =
    case current of
        Mov ml ->
            Mov (MoveList.move moveOp elem after ml)

        _ ->
            current


{-| Add a signed contribution (keyed by this op's id) to a counter node, creating
the counter if the target was empty. Idempotent: replaying the op re-inserts the
same keyed contribution.
-}
incrementBy : OpId -> Int -> Node -> Node
incrementBy stamp delta current =
    let
        contributions =
            case current of
                Cnt d ->
                    d

                _ ->
                    Dict.empty
    in
    Cnt (Dict.insert (Id.opIdToString stamp) (Node.increment stamp delta) contributions)
