# 15 — Pending ops: surviving a delivery that isn't causally closed

**Status:** ✅ built — `OpLog.applyOpsWithPending` / `materializeWithPending`, a `pendingOps`
list on the `Doc`, and a tombstone-preserving `Rga.insertElement`. Plus the follow-up below:
a creation seed is now the value's canonical skeleton (`Node.vacate`), a removal is gated, and
an implicitly created map key is stamped `Id.unwrittenStamp`.
`tests/PendingOpsTests.elm` (7) pins the first, `tests/DictTests.elm` + the
`tests/DeliveryOrderTests.elm` properties the second; full suite 472/0.
**Fixes:** the "a `DeleteElem` folded before its target's `InsertElem` is a silent no-op"
caveat recorded in [`02-oplog.md`](02-oplog.md).

## The hole

`causalOrder` is a plain ascending `compareOpId` sort. That is sound *because* Lamport ids
make every dep strictly smaller than the op that names it — so a peer's own store, which is
always causally closed, folds in an order where every op's subject already exists. Partial
stores (deps pointing at ops we don't hold) were deliberately tolerated: the fold does not
refuse to run, it just folds what it has.

What it folds them *into* is the problem. `updateAt`'s `IntoKey` branch **creates** a
missing entry, but its `IntoElem` branch descends through four in-place mutators —

  - `Rga.updateElement` (nested edit into a list/text element),
  - `MoveList.updateValue` (nested edit into a movable-list item),
  - `Tree.updateValue` (nested edit into a tree node's payload),
  - `Rga.delete` itself,

— each a `Dict.update ... (Maybe.map f)`, which **evaporates** when the subject is absent.
So an op whose target names an element we don't hold doesn't fail; it does nothing, and it
never gets a second chance, because the next fold folds the same op into the same absence.

Two symptoms, one loud and one quiet:

  - **An orphan delete** silently didn't happen — and once the withheld insert arrived, the
    deleted content came back **and stayed**, on every replica downstream.
  - **An orphan nested edit** silently didn't happen and was **lost for good** — the
    user-visible shape of this is "my change didn't save."

A peer cannot produce this. Its *infrastructure* can, and that is the realistic trigger:

  - a relay or intermediary answering delta queries out of an op table and filtering
    (by author, by tenant, by subscription) — it ships the edit but not the insert;
  - a persisted log truncated or torn at the tail, then merged with a newer delta;
  - a payload that lost part of itself (a damaged store, a partial decode).

None of these are exotic once the op log is a thing other systems store and forward, and
the failure mode is silent data loss, which is the worst kind to leave latent.

## The fix: hold back, then retry

`applyOpsWithPending base batch` folds the batch, and instead of applying an op whose
subject is absent, **holds it**. When a pass held something back but also applied
something, it runs another pass over just the held ops — an applied op may have been the
insert that unblocks one. It stops at the fixpoint: nothing held, or a whole pass unblocked
nothing.

```elm
applyOpsWithPending : Node -> List Op -> ( Node, List Op )
```

`materialize` delegates to `materializeWithPending` and drops the second component, so the
settling happens in **every** path, not only on ingest. That matters: `Doc` maintains
`cached == materialize base store` incrementally, checked by `Doc.cacheConsistent`. If only
the incremental path settled, live state and `checkout`/history-scrubbing would disagree
about the same document.

On the `Doc` side, held ops are kept in a `pendingOps` list and prepended to the next
ingest's batch (`Doc.Internal.ingest`), so a later delivery retries them. They are also
excluded from the diff for that merge — a held op hasn't happened yet, so it must not be
reported as a change — and included when it lands.

`pendingOps` is **derived**, not independent state: a full `rebuild` recomputes it from
`(base, store)` and discards the old one, and `fork`/`forkAt` let the branch derive its own.
That is only legitimate because of:

## Why retry converges: satisfaction is monotone

Elements, map keys, movable-list values and tree payloads are only ever **added**; a delete
tombstones in place rather than removing. So the set of ops that *can* apply only grows: an
op that is applicable now cannot become inapplicable later. Therefore

  - the fixpoint is a function of the **op set**, not of arrival order — which is the same
    property the rest of the op log rests on, now extended to the ops that used to lack it;
  - a retry is safe, because re-applying an op that already applied has to be a no-op.

The second point is where `Rga.insertElement` came in. It was a plain `Dict.insert`, so
re-applying an `InsertElem` whose element had since been deleted would **resurrect** it.
It now ORs the tombstone (`deleted = el.deleted || old.deleted`), so `deleted` never goes
`True → False` through the one write door every element goes through. (Bonus: a corrupt or
hostile op can no longer un-delete a position that other elements anchor to. It still can't
reveal the victim's content — the element is rebuilt from the op's own seed.)

`compact` needed one change: it folds ops below the cut into `base`, and folding an op that
couldn't apply would discard it permanently, so still-pending ops are **retained in the
store** instead.

## Where the line is drawn

Satisfaction is tested against the **materialized `Node`**, not against `OpStore.member` of
the dep. Testing the store would leave every op that references a pre-compaction element
permanently pending — after compaction the insert is gone from the log but its element is
right there in `base`.

The gating rule, stated so it's decidable case by case:

> Gate exactly the actions whose present behaviour on an unsatisfied target already **is** a
> silent no-op. Never gate one that **constructs** its container.

  - **Gated:** `DeleteElem`, `MoveElem`, `AddMark`, plus any action whose target contains an
    `IntoElem` step (that's the nested-edit case; the step itself is what can fail). Plus,
    since the follow-up below, a **removing** `SetKeyPresence`.
  - **Not gated:** `SetReg`, a **creating** `SetKeyPresence`, `Increment`, `InsertElem`,
    `InsertText`, `TreeMove` — these create what they need, so gating them would convert
    *working* behaviour into permanently pending.

`canApply` mirrors `updateAt` case for case, **including its omissions** (`childAtElem` has
no `Rich` arm because `updateAt`'s `IntoElem` branch has none): an op addressing something
the fold cannot descend into stays pending rather than evaporating.

## Cost

`canApply` short-circuits: if the action isn't gated and the target has no `IntoElem`, it
skips the target walk entirely and answers `True`. That is the hot path — every register,
text and insert op in a well-formed batch. A gated op pays one target walk (`Dict.get` per
step) plus one `Rga.get`/`Dict.get` for its subject, so a full re-fold of a delete-heavy
document adds one lookup per delete: linear, small constant.

The **local** commit path is not gated at all. A local op always resolves (it's built
against current state), and it can never unblock a *remote* pending op, because ids are
per-replica. So local editing pays nothing.

## Follow-up: the one slot two ops can create

The item this doc first recorded as "adjacent, deliberately not fixed" —
`setKeyPresenceAt`'s `Just` branch keeps `e.value`, so a creation seed is dropped if something
else made the key first — turned out to be the same class of defect as the one above, not a
cosmetic edge. `tests/DeliveryOrderTests.elm` found it by fuzzing arbitrary partitions of a
history.

**A map key is the only container slot two ops can create.** An element, movable-list value
or tree node is named by the id of the op that inserted it, so no two ops ever make the same
one. Two replicas that both `setKey "k"` on an absent key are both creating *that* key. Only
the first `SetKeyPresence` to be folded installed its seed, so when the seed carried the value,
the result depended on **arrival order** — and arrival order genuinely differs between the
two paths that must agree: `rebuildIncremental` folds a delta in the order it arrives,
`materialize` folds the store in causal order. One op set, two documents, which is exactly
what `cacheConsistent` exists to catch.

Three changes, and the value never rides in a seed again:

1. **A creation carries the value's canonical *skeleton*, not its value** — `Node.vacate`:
   the same kind of node holding nothing, stamped `Id.unwrittenStamp` (counter `0` under the
   empty replica id, so every real write outranks it). It is derived from the seeded node's
   constructor alone, so it needs no schema access and is byte-identical on every replica —
   whichever creation wins installs the same thing. The value follows as ordinary ops through
   the value-diff path (`seedNodeAt`), which is what `setKey` already did for a key that
   existed. `Mov`, `Tree` and `Rich` answer `Nothing` and are still shipped whole: the diff
   has no arm that rebuilds rich text, and refilling a movable list or tree from empty
   re-mints every id, so emptying them would trade a rare convergence edge for certain data
   loss.
2. **A removal never creates the entry it tombstones.** Letting it would reintroduce the
   order-dependence from the other side (remove-then-create would keep the removal's
   placeholder as the value). So a removing `SetKeyPresence` is *gated* — the one action gated on
   something a sibling op **creates** rather than on something that merely has to have
   arrived. Monotonicity still holds, because keys are only ever added.
3. **A key created implicitly, by a write passing *through* it, is stamped
   `Id.unwrittenStamp`** rather than with that op's id (`updateAt`'s `IntoKey` branch). The
   stamp *is* the key's presence LWW value, and such an op said nothing about presence — the
   key exists only because something was written inside it. With the op's own id in there, the
   entry's stamp depended on which op reached the key first, so an incremental fold and a full
   materialize of the same ops disagreed on it. Losing to every real presence op is also the
   behaviour wanted: whichever `SetKeyPresence` arrives, before or after, decides presence.

Cost: a freshly created key is briefly *shaped but empty* if a delivery is torn between the
creation and the ops that fill it (it reads as its type's zero, not as a failed read), and
text nested inside a created record value arrives per character rather than as a run.

Still not fixed, both pre-existing and independent:

  - **`insertElem`'s `_ ->` fallback creates a `Seq`** even where a `Mov`/`Txt`/`Rich`
    belonged.
  - **`updateAt`'s `IntoElem` branch has no `Rich` case**, so nested edits into a rich-text
    element can't be addressed at all (`canApply` mirrors the omission on purpose, above).

## Testing

`tests/PendingOpsTests.elm` builds a real history (`A`, then `B`, then edit `B`, then delete
`B`), takes per-edit deltas with `encodeSince`, and delivers them **in the wrong order** —
the exact shape a filtering relay produces. Covered: orphan delete lands once its insert
arrives; orphan nested edit lands (fixed list *and* movable list); all four permutations of
the three deltas read alike; re-delivering the withheld insert does not resurrect the
deleted element; compaction keeps a still-pending op; forking recomputes what is pending.
Every test asserts `Doc.cacheConsistent` throughout, including *while* ops are held back.

All 7 fail (with concrete diffs) when `canApply` is neutered to `always True`, so they pin
the fix rather than merely passing alongside it.

`tests/DeliveryOrderTests.elm` fuzzes the general claim over the whole schema — for **any**
partition of a document's ops into deltas and **any** delivery order, the receiver reads
exactly what the sender reads, with a consistent cache and nothing left pending — including a
partition re-cut into single-op deltas, which can split an edit that emitted several ops. That
is what found the follow-up above; `tests/DictTests.elm` pins its four cases directly
(concurrent create of a text key and of a record key, the value's ops arriving ahead of the
creation, and a removal arriving ahead of it).
