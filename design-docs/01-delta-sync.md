# 01 — Delta sync + version vectors

> ⚠️ **Superseded by [`02-oplog.md`](02-oplog.md).** This was the *state-based*
> delta plan. Once we decided collaborative time-travel is a first-class
> requirement, the source of truth moved to an op-log, where delta sync is simply
> "ship a range of the log." The version-vector design below survives as the
> op-log's delta-sync step; the "stamp deletions" schema change is moot because
> deletions become first-class `DeleteElem` ops. Kept for context and rationale.

**Status:** superseded
**Roadmap item:** #1 (reframed — see `02-oplog.md`)
**Prerequisite for:** GC / compaction (#6), efficient large documents

## Problem

Today sync is full-state. `Crdt.encode : Doc -> Value` serializes the entire
`Node` tree (every register, every map entry, every live element *and* tombstone),
and the demo ships that whole blob on every keystroke. Two consequences:

- **Bandwidth grows with document size, not edit size.** A one-character edit to
  a large board re-sends the whole board.
- **It never shrinks.** Tombstones are retained forever (correctly — that is what
  keeps state-based merge idempotent), so the blob only grows.

We want: given what a peer already has, send only what it's missing.

## Core idea

Every piece of replicated state in `elm-crdt` is already stamped with the `OpId`
(`(counter, replicaId)`) that last wrote it — *except deletions* (see the schema
change below). A **version vector** `Dict ReplicaId Int` records, per replica, the
highest counter a document has observed. Then:

> A delta from A→B = every stamped fact in A whose `OpId.counter` is **greater
> than** B's version-vector entry for that fact's `replicaId`.

Because our `OpId` counters are per-replica and monotonic (minted by
`Id.nextId`, advanced past remotes by `Id.observe`), "counter > vv[replica]"
cleanly means "B has not seen this write." The receiver merges the delta exactly
as it merges a full state today — the delta is just a *partial* `Node` — so all
the existing convergence guarantees carry over unchanged.

This keeps us **state-based**, not op-based: we are not building an op log. We are
filtering the existing state tree by version. That is a much smaller change than
moving to Loro's OpLog model, and it composes with everything already built.

## The one load-bearing schema change: stamp deletions

This is the crux. Most state is already stamped:

- `Register` → `stamp : OpId` ✅
- `Map.Entry` → `stamp : OpId` (presence cell) ✅
- RGA `Element` → `id : OpId` for insertion ✅
- RGA `Element` deletion → **`deleted : Bool`, no stamp** ❌

A version-vector delta cannot tell whether a tombstone is *new* to the receiver,
because the deletion carries no version. So we must version deletions:

```elm
-- src/Crdt/Rga.elm — Element
type alias Element c =
    { id : OpId
    , origin : Maybe OpId
    , content : c
    , deletedAt : Maybe OpId   -- was: deleted : Bool
    }
```

`deletedAt = Nothing` means live; `Just stamp` means tombstoned at that stamp.
Merge of two elements takes the deletion with the larger `OpId` (or, since delete
is monotonic, simply `Nothing`-loses-to-`Just`, larger-`Just` wins). This:

- makes the deletion a first-class, versioned fact a delta can select;
- preserves idempotence (re-merging a tombstone is a no-op);
- is the **same** metadata GC (#6) needs to know a tombstone is causally stable.

`Crdt.delete` mints a stamp from the `Ctx` (it currently takes no `Ctx` — it
will need one, a small signature change through `Crdt.Edit`).

## Public API additions

```elm
-- Crdt
type alias Version            -- opaque; internally Dict ReplicaId Int

version      : Doc -> Version
encodeSince  : Version -> Doc -> Value      -- delta: facts newer than Version
encodeVersion : Version -> Value            -- so a peer can advertise what it has
decodeVersion : Value -> Result String Version

-- decode already exists and already merges; a decoded delta merges the same way.
-- merge : Doc -> Doc -> Doc   (unchanged — a delta Doc is just a sparse Doc)
```

Sync protocol (demo, two-message):

```
B → A :  encodeVersion (version docB)          -- "here's what I have"
A → B :  encodeSince (decodeVersion …) docA    -- "here's what you're missing"
B      :  merge docB (decode delta)
```

Full-state `encode` stays as-is for first contact / persistence (it is just
`encodeSince emptyVersion`).

## Implementation sketch

New module `Crdt/Version.elm`:

```elm
type alias Version = Dict String Int           -- replicaId string -> max counter

empty   : Version
observe : OpId -> Version -> Version            -- bump the entry to max
covers  : Version -> OpId -> Bool               -- counter <= vv[replica]  → already seen
fromNode : Node -> Version                       -- fold every stamp in the tree
```

`Crdt/Json.elm` gains a **version-filtered encoder**: the same tree walk it does
today, but it emits a node/entry/element only when at least one of its stamps is
*not* covered by the given `Version` (and prunes wholly-covered subtrees). A
register newer than the vector is emitted; an unchanged map key is skipped; an
RGA emits only its new/newly-deleted elements.

Decoding a delta needs no new code path: a delta is a structurally-valid (sparse)
`Node`, and `merge` already unions sparse maps/RGAs and LWW-resolves registers.
The only subtlety is **maps**: a delta map must still carry its present/absent
keys with stamps so the receiver's keywise merge does the right thing — which it
already does, since we filter by stamp, not by presence.

## Edge cases & risks

- **A key whose value subtree changed but whose own presence stamp didn't.** The
  filter must recurse into entry *values* and emit the entry (with its existing
  presence stamp) when the value has newer facts. Test: edit a nested field only.
- **Tombstone-only deltas.** After stamping deletions, a delta may carry an
  element that exists on the receiver but is newly `deletedAt`. Test: delete on A,
  delta to B, assert B converges to deleted (the existing tombstone-idempotence
  test, but over a delta).
- **Counter gaps from `observe`.** Our counters skip values after observing
  remotes, so a version vector is *not* dense. `covers` must compare against the
  per-replica max, never assume contiguity. This is why `Version` is keyed by
  replica, not a single Lamport number.
- **Clock vs. version vector.** We keep the single Lamport `Ctx` for *minting*
  ids (unchanged); the version vector is derived separately for *selection*. They
  are not the same structure and should not be conflated.
- **Delta must be a no-op when `version` is already current** (`encodeSince
  (version doc) doc` encodes nothing). Cheap, high-value test.

## Testing strategy

Add `tests/DeltaTests.elm`:

1. **Round-trip equivalence:** for random edit sequences, `merge docB (decode
   (encodeSince (version docB) docA))` reads equal to `merge docB docA` (delta ≡
   full state).
2. **Minimality:** `encodeSince (version doc) doc` is empty; after one edit, the
   delta contains exactly that edit's facts (assert size/shape).
3. **Tombstone delta:** delete-then-delta keeps the element deleted on the peer,
   both merge orders.
4. **Convergence under deltas:** the existing multi-replica convergence property,
   but peers exchange `encodeSince` deltas in random order instead of full state.
5. **Law preservation:** stamping deletions must not break the existing
   commutativity/associativity/idempotence fuzz tests over `Node`.

## Migration / blast radius

- `Rga.Element.deleted : Bool` → `deletedAt : Maybe OpId` touches `Crdt.Rga`,
  `Crdt.Node` (merge + maxCounter), `Crdt.Json` (encode/decode), `Crdt.Text`
  (reads `.deleted`), and the `Fuzzer Node`/RGA tests.
- `Crdt.delete` and the list/text delete edits gain a `Ctx`. Small ripple through
  `Crdt.Edit`.
- Wire format changes (tombstones now carry a stamp). We have no compatibility
  obligations, so this is a clean break — bump and move on.
- `Crdt.encode`/`decode` and `merge` keep working unchanged; the new functions
  are additive.

## Out of scope (deliberately deferred)

- **Binary/columnar encoding** — orthogonal compression; deltas already cut the
  bulk. Revisit only if JSON size is still a problem after deltas.
- **GC / tombstone compaction (#6)** — enabled by versioned deletions but is its
  own design (needs a "stable below this version vector" argument).
- **Op-log architecture** — not needed; we stay state-based.
