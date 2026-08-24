# 14 — Extensibility: user-defined CRDT types

**Status:** ✅ built — `S.opSet { contribution, fold }` + `Ref.contribute` / `Ref.retract`
(with removal in v1). `tests/ExtensibilityTests.elm` (7) builds max-register, MV-register,
and add-wins set as user types and proves convergence; full suite 336/0. **Roadmap item:**
#3 of the remaining list.

**As-built:** exactly the design below. A user CRDT is `opSet` over the existing `Map`
node — contributions written under freshly-minted op-id keys (`Ref.contribute`, returning
the key), removable by key (`Ref.retract` — an LWW presence flip), folded at read by the
user's pure `fold`. **Zero changes to the closed `Node`/`Action` unions or the wire.**
Convergence is `Dict.union` of contributions (proven by the concurrent-edit tests, both
merge orders). A new kind marker `OpSetK ck c` (contribution kind + type) gates the edit verbs so an op-set is only
`contribute`/`retract`-able, never `set`.

Can a library *user* define their own CRDT type — one with conflict-resolution semantics
we didn't ship (a grow-only max-register, a multi-value register that keeps concurrent
siblings, an add-wins set over values, a counter that folds differently) — without
forking the library?

## The two things "a custom type" can mean

The architecture forces a sharp split, and getting the design right is mostly about
naming which side a request falls on.

### (A) A custom *interpretation* of an existing node — already supported

`Crdt kind a` **is** a user-supplied interpretation of the fixed `Node`: a triple of
`decode : Node -> Result Error a`, `empty`, `seed`. Every built-in is one — `int`/`text`
over `Reg`/`Txt`, `counter` over `Cnt`, `dict`/`record`/sum-types over `Map`. The
migration combinators (`map`, `optional`, `withDefault`, `custom`/`catchAll`) are more of
the same: a domain type stored in an existing node kind with custom read logic.

So any custom type **whose convergence equals an existing kind's merge** is already
expressible today, no core change: a domain register (`map` over `string`/`int`), a
record, a sum type, a dict/list/tree of your type. This is a large fraction of "custom
types" in practice.

### (B) A custom *convergence* the fold doesn't already compute — the real question

The schema layer controls only **read** (`decode`) and **initial write shape**
(`empty`/`seed`). It has **no merge/`encode` hook.** Conflict resolution lives elsewhere:

- On the **public op-log path** (`Crdt.OpDoc`), `merge` is just op-set union
  (`Dict.union`) — convergence is **entirely read-time re-fold** in `OpLog.applyOp`'s
  per-action updaters (register LWW, counter sum, RGA order, tree/move, marks).
- (The state-based `Node.merge` also exists but `OpDoc` never calls it.)

So a type needing *new* resolution — max/min register, MV-register, OR-set over values, a
custom counter — cannot be built in the schema layer as it stands, because that
resolution has to run in the fold, which the schema layer can't reach.

## Why we can't just "let users add a `Node` variant"

`Node` and `Action` are **closed unions** in a pure-Elm library; there is no runtime type
extension. Adding a variant (were we to do it *in* the library) breaks the build at 3
sites (`Node.reStampWithMap`, `rank`, `maxCounter`), adds an `applyOp` arm + wire-codec
cases, and — worse — **silently misbehaves** at ~10 wildcard sites, most dangerously
`Node.merge` (a new variant falls through to the LWW rank tiebreak → a silent convergence
bug) and the `applyOp` updaters' `_ -> current` no-ops. A user obviously can't do this at
all, and even for us it is error-prone. So "open the union" is a non-goal; the design must
give custom *convergence* **without** a new variant.

## The key insight: the counter is the template

`Cnt` is `Dict String Increment` — a **grow-only map of contributions keyed by the op-id
that produced each**, folded to a value at read (`sum`). Merge is `Dict.union` (trivially
commutative/associative/idempotent — a shared op-id key carries an identical
contribution). *All* the semantics is in the **read-fold**.

This is the shape of an **operation-based CRDT** in general: state = the set of
operations, value = a fold over them. And crucially, a `Map` node **already is** an
op-id-keyed contribution store: writing a contribution under its own unique op-id key is
a `SetKeyPresence` op; merge unions the keys; the schema's `decode` folds
`presentEntries`. So the whole class — max-register, MV-register, OR-set-over-values,
custom counters, LWW-with-your-own-tiebreak — is expressible as **"a `Map` of op-keyed
contributions + your fold"**, using the *existing* `Map` node and `SetKeyPresence` action.
**Zero new node variants, zero new actions, zero wire changes.** Convergence is free
(union), and the user only writes a pure fold — the part that is actually theirs.

### Proposed: `S.opSet` (working name)

A combinator that packages the pattern:

```elm
opSet :
    { contribution : Crdt ck c        -- schema for one contribution's value
    , fold : List c -> a              -- read: fold present contributions → the value
    }
    -> Crdt (OpSetK ck c) a
```

- **Node representation:** a `Map` whose keys are op-ids, each entry a `contribution`
  node. `empty` = empty map (folds to `fold []`).
- **Write:** a single new `Ref` verb, `contribute : Ref … -> c -> OpDoc -> OpDoc`, that
  mints an op-id and emits `SetKeyPresence (target ++ [IntoKey <opId>]) present=True
  seed=<contribution c>`. (This is `setKey` with a freshly-minted op-id key — the one
  genuinely new primitive, and it's tiny, reusing existing ops.)
- **Read:** `decode` collects `presentEntries`, decodes each through `contribution`, and
  applies the user's `fold`.
- **Merge:** nothing to write — `Dict.union` of the map keys, already correct.

Worked examples the user gets for free:

| custom type | contribution | fold |
| --- | --- | --- |
| **max-register** | `int` | `List.maximum >> Maybe.withDefault 0` |
| **min-register** | `int` | `List.minimum >> …` |
| **multi-value register** | `a` | `identity` (keep all concurrent values) |
| **add-wins set** (grow-only) | `a` | `Set.fromList` |
| **custom counter** | `int` | your own combine (e.g. capped sum) |
| **LWW, your tiebreak** | `(stamp, a)` | pick by your comparator |

Removal/tombstoning (turning grow-only into a two-phase set, or LWW-with-delete) can
layer on later by letting a contribution be `present=False` (the `SetKeyPresence` flag is
already there) — noted, not in the first cut.

### What `opSet` does *not* cover

Sequence-like CRDTs with novel *ordering* (a different list/text convergence than
Fugue-RGA) are genuinely out of reach without touching the ordered-node machinery —
ordering isn't a fold over an unordered set, it needs the RGA/Fugue element graph. That
stays a library-internal concern (adding such a type is the "new variant" cost above),
and is a **non-goal** for user extensibility. `opSet` deliberately targets the
map/set/register family, which is where user-defined CRDTs overwhelmingly land.

## Scope (proposed)

1. **`S.opSet { contribution, fold }`** in `Crdt.Schema` — the node is a `Map` of op-keyed
   contributions; `decode` folds present entries; `empty` = empty map.
2. **`Ref.contribute`** — mint an op-id, emit `SetKeyPresence` writing the contribution under
   that key. The only new primitive (a thin wrapper over the existing presence op).
3. **Tests** — a `tests/ExtensibilityTests.elm` building a max-register and an MV-register
   as user types, asserting: concurrent contributions from two replicas both survive and
   converge in both merge orders (the CRDT law), and the fold reads correctly. This is the
   proof the extension point is a real CRDT, not just a hook.
4. **Docs** — a short "define your own CRDT" guide showing the max-register end to end.

Explicitly **out of scope for v1:** opening the `Node`/`Action` unions to user variants
(impossible in pure Elm and error-prone even for us), and custom sequence *ordering*.

## Risks / open questions

- **Op-id as a map key.** Contributions keyed by minted op-ids means the map grows one
  entry per contribution (like the counter, and like RGA elements). That's the normal CRDT
  cost; GC/compaction of settled contributions is the same open problem the op-log already
  has, not new here.
- **`fold` must be a pure, order-independent function of the contribution *set*** for the
  result to converge. That's the one law the user must uphold; document it loudly (a fold
  that depended on insertion order would break convergence). `List.maximum`, `Set.fromList`,
  `sum` are all fine; "take the first" is not.
- **Contribution identity.** Keying by op-id gives each contribution a stable identity, so
  a later op could target/tombstone a specific one (removal, above). Good to design the key
  scheme now even if removal ships later.
- **Kind phantom.** `opSet` needs a `kind` marker (`OpSetK ck c` — carrying the contribution KIND as well as its type, so `contribute` cannot be handed a differently-merging schema of the same type) so `Ref.contribute` is the
  only edit verb the compiler allows on it (no `set`/`increment`), mirroring how `Counter`
  gates `increment`.
