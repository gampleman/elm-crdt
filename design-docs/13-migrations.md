# 13 — Schema migration / evolution

**Status:** ✅ v1 built (`optional` / `withDefault` / `map` / `aliasedField` /
`catchAll`), 8 tests in `tests/MigrationTests.elm`, full suite 329/0. **Roadmap item:**
#2 of the remaining list.

**As-built note (read this first).** The design below framed migration as pure read-time
tolerance. Building it surfaced one architectural fact that reshaped the mechanism: the
empty `base` is **per-replica and schema-derived** (each peer `init`s its own base), and
it is *not* shipped on the wire — so a newer peer's base fabricates the added fields,
which would **mask** the "field is absent" signal that read-time defaults rely on. The
fix (which the sections below are annotated with) is **seed-the-default**: a
`withDefault d` field seeds `d` — not an arbitrary `0`/`False`/`""` — into the base, so
the fabricated value *is* the intended default; `optional` seeds a uniform `Maybe`-as-map
so `Nothing` is what an added field reads; a rename resolves by **highest stamp** across
old/new names (a base seed's `init` stamp always loses to a real write); and `set` on a
field the base didn't seed **creates the key on write** (`OpDoc.seedNodeAt` →
`seedAbsentField`). All of it stays convergent because a base seed carries a low Lamport
stamp, so any real write wins LWW — the seed is only ever the pre-write value.

How does an application evolve its document types over time without breaking peers who
are still running an older build? In a centralized app you migrate the database on
deploy; in a **local-first, peer-to-peer** system there is no single deploy moment and
no central database — two tabs (or a tab and a months-old cached PWA) on **different
schema versions edit the same document concurrently, indefinitely**. That is the problem
this document scopes a design for.

## What we already have (the ground truth)

The architecture makes this problem narrower than it first looks:

- **The `Node` tree is self-describing and schema-blind at merge.** Every node carries a
  kind tag (`"t"`) and every op an action tag (`"k"`); `applyOp` never consults the
  schema. So an op that sets a **field/key/variant the local schema doesn't know still
  merges into the tree, converges, and re-serializes losslessly.** Unknown data is never
  dropped by sync — only the *schema*, at *read time*, decides what to surface. **We are
  already in Cambria's "translate on read, not write" world**, structurally, for free.
- **Read is the only place drift bites.** `decode : Node -> Result Error a` is a pure
  read-time walk. Today it is **rigid**: a missing declared record field is a hard
  `MissingField` error; a wrong node kind is `TypeMismatch`; an unknown sum-type `$tag`
  is `BadValue "unknown variant"`. Extra record fields and extra dict keys are already
  tolerated (only declared names are looked up).

So migration here is **not** a data-rewriting or deploy-time step. It is: *make the
read-time decode tolerate, and deterministically coerce, the shapes that schema drift
produces* — because the data itself already syncs fine.

## Prior art, and why neither fits as-is

**Lamdera Evergreen** (centralized, single deploy timeline). On each deploy it diffs the
snapshotted `Model`/`Msg` types against the previous deploy and code-generates a stub
`Migrate.elm` with per-type functions (`migrate_Model_v3_v4 : Old.Model -> New.Model`)
you fill in. It works because Lamdera owns both ends and there is **one linear version
timeline**: the whole app steps forward at deploy time, old stored models / in-flight
messages are migrated on arrival to the current version. **This does not fit us:** there
is no deploy moment, no single "current version", and peers on old versions keep writing
*new* data the whole time — a one-way `old -> new` migration at a version boundary can't
model two live versions writing concurrently forever. (What we *can* borrow: the ergonomic
idea of a *typed, per-field migration function you only fill in where inference can't*.)

**Cambria** (decentralized — our exact setting). Bidirectional **lenses** (rename, add-
with-default, wrap, hoist, convert…) form a graph between schema versions; data is
**translated on read** along the shortest path; ops are tagged with the writing schema;
lenses are stored in the document so any peer can translate any peer's writes. This is
the right *shape*, and it confirms our translate-on-read instinct. But full Cambria is
**heavy**: a lens graph, JSON-Patch rewriting, per-op schema tags, and the honest
admission that `convert` breaks the bidirectional law and that the consistency /
conservation / predictability trilemma has no clean general answer. We do not need the
whole thing, and Elm can't express the dynamic lens-graph plumbing ergonomically.

## The design: tolerant, defaulting decoders (+ optional read-time coercion)

Lean on what we have. The v1 mechanism is **evolving the decoder, not the data** — make
`Crdt kind a` able to *read older/newer shapes*, so a peer on any version reads a
document written by any other version to a sensible value. Three layers, in priority:

### Layer 1 — additive evolution without breaking (the 80% case)

The single biggest gap: **there is no way to say "this field may be absent."** So *any*
added field breaks old-doc reads today. Fix with decoder combinators that make absence
and drift first-class — the same vocabulary `elm/json` has:

- **`optional : Crdt k a -> Crdt Settable (Maybe a)`** — a field/value that may be
  missing reads as `Nothing` instead of `MissingField`.
- **`withDefault : a -> Crdt k a -> Crdt k a`** — missing (or undecodable) → `a`. This is
  Cambria's *add-with-default* as a pure read rule: an old doc lacking `priority` reads
  as `Medium`; the field is minted on first write.
- **`map : (a -> b) -> (b -> a) -> Crdt k a -> Crdt k b`** — read-time value transform
  (both directions, so seeding/round-trip stays coherent; unlike Cambria's `convert` we
  require both to keep it honest). Covers int→string, enum re-spelling, unit changes.

With these, the common evolutions are non-breaking **by construction**, no version tags,
no migration functions:

| evolution | how it reads on the *old* peer | how it reads on the *new* peer |
| --- | --- | --- |
| **add a field** (`withDefault`/`optional`) | field ignored (already tolerated) | old docs → default; new docs → written value |
| **remove a field** | — | ignored (already tolerated) |
| **rename a field** | see Layer 2 | keep old name as an `optional` alias, prefer new |
| **widen a value** (`map`) | reads coerced value | reads coerced value |
| **add a sum variant** | **still breaks** → Layer 3 | reads fine |

This layer is cheap, purely additive to `Crdt.Schema`, and covers most real evolution. It
is the first thing to build.

### Layer 2 — field aliases / renames

A rename is "read `newName`, else fall back to `oldName`." Expressed with the Layer-1
primitives as an alias list on a field — decode tries names in order. Writes always go to
the new name; the old name lingers in the `Node` (harmless, tolerated) until a GC/rewrite
pass. No data migration; both a v1 and a v2 peer converge on the same `Node`, and each
reads it through its own alias set. (This is Cambria's `rename` lens, minus the graph.)

### Layer 3 — sum-type drift (the hard case, needs a decision)

Adding a variant is the one evolution that **cannot** be made non-breaking by tolerance
alone: an old peer reading a doc whose `$tag` is a variant it doesn't know has no value to
produce. Options (decide before building):

- **(a) `catchAll` variant** — the schema names a fallback the decoder routes unknown tags
  to (e.g. `Unknown String` carrying the raw tag). Old peers read newer docs as
  `Unknown "wontfix"` instead of `Err`. Forward-compatible, opt-in, no new machinery
  beyond a decoder branch. **Leaning this.** Trade-off: an old peer that *re-writes* the
  value may clobber the unknown variant (the Cambria conservation problem) — documented,
  and mitigated because sum-type writes are a whole-value `switch`, so we can choose to
  **preserve an unread `$tag`** rather than overwrite it.
- **(b) required exhaustive migration** — like Evergreen, force the app to supply a
  total `unknownTag -> a`. Safe but heavy, and it's just `catchAll` with no default.
- **(c) do nothing** — document that adding a variant is a breaking change requiring all
  peers to upgrade. Honest, but weak for a local-first library.

### Layer 4 (out of scope for v1) — declared, versioned lenses

If Layers 1–3 prove insufficient (e.g. structural moves — hoist a field into a nested
record, split one field into two), the escalation is a Cambria-style **lens** recorded in
the document: a small, ordered list of reversible steps (rename / add-default / wrap /
hoist) tagged with a schema id, stored in a reserved doc key so any peer can apply it on
read. This is real work (a lens interpreter, schema-tagged reads) and carries Cambria's
trilemma caveats. **Not built in v1** — recorded so the escalation path is known. The
Layer-1/2/3 combinators are deliberately a *subset* of lens operations, so they forward-
compatibly become the manual-write case of a future lens.

## Why not schema-version tags on the wire?

Tempting (tag each op/doc with a schema version, negotiate). But it fights the model:
there is no single version timeline to tag against (every peer has its own), version
*numbers* don't compose across independent forks, and the data already carries enough
structure (self-describing nodes) to be read by shape. Cambria itself tags ops with a
*schema identity* only to look up the right **lens** — which we're deferring to Layer 4.
For Layers 1–3, shape-tolerant decoding needs no version tag at all. If Layer 4 lands, it
introduces a *schema id* (content-hash of the schema, not a linear number) at that point.

## Concrete v1 scope (proposed)

1. **`optional`, `withDefault`, `map`** in `Crdt.Schema` — the additive-evolution core.
   Pure decoder changes; `empty`/`seed` follow (default seeds the default; `optional`
   seeds `Nothing` as an absent key). No wire, no `OpDoc`, no `Ref` change beyond new
   schema constructors.
2. **field aliases** (Layer 2) — `field` variant (or `alias`) that decodes a list of
   names in order.
3. **`catchAll` for sum types** (Layer 3a) — `Ref.custom` gains a fallback variant;
   `decodeCustom` routes unknown tags to it; sum-type `switch` preserves an unread tag.
4. **Docs + tests**: a "schema evolution" test module asserting the compatibility matrix
   above — a v1-schema doc reads correctly under a v2 schema and vice versa, and both
   converge after concurrent edits from peers on different schemas (the property that
   actually matters). This is the migration analogue of the convergence tests.

Layer 4 (declared lenses) is explicitly **deferred**, with the v1 combinators designed as
its forward-compatible subset.

## Risks / open questions

- **Round-trip coherence of `map`.** Requiring both directions keeps seeding honest, but a
  non-invertible change (two fields → one) isn't a `map`; it's a Layer-4 lens. Document
  the boundary.
- **`catchAll` conservation.** An old peer overwriting an unknown variant loses it. Decide
  the write rule (preserve-unread-tag vs clobber) and test the concurrent case.
- **Interaction with GC.** Aliased/lingering old field names are extra keys that a future
  `gc`/rewrite could prune once no peer reads them — needs the "no peer is old" signal,
  which we don't have in a pure P2P setting. Leave them; they're cheap.
- **This is read policy, not merge policy.** Like the block-structure "constraints are
  read-time" conclusion (design-docs/11), migration here never gates a write or a merge — it
  only changes how converged data is *read*. That keeps convergence untouched.
