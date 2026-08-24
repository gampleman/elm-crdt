# 06 — Sum types (custom-type variants)

**Status:** ✅ **built.** `S.custom` / `variant0..3` / `buildCustom` in
`Crdt.Schema`; LWW `$tag` register + positional payload map. Tested in
`tests/SumTypeTests.elm` (14). Editing/switching variants is done via `Crdt.Ref`
([`07-optics.md`](07-optics.md)); the demo has a sum-type board status. The design
below stands as written; the D-decisions were implemented as recommended.
**Roadmap item:** #1 (type expressiveness), the **v2 typed API** — built
**together with the `Ref` write API** ([`07-optics.md`](07-optics.md)). This doc
covers the *representation and combinators*; `07` covers *typed editing* (including
how you edit a variant's payload — via `Ref.variantPayload`).
**Depends on:** `Crdt.Schema` (combinator layer), `Crdt.Node` (`Map` + `Reg`), the
edit layers (`Crdt.OpDoc`, `Crdt.Edit`). **No change to `Crdt.Node`/merge.**

## Problem

Idiomatic Elm models state as custom types:

```elm
type Status
    = Active
    | Snoozed Posix
    | Done String
```

`Crdt.Schema` today offers `record` / `list` / `movableList` / `dict` / `text` /
`counter` / primitives — **all product types. There is no variant combinator.** For
any realistic Elm app this is a blocking gap: you cannot describe a custom type, so
you cannot put one in a collaborative document without a bad workaround.

The two workarounds available today are both poor:

- **Tagged record** — a `tag : String` register plus a field per variant's payload,
  all always present. Wastes space; makes impossible states representable (two
  payloads populated at once); the mapping is manual and error-prone.
- **Opaque blob** — `Json.Encode` the whole variant into one `string`/`text`
  register. Kills all sub-field CRDT merging: a concurrent edit to a payload field
  clobbers the entire variant under LWW.

We want custom types to be **first-class in the schema**, with sensible CRDT
semantics for concurrent variant changes.

## Design in one line

> A sum type = an **LWW register for the active tag** + a **`Map` of per-variant
> payload subtrees**. The tag decides which variant is live; each variant's payload
> is its own sub-CRDT that merges independently.

This is pure schema + edit-layer sugar over primitives we already have (`Map`,
`Reg`). The uniform `Node` type and the monomorphic `merge` are **untouched**, so
there is no new convergence surface to prove — the semantics fall out of the LWW
register and `Map` rules that are already tested.

## Node representation

A `custom` schema materializes to a `Map` with:

- a reserved key **`"$tag"`** → an LWW string `Reg` naming the active variant;
- one key **per variant** (e.g. `"active"`, `"snoozed"`, `"done"`) → that variant's
  **payload subtree**.

A variant with **N constructor arguments** stores its payload as a small `Map` of
positional keys `"0" … "N-1"` (reusing the record machinery: a variant is just a
record with positional fields). A **nullary** variant (`Active`) has an empty
payload map (or we omit its key entirely — see decision D4).

Example state for `Done "ship it"`:

```
Map
  "$tag"  -> Reg (PString "done")          -- LWW
  "done"  -> Map { "0" -> Txt (rga "ship it") }
  -- "snoozed" / "active" payload keys may or may not be present (see D1)
```

Decode: read `$tag`, look up that variant's payload key, decode its positional
fields through the variant's sub-schemas, apply the Elm constructor.

## Combinator API

Mirrors the record builder, plus a **dispatcher** (the standard "pattern-match
encoder" trick, as in `miniBill/elm-codec`'s `Codec.custom`). The dispatcher is how
`seed` learns which variant a concrete value is and extracts its payload — Elm has
no generic way to reflect on a custom type, so the caller supplies the `case`.

```elm
statusSchema : Crdt Status
statusSchema =
    S.custom
        (\active snoozed done value ->
            case value of
                Active    -> active
                Snoozed t -> snoozed t
                Done note -> done note
        )
        |> S.variant0 "active"  Active
        |> S.variant1 "snoozed" Snoozed S.int          -- Posix as millis
        |> S.variant1 "done"    Done    S.text
        |> S.buildCustom
```

- `custom dispatcher` — begins the builder; `dispatcher` receives one match-handler
  per variant (in declaration order) plus the value to match.
- `variantN name ctor schema1 … schemaN` — declares a variant: its tag string, its
  Elm constructor, and a sub-schema per argument. We provide `variant0`…`variant3`
  (more can be added; 3 covers the vast majority of Elm custom types).
- `buildCustom` — closes the builder into a `Crdt a`.

The `variantN` count must match `dispatcher`'s handler arity; this is enforced by
the Elm type checker (the builder threads the handler-tuple type), so a mismatch is
a compile error, not a runtime one.

## Concurrency semantics (this is the good part)

Everything follows from "LWW tag + independent payload subtrees":

- **Concurrent *different* variants** (`Active` vs `Done "x"`) — the `$tag` register
  is LWW, so exactly one variant wins deterministically by `(counter, replica)`. No
  garbage/mixed state is ever observable.
- **Concurrent edits to the *same* active variant's payload** (two people editing
  the `Done` note) — merge **character-wise** through the payload's own `Txt` CRDT.
  Strictly better than the blob workaround, which would LWW-clobber.
- **A concurrent edit to a *losing* variant's payload** — retained in that variant's
  subtree but not observed (the tag points elsewhere). Like a tombstoned-but-
  recoverable branch; consistent with how our `dict` keeps non-present values.

Because it's an LWW register + `Map`, the whole thing is automatically commutative,
associative, and idempotent — inherited, not re-derived.

## Edit API

Two shapes, paralleling existing edits:

1. **Switch variant** — seed-shaped, like `listAppend` / `setKey`:

   ```elm
   OpDoc.setVariant path (statusSchema |> S.with (Done "note"))
   ```

   Emits: a `SetReg` on `$tag` + the payload subtree seed for that variant (a
   `SetKeyPresence`-style op carrying the seeded `Node`, reusing the seed mechanism).

2. **Edit inside the active variant** — normal path descent. A path like
   `… |> field "status" |> variantField "0"` (or a dedicated `Path` step) resolves
   *through* `$tag` to the active variant's payload, so nested edits address it
   stably. Path resolution (`walk` in `OpDoc`, `Crdt.Edit`) gains a case: at a
   `custom` node, descend into the payload map of the current `$tag`.

   Open sub-question (D3): do we need a new `Path.Seg`, or can we treat the payload
   as an ordinary nested `Map`/record and reuse `field`/`index`? Leaning **reuse** —
   the payload *is* a `Map`, so `field "status" |> field "0"` may just work once
   `walk` knows to redirect through `$tag`.

## Decisions to pin down before coding

- **D1 — Payload persistence across variant switches.** Going
  `Done "note"` → `Active` → `Done`: do you get `"note"` back (payload keys persist
  in the `Map`) or a fresh empty `Done`? **Recommendation: persist** — it's cheaper
  (the `Map` already keeps keys), it enables the "losing branch recoverable"
  property, and it matches our dict semantics. Caveat to document: `$tag` alone does
  not fully determine observable state; the payload behind it does.
- **D2 — Reserved key collision.** `"$tag"` must never collide with a variant name.
  `$` is not producible by `variantN` (names are user strings, but we can validate /
  the `$` prefix convention makes accidental collision unlikely). **Recommendation:**
  reserve the `$`-prefix for library keys and document it; optionally assert.
- **D3 — Path descent into the active variant.** New `Path.Seg` vs. reuse
  `field`/`index` (see Edit API). **Recommendation:** reuse if `walk` redirection is
  clean; add a `Path.variant`/`Path.payload` step only if reuse is ambiguous.
- **D4 — Nullary variant payload.** Empty payload `Map` vs. omit the key. **Rec:**
  omit — a nullary variant carries no data, so only `$tag` need change. Keeps state
  minimal and makes "switch to `Active`" a single `SetReg`.
- **D5 — Decode of an unknown/missing tag.** If `$tag` names a variant not in the
  schema (schema drift) or is absent (malformed). **Rec:** `Err (BadValue …)` — a
  decode error surfaced through the existing `Schema.Error`, never a crash.

## Why this is low-risk

- **No merge/`Node` change** — it's a `Map` + `Reg` underneath, both already proven.
  The only new code is in `Crdt.Schema` (the builder), `Crdt.OpDoc`/`Crdt.Edit`
  (the `setVariant` edit + `walk` descent), and JSON is already handled (it's a
  `Map`).
- **Type-safe arity** — the builder threads handler types so `dispatcher` arity and
  `variantN` count must agree at compile time.
- **Composes** — a variant payload can be any `Crdt a`, including lists, records, or
  another `custom`, so nested/recursive-ish sums work (within Elm's own recursion
  rules for the schema value).

## Test plan (`tests/SumTypeTests.elm` + `tests/SchemaTests.elm` additions)

1. Round-trip: seed each variant, `read` back the exact Elm value (all arities).
2. Nested payload edit: edit a field *inside* the active variant; reads back.
3. Concurrent different variants → LWW tag winner, deterministic, both merge orders
   agree.
4. Concurrent same-variant payload edits → character-wise merge (no clobber).
5. Losing-variant payload retained (switch away, switch back → payload survives, per
   D1).
6. Variant switch is one logical edit; undo (`OpDoc.undo`) reverts tag + payload.
7. JSON round-trip (should be free — it's a `Map`).
8. Decode errors: unknown tag, missing tag, wrong payload shape → `Err`, no crash.
9. Compose: a `list` of a custom type, and a custom type with a `record` payload.
