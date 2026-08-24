# 07 — `Ref`: a type-safe write API (v2)

**Status:** ✅ **built, and now the _only_ public write API.** `Crdt.Ref` — a
write-only optic `Ref r kind a`, `at` composition, `set`/`over`/`increment`/`switch`;
element refs (`index`/`key`) and container ops (`append`/`remove`/`move`,
`setKey`/`removeKey`); typed cursors (`cursorAt`/`cursorOffset`); ref-emitting
builders for both records (`record`/`field`/`build`) and sum types
(`custom`/`variantN`/`buildCustom`). Kind phantom lives on `Crdt kind a`
(Option 2) with container element kinds (2a: `ListK mv ek a` carries element kind +
`Fixed`/`Movable`, `DictK vk a` carries value kind); no cardinality (writes are
total). Tested in `tests/RefTests.elm` (15) + compile-failure probes (`increment`
on text, `move` on a `Fixed` list — both type errors).

**The stringly-typed `Path` API is gone from the public surface.** `Crdt`,
`Crdt.Edit`, `Crdt.History`, and `Crdt.Path` are no longer exposed modules — the
package's only editing story is typed refs on `Crdt.OpDoc`. (Those modules remain
in-repo, unexposed, exercised by the state-based convergence/law tests.) The demo
edits entirely through refs — zero `Path` usage.

The design write-up below is retained; the Resolution section records the decisions.
**Roadmap item:** #1, the **v2 typed API**, designed and built together with sum
types ([`06-sum-types.md`](06-sum-types.md)).
**Depends on:** `Crdt.Schema`, `Crdt.Path` (refs compile down to `Path`), the edit
layers (`Crdt.OpDoc`, `Crdt.Edit`). **No change to `Crdt.Node`/`Crdt.OpLog`/merge** —
this is a typed layer over the existing runtime path core.

## Resolution (supersedes the exploration below)

The exploration below works through three encodings and lands on a "hybrid" with a
**cardinality phantom** (`One`/`Opt`/`Many`) kept sound across composition. A later
insight collapses that:

> **Cardinality is only load-bearing for the _read_ path.** Setting is *total* at
> every cardinality — `set` on one target writes it, on an absent variant no-ops,
> on many targets writes them all. Only *reading* needs cardinality, because the
> return type varies (`a` vs `Maybe a` vs `List a`). This API is **write-only**, so
> cardinality buys nothing and is dropped.

Decisions:

- **The optic is a `Ref root a`** — a write-only optic (optics term: a *Setter*). It
  supports `set` / `over`, not `get`. Reads stay at the whole-document level
  (`OpDoc.read` → typed value → pattern-match in Elm); there is deliberately no
  focus-level `get`/`preview`/`getAll`.
- **No cardinality phantom.** `set`/`over`/`listAppend`/… are total over one, zero-
  or-one, and many targets. Writing "add a tag to every todo" is a feature, not the
  bug the hybrid worked to reject.
- **The `kind` phantom stays.** It is still needed: `increment` on a text ref is a
  genuine type error (the *value* type differs — `Int` vs `String`), independent of
  cardinality.
- **Composition is `at`** (not `andThen`). `andThen` is Elm's convention for
  *monadic* sequencing (the next step depends on the previous runtime value); ours
  is pure structural descent — extend the path by one step. `at` reads as navigation
  in a pipe. (Elm 0.19 forbids user-defined infix operators, so it is a named
  function used with `|>`, never a symbol.)
- **Verbs: `set` / `over`.** Note "write-only" is about the *surface*: `over`,
  `increment`, and text `set` all read the current value *internally* to compute
  their op — you just can't extract a value out through a `Ref`.

Sketch of the resulting surface:

```elm
type Ref root kind a          -- write-only optic; `kind` guards ops, no cardinality

at  : Ref sub k2 a -> Ref root k1 sub -> Ref root k2 a    -- structural descent
set : Ref root kind a -> a -> OpDoc root -> Result Error (OpDoc root)
over : Ref root kind a -> (a -> a) -> OpDoc root -> Result Error (OpDoc root)
increment : Ref root Counter Int -> Int -> OpDoc root -> Result Error (OpDoc root)
listAppend : Ref root (ListKind a) x -> Seed -> OpDoc root -> Result Error (OpDoc root)

-- schema hands back refs; one composition word everywhere:
set boardTitle "Trip" doc
over (board.status |> at status.done) String.toUpper doc   -- no-op if not `Done`
over (board.todos  |> at todo.done)  not doc                -- writes every todo
```

Because there is no cardinality to track, the `BadB7` soundness problem that killed
pure-B's single `andThen` **cannot arise** — there is nothing for composition to
erase. So `at` is genuinely one function, and the SpikeC hybrid is *not* needed
(it solved a problem that only exists when reads are in scope). The `kind` guard is
carried through `at` exactly as the spikes showed it is (concrete named type →
legible errors).

Open items unchanged by this: the cross-cutting decisions (CD1 builder shape, CD2
list dual-ref, CD3 v1-`Path` coexistence, CD4 prism `set` = switch-variant vs edit)
still apply; see that section below. CD4 in particular: switching a sum type to a
new variant is a distinct op from `set`-through-a-variant-ref (which edits the
payload iff that variant is active).

---

## Problem

The edit API is **stringly-typed**. You navigate by `Path`:

```elm
OpDoc.setText (Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "text") "hi"
```

Two independent things can be wrong here and **neither is caught at compile time**:

1. **The path may not match the schema.** `field "txet"` (typo), or `field "todos"`
   on a document whose root has no such field → runtime `PathNotFound`.
2. **The op may not match the target kind.** `setText` on a `Bool` field, or
   `increment` on something that isn't a counter → runtime `WrongNodeType`.

For a library sold on "describe your typed document once," runtime path errors are
the weakest seam. We want navigation and edits **checked by the compiler against
the schema**.

## Why derivation is impossible in Elm (the hard constraint)

In Haskell you'd derive lenses via Template Haskell. Elm has **no macros, no
generics, and — critically — no way to recover a field name from an accessor
function.** `.text : Todo -> String` cannot be mapped to `"text"`, and functions
aren't comparable, so you can't look one up in the schema either.

**Consequence:** type-safe foci must be **emitted by the schema definition itself**.
Any design where the user writes paths *separately* from the schema either
re-declares the structure twice (drift) or can't recover the CRDT key. So the v2
schema builder must **hand back the foci** as it defines the structure. This is the
central shape of the whole proposal, independent of which encoding we pick.

## Optics ↔ our containers

The three classic optic strengths line up exactly with our container kinds:

| Optic | Focuses | Container | Editable how |
| --- | --- | --- | --- |
| **Lens** | exactly one, always present | record field, present dict key | `set` / `over` |
| **Prism** | zero-or-one (may not match) | **a sum-type variant** | `set` (switch to it) / `over` (if active) |
| **Traversal** | zero-to-many | `list` / `movableList` elements | `over` each; `overIndex` |

The middle row is why this is designed with sum types: **focusing "the payload if the
active variant is `Foo`" _is_ a prism**, and composing into an argument is
`variantPrism << argLens`. The awkward "descend through `$tag`, branch may be
inactive" problem (06's D3) becomes the ordinary prism story.

## What a CRDT focus must carry

A focus is not a pure lens `(a -> a)`; edits must emit **ops**. Minimum payload:

```elm
-- conceptually, whichever encoding we choose:
--   path   : Path        -- compiles to the existing runtime path
--   schema : Crdt a      -- sub-schema at the focus, so `set`/`over` can seed sub-edits
--   kind   : <phantom>   -- Text | Reg | Counter | List a | ...  (guards container ops)
```

Two payoffs regardless of encoding:

- **The `setText`/`setBool`/`setInt`/`setString` family collapses into one `set`.**
  The focus carries the sub-schema, so `set` knows text does a char-diff while a
  register does LWW. `text` vs `string` register (both `String`) no longer needs a
  distinct function — the schema disambiguates.
- **The phantom `kind` guards container-specific ops at compile time:**
  `increment : Focus root Counter Int -> …`, `listAppend : Focus root (List a) -> …`.
  `increment` on a non-counter is now a *type error*.

**What optics do _not_ subsume:** structural sequence edits — append, remove, move —
are not `over` (a traversal can edit each existing element but can't insert or
reorder). Those stay as dedicated focus-taking ops (`listAppend listFocus seed`,
`listMove listFocus from to`). Optics are the **navigation + leaf/element-edit**
layer, not a total replacement for the edit API.

---

# Decision #3: how to encode the optics

Elm has **no higher-kinded types**, so we cannot write one polymorphic `<<` that
composes lens-with-prism-with-traversal and "just works" like Haskell's `.`. This is
*the* forcing constraint, and the two realistic encodings trade off against it
differently. Below are concrete sketches of both, editing the running schema:

```elm
type alias Board = { title : String, todos : List Todo, status : Status }
type alias Todo  = { text : String, done : Bool }
type Status = Active | Snoozed Int | Done String
```

## Encoding A — three distinct optic types

Separate `Lens`, `Prism`, `Traversal`, each a record of functions, plus explicit
composition operators and up-casts (a Lens *is* a Traversal that hits one; a Prism
*is* a Traversal that hits zero-or-one). This is the elm-monocle / classic-optics
shape.

```elm
type Lens root a      = Lens { path : Path, schema : Crdt a }
type Prism root a     = Prism { path : Path, tag : String, schema : Crdt a }
type Traversal root a = Traversal { paths : root -> List Path, schema : Crdt a }

-- composition: one operator per pairing that can occur
composeLL : Lens r a -> Lens a b -> Lens r b
composeLP : Lens r a -> Prism a b -> Prism r b      -- into a variant
composeLT : Lens r a -> Traversal a b -> Traversal r b
-- … and the up-casts
lensToTraversal  : Lens r a -> Traversal r a
prismToTraversal : Prism r a -> Traversal r a
```

The schema builder returns typed foci per field/variant/element:

```elm
board :
    { schema : Crdt Board
    , title : Lens Board String
    , todos : Traversal Board Todo          -- each element
    , todosList : Lens Board (List Todo)     -- the list container (for append/move)
    , status : Lens Board Status
    }
board = ...

todo : { schema : Crdt Todo, text : Lens Todo String, done : Lens Todo Bool }

status :
    { schema : Crdt Status
    , snoozed : Prism Status Int
    , done : Prism Status String
    }
```

Usage:

```elm
-- edit the title (Lens): compile-checked value type, one `set`
doc |> Crdt.set board.title "Trip"

-- edit the note text of the active `Done` variant (Lens ∘ Prism):
doc |> Crdt.set (board.status |> composeLP status.done) "shipped"

-- edit every todo's text? no — text is per-todo; edit todo 0's done:
doc |> Crdt.setAt (board.todos) 0 ... -- traversal + index
```

**Pros**

- Each optic advertises its **strength in its type**: a `Prism` visibly may-not-match
  (so `set` = "switch to this variant", `over` = "edit if active"); a `Traversal`
  visibly hits many (so it has `over`/`overIndex`, not `set`). The API surface reads
  honestly.
- Op guards are clean: `listAppend` takes a `Lens root (List a)`; `increment` takes a
  `Lens root Int` tagged counter — mismatches are type errors.
- Matches published Elm optics libraries → familiar, and we could even interop.

**Cons**

- **Composition is a combinatorial mess.** With no HKT, every pairing needs a named
  operator (`composeLL`, `composeLP`, `composeLT`, `composePL`, …) or a pile of
  `xToTraversal` up-casts at call sites. `board.status |> composeLP status.done` is
  noisy next to Haskell's `status . _Done`.
- Deep paths read badly: `composeLL (composeLL a b) c`.
- More types to teach, more exposed surface to document and keep coherent.

## Encoding B — one unified `Focus`, phantom-typed by strength

A single opaque type carrying a **phantom cardinality** (`One` / `Opt` / `Many`) and
a **phantom kind**. One `<<`-style `andThen` composes any two, with the cardinality
combined by a type-level rule encoded as a plain function. This is the
erlandsona/elm-accessors approach adapted.

```elm
type Focus root card kind a
    = Focus { path : Path, schema : Crdt a, resolve : root -> List Path }

-- phantom tags (never constructed)
type One  = One      -- always present (lens-like)
type Opt  = Opt      -- zero-or-one (prism-like)
type Many = Many     -- zero-to-many (traversal-like)

-- one composition; the result cardinality is the "weaker" of the two.
-- encoded by making `andThen` accept a combiner the tags resolve:
andThen : Focus a cb kind2 b -> Focus root ca kind1 a -> Focus root (Combine ca cb) kind2 b
```

The builder returns a record of `Focus`es, uniformly:

```elm
board :
    { schema : Crdt Board
    , title : Focus Board One Text String
    , todos : Focus Board Many (Record Todo) Todo
    , todosList : Focus Board One (List Todo) (List Todo)
    , status : Focus Board One (Variants Status) Status
    }

status :
    { schema : Crdt Status
    , snoozed : Focus Status Opt Reg Int
    , done : Focus Status Opt Text String
    }
```

Usage — **one composition operator everywhere**:

```elm
doc |> Crdt.set board.title "Trip"

-- lens ∘ prism, same `andThen` as any other compose:
doc |> Crdt.set (board.status |> andThen status.done) "shipped"

-- deep path reads uniformly:
doc |> Crdt.over (board.todos |> andThen todo.done) not
```

The op guards use the phantom `kind` (and, where needed, `card`):

```elm
set       : Focus root card kind a -> a -> OpDoc root -> Result Error (OpDoc root)
increment : Focus root card Counter Int -> Int -> OpDoc root -> Result Error (OpDoc root)
listAppend : Focus root One (List a) x -> Seed -> OpDoc root -> Result Error (OpDoc root)
```

**Pros**

- **One composition operator.** `a |> andThen b |> andThen c` reads the same at every
  depth and for every optic mix — the closest Elm gets to Haskell's `.`.
- Smaller exposed surface: one type, one compose, `set`/`over` + the guarded ops.
- The cardinality is still visible in signatures (`One`/`Opt`/`Many`), so `set`
  vs `over` semantics can still be constrained where it matters (e.g. `set` on a
  `Many` can be disallowed by requiring `card` in `{ One, Opt }`).

**Cons**

- **Phantom-type errors are notoriously bad.** When `andThen` mis-composes, Elm
  reports a mismatch deep in `Combine ca cb`/phantom land, not "you can't set a
  traversal." This is the single biggest risk and the thing to actually spike.
- The `Combine` cardinality rule has to be encoded without type-level functions —
  in practice either a small set of overloaded `andThen`s (sliding back toward A) or
  a deliberately loose rule that lets a few nonsensical compositions typecheck and
  fail at runtime (partially defeating the point).
- Less familiar than the classic three-optic vocabulary.

## The tension in one line

> **A** makes each optic's *strength* explicit and gives great error messages, at the
> cost of N composition operators. **B** gives one uniform compose and a small
> surface, at the cost of phantom-type error messages and a shaky cardinality-combine
> story.

## Spike results (measured, not assumed)

Both encodings were built as compiling spikes (`spikes/src/SpikeA.elm`,
`SpikeB.elm`) with the three running edits, plus deliberate wrong-usage modules
(`BadA1–4`, `BadB1–7`) whose compiler output was captured. Findings:

**1. Both compose the three real edits.** A needs three named operators
(`composeLL`/`composeLP`/`composeLT`); B does all three with one `andThen`. B's
call sites are cleaner, as predicted:

```elm
-- A: lens ∘ prism
setVariant (composeLP boardStatus statusDone) "shipped" doc
-- B: lens ∘ prism — same andThen as everything else
set (boardStatus |> andThen statusDone) "shipped" doc
```

**2. The feared phantom-error catastrophe did NOT happen.** Every wrong-usage
error in B was as readable as A's, because both put the phantoms in a *concrete,
named* type. Example — `increment` on a text focus, in B:

```
This `boardTitle` value is a:
    Focus Board One Text String
But `increment` needs the 1st argument to be:
    Focus Board One Counter Int
```

That names the real problem (`Text` vs `Counter`). A's equivalent is essentially
identical minus the `One`. So on the *kind* dimension, B is a win: one compose
operator **and** legible errors.

**3. But B's cardinality phantom is UNSOUND — this is the decisive finding.**
The blocker isn't error quality; it's that a single `andThen` **cannot track
cardinality** without type-level functions. To make one operator accept every
pairing, its result cardinality must be a free type variable
(`andThen : … -> … -> Focus root cardResult kind b`). That free variable then
unifies to *whatever the caller needs*. Concretely (`spikes/src/BadB7.elm`,
**which compiles cleanly and should not**):

```elm
manyStep : Focus root Many kx a            -- a traversal (zero-to-many)
listStep : Focus a One (ListKind Int) (List Int)
composed = manyStep |> andThen listStep    -- morally Many, but cardResult is free
x = listAppend composed 42                 -- listAppend wants `One` … and it TYPECHECKS
```

`listAppend`'s `One` guard is silently satisfied by the fabricated `cardResult`.
So "append to the list inside *every* element" would compile and then do something
incoherent. In B, **the `card` phantom is cosmetic** — the only guard that
actually holds across composition is `kind`. (BadB2 — `listAppend` directly on a
`Many` focus with no composition — *is* caught, because there's no `andThen` to
erase the cardinality. It's specifically composition that breaks it.)

To make cardinality real in B you'd need per-cardinality `andThen`s
(`andThenMany`, `andThenOpt`, …) so the result cardinality is computed by
dispatch — which reintroduces A's N-operator problem, defeating B's one reason to
exist.

A, by contrast, tracks cardinality soundly for free: a `Traversal` composed with a
`Lens` yields a `Traversal` (via `composeLT`), and there is no `set`/`listAppend`
that accepts a `Traversal`, so the bad program never typechecks. The cost is
exactly the extra operators.

## Recommendation (post-spike)

**Ship a hybrid: one unified `Focus root card kind a` type (B's legible concrete
errors + small type surface), but expose cardinality-specific composition rather
than a single cardinality-erasing `andThen`.** In practice a few named composers
whose result cardinality is *pinned*, not free:

```elm
into      : Focus a One  k b -> Focus r One k2 a -> Focus r One  k b   -- One ∘ One = One
intoOpt   : Focus a Opt  k b -> Focus r One k2 a -> Focus r Opt  k b   -- into a variant
eachInto  : Focus a One  k b -> Focus r Many k2 a -> Focus r Many k b  -- through a traversal
```

This keeps B's payoffs — one `Focus` type, concrete/legible errors, `set`/`over` +
kind-guarded ops — while making the cardinality guard **sound** (you physically
cannot land a `Many`-derived focus in a `One`-only op). The price is 3–4 composers
instead of one, but they read as intent (`into` / `eachInto`) rather than as
optic-pair bookkeeping (`composeLT`), and there are far fewer of them than A's full
matrix because kind is handled by the single type, not multiplied in.

Net verdict on the original A-vs-B question: **B's ergonomics win and its error
messages are fine — the spike killed only the *single-`andThen`* form, on
soundness, not B itself.** The unified type survives; the "one operator for
everything" dream does not.

> Pure-A remains the safe fallback if the hybrid's composer set turns out to sprawl
> once dict-key, tree, and cursor foci are added — revisit after those exist.

### Hybrid spiked and confirmed (`spikes/src/SpikeC.elm`)

The hybrid was then built and its soundness verified against the exact case that
broke pure-B:

- **`SpikeC.elm` compiles** — all three running edits still compose via the pinned
  composers (`boardStatus |> intoOpt statusDone`,
  `boardTodosEach |> eachInto todoDone`), so the ergonomics survive.
- **`BadC7.elm` (the BadB7 analogue) now FAILS to compile** — composing a `One` step
  through a `Many` outer via `eachInto` yields `Focus root Many (ListKind Int) (List Int)`
  (cardinality *propagated*, not fabricated), and `listAppend` needs `One`:

  ```
  This `composed` value is a:
      Focus root Many (ListKind Int) (List Int)
  But `listAppend` needs the 1st argument to be:
      Focus root One (ListKind Int) (List Int)
  ```

- **`BadC8.elm`** — `set` on a `Many`-derived focus is rejected (`Many` vs `One`).
- **`BadC9.elm`** — you cannot launder a `Many` outer through the `One`-only `into`
  composer; the pipe is rejected at the composer, not deferred to the op.

So the hybrid delivers both properties simultaneously: **one unified `Focus` type
with legible errors, and cardinality that survives composition.**

> **Superseded** by the Resolution at the top: once we accept the API is write-only,
> cardinality is unneeded and this hybrid is not implemented. `SpikeC` remains a
> valid proof that cardinality *can* be tracked soundly if reads are ever added —
> keep it for that branch. The shipped design is `Ref` (no cardinality) + `at`.

## Cross-cutting decisions (still apply to the `Ref` design)

- **CD1 — Builder shape.** A positional `recordN`/`customN` builder must return
  `(schema, refs)`. Options: a big tuple (positional, terse, order-fragile) vs. a
  record of named refs (verbose to declare, self-documenting at use). Lean **record
  of named refs** — it's the thing users hold onto and name.
- **CD2 — Element refs vs. list-container ops.** A `list` gives *both* a ref that
  descends into each element (for `over`/element edits — total over the many) *and* a
  ref to the list container itself (for `append`/`remove`/`move`). The builder should
  hand back both (`todos` and `todosList`). No cardinality type distinguishes them
  now, so the distinction is by **name/kind** (a `ListKind` container ref vs. an
  element ref); naming convention TBD. Note `over` through the element ref writing
  every element is intended and fine.
- **CD3 — Coexistence with v1 `Path`.** Refs compile to `Path`, so `Crdt.set ref`
  is sugar over the existing `resolve`/emit machinery. The v1 path API can stay
  during migration (deprecate later), meaning **zero runtime/merge risk** and an
  incremental cutover.
- **CD4 — Variant `set` semantics (no longer a cardinality question).** Two distinct
  operations, distinguished by *which function you call*, not by the type:
    - `set (root |> at variant.done) "x"` — edit the `Done` payload **iff** the value
      is currently `Done`; **silently no-ops otherwise** (write is total; the ref
      matches zero targets when the variant is inactive).
    - a separate `switch`/`setVariant` op — change the active variant (emit `$tag` +
      payload seed), e.g. `Active → Done "x"`.
  Because there is no `Opt`/`One` phantom to lean on, these must be *named*
  differently rather than both being `set`. Decide the names alongside sum types
  ([`06-sum-types.md`](06-sum-types.md)); the silent-no-op of the first is the
  footgun to document.

## Test / spike plan

1. ~~Two encoding spikes (A, B); capture wrong-usage error text. Then spike the
   chosen hybrid.~~ **Done** — `spikes/src/Spike{A,B,C}.elm` + `Bad{A,B,C}*.elm`.
   Result: hybrid (see "Spike results"). Key artifacts: `BadB7.elm` compiles when it
   shouldn't (proves single-`andThen` cardinality erasure); `BadC7.elm` correctly
   fails (proves the hybrid's pinned composers restore soundness).
2. Implement `Ref` + `at` + `set`/`over` for the real `OpDoc`; round-trip the three
   running edits (ref → `Path` → ops → `read` back).
3. Variant refs: `set` through an active variant edits its payload; through an
   *inactive* one is a **silent no-op**; `switch`/`setVariant` changes the tag.
4. Element refs: `over` writes every element (intended); container ref supports
   `append`/`remove`/`move`.
5. Compile-failure fixtures (elm-verify-examples or a `should-not-compile` note):
   `increment` on a text ref, `set` a `Bool` through a `String` ref, `listAppend` on
   a record ref — all guarded by the `kind` phantom (no cardinality needed).
