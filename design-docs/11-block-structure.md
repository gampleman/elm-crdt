# 11 — Rich-text block structure (paragraphs, headings, lists)

**Status:** ✅ done — library + TipTap demo binding. Extends rich text (`design-docs/10`,
done). **Roadmap item:** #2 (rich text — block structure).

Library shipped: distinguished **marker** + **nest-token** elements in the rich char
sequence — now constructors of `Node.RichElem` (`TextChar` | `Token Marker` | `Token Nest`),
so an element is exactly one of the three and every walk has to say what it does with each.
They were originally non-string prims smuggled inside a register (marker = `PInt 0`, token =
`PInt 1`), distinguishable only by convention; `design-docs/16` records why that changed.
Inserting one is its own action (`InsertToken`, wire tag `tok`), because a rich element is
never a whole node;
`RichText.toBlocks : RichNode -> List Block` (`Block = { marker, type_, depth, spans }`);
`OpDoc`/`Ref` edits `splitBlock`/`mergeBlock`/`setBlockType`/`indentBlock`/
`outdentBlock` (mapping to `InsertToken`/`DeleteElem`/`AddMark`) + `OpDoc.readBlocks`. Block type is an opaque `block`-mark string (no library
vocabulary); depth is accretive nest-token count (concurrent indent/outdent commute).
Tested: `tests/BlockCoreTests.elm` (10, pure `toBlocks`) + `tests/BlockTests.elm`
(11, end-to-end: split preserves char identity + marks span it, merge, type-change
LWW convergence, indent/outdent incl. concurrent cancel, concurrent split+edit, wire
sync, undo/redo). Full suite 287 / 0.
**Depends on / touches:** `Crdt.Node` (`RichNode`), `Crdt.RichText` (read model),
`Crdt.OpDoc`/`Crdt.Ref` (split/merge/set-type edits), the TipTap binding
(`demo/editor/crdt-richtext.js`, position mapping). No new container.

## Goal

Lift v1 rich text from **one paragraph** of inline-formatted text to a **sequence of
blocks** — with an open, app-defined type (paragraph, headings, blockquote, list
items, …) **and nesting depth** (indent/outdent) — while preserving everything that
already works: Peritext marks, Fugue ordering, stable cursors, and clean concurrent
merge. Concurrent **split**, **merge**, **type-change**, and **indent/outdent** must
converge without losing text identity or garbling.

## The decision: in-sequence block markers (Peritext), not a list of nodes

**Decided (with the user).** Blocks are **not** separate `RichNode`s in an
`S.list`/`S.tree`. Instead the document stays **one flat Fugue character sequence**,
and a **block boundary is a special marker element** interleaved in that sequence.
Each block's content is the run of characters from one marker to the next.

Why this over a list of text nodes:

- **Split/merge stay identity-preserving and O(1).** Splitting a paragraph = insert
  one block-marker element at the caret. Merging = tombstone one marker. No
  characters move, so their `OpId`s (and any cursor/mark anchored to them) survive.
  With separate nodes, a split would have to *move* half the characters into a new
  node — losing identity, breaking anchored cursors, and making concurrent
  cross-split edits diverge.
- **It reuses the machinery we already have.** Markers ride the same Fugue sequence
  and merge (`Dict.union` of RGA elements) as characters; block *type* is a mark on
  the marker, resolved by the same per-target LWW as bold/link. Nothing new to prove
  about convergence — it's the text+marks model we already tested, applied to a new
  kind of element.
- It's what Peritext, Yjs (XML), and ProseMirror's model all do, for these reasons.

## Representation

The `RichNode` shape is unchanged (`{ text : Rga Node, marks : Dict String MarkOp }`);
we add a new *kind of element* to the `text` sequence and a convention for typing it.

### Block-marker elements

Today every element in `text` is a character: `content = Reg (PString c)`. A block
marker is a distinguished element the read model recognizes as a boundary rather than
a glyph. Two representation options (decide in code review of Part 1):

- **(A) Sentinel prim** — `content = Reg (PString "\u{0000}")` (or a reserved
  marker string). Cheapest (no `Node` change), but overloads `PString`; the read
  model special-cases the sentinel. Risk: a real char equal to the sentinel.
- **(B) A dedicated marker node** — extend the char content convention so an element
  is *either* a char reg *or* a marker. Since element `content` is a `Node`, a marker
  can be a small `Map`/`Reg` tagged distinctly (e.g. `Reg (PString ...)` under a
  known shape) without a new `Node` variant. Cleaner than a magic char; slightly more
  code in `charOf`/seeding.

Took **(B)** — a marker is semantically not text, and overloading a printable char invites
bugs. Note what neither option could express while element content was `Node`: "this element
is a character *or* a marker *or* a nest token" as something the compiler checks. That needed
the content type itself to change (`design-docs/16`), which is where it ended up.

### Block type = a mark on the marker (an opaque string — the library defines no vocabulary)

A block's type is a **mark** whose range covers just the marker element, keyed by a
`"block"` mark type carrying a value mark (`Value "h1"`). Crucially, **the library
does not enumerate block types.** The value is an **opaque string**, exactly like a
`link` href or a `color` — the library stores and LWW-resolves it but never
interprets it. Which strings mean what (`"h1"`, `"blockquote"`, `"ul"`, …) is
entirely the **application's** vocabulary, the same way the demo — not the library —
decides that `link`/`color` are its value marks.

So the library's block contract is just two things:
1. a distinguished **marker element** is a block boundary, and
2. each block's *type* is "the string the `block` mark resolves to on its marker, or
   a default (`""` / no mark) meaning the app's base block."

This means:

- **Type changes converge by LWW** exactly like `link` href — concurrent "make this
  `h1`" / "make this `blockquote`" resolve to the higher-`OpId` op, deterministically,
  with **no library change to add a new type**.
- The demo picks its own set (paragraph default + h1–h3 + blockquote, say); a
  different app could use `"callout"` or `"code-block"` with zero library changes.
- It rides the existing `AddMark`/`clearMark` ops and `Ref.mark`; no new op type for
  typing a block.

### The leading block

The sequence has an implicit first block before the first marker (a document always
has at least one block). The read model treats "characters before the first marker"
as the leading paragraph, so an empty doc = one empty paragraph with no markers, and
the marker count = block count − 1. (Alternative: a mandatory leading marker; decide
in review — the implicit-leading rule keeps the empty doc marker-free.)

### Nesting depth = accretive **nest tokens**, not an LWW value

A block's indent depth is represented by a run of **nest-token elements** attached to
its marker (Fugue-placed right after the marker, before its text). Depth = the count
of live (non-tombstoned) nest tokens. This is deliberately **accretive**, not an LWW
depth number, because that is what makes concurrent indent/outdent converge sensibly:

- **indent** = insert one nest token; **outdent** = delete one nest token; depth =
  live-token count. These are the same `InsertElem`/`DeleteElem` we already have — no
  new op kind, no new addressing (a nest token is a distinguished element like the
  marker, not a counter that an `Increment` must path into).
- Concurrent **indent + outdent** are an insert and a delete of *different* elements,
  so they commute — both apply, net ±0, exactly the intended "they cancel." Two
  concurrent indents → +2. Convergence is free (it's character-insert/delete again).
- An LWW depth value (`Value "2"`) was rejected: concurrent indent-to-2 / outdent-to-0
  would clobber, silently losing one peer's intent. A PN-counter was considered
  (sums cleanly, reuses `Cnt`) but needs `Increment` to path *into* a marker's
  counter content — new addressing — for no real gain over tokens.

**One documented semantic choice:** two concurrent *outdents* from depth 2 must each
choose which token to delete. Rule: **outdent deletes the highest-`OpId` live nest
token** of that block. So two peers each pressing outdent once delete the *same*
token → depth 1 (not 0) — which reads as "one outdent happened," arguably the right
intent. Deterministic given the rule, so it converges. (If an app wanted "each
outdent is independent," it would delete distinct tokens; we choose the
merge-friendly rule.)

Nest tokens are skipped by the text read (they are not glyphs, like markers), and
depth clamps at ≥ 0 by construction (you can't delete a token that isn't there).

## Ops (all existing)

No new op kinds. Every block edit maps onto ops we already have:

- **Split block at caret** → `InsertElem` of a marker element at the caret position
  (Fugue-placed between the surrounding chars, like any insert).
- **Merge block into previous** (backspace at block start) → `DeleteElem` of the
  marker element (tombstoned; the two runs become one block).
- **Set block type** → `AddMark { type_ = "block", value = Value "h1", … }` over the
  marker (or `clearMark` → back to the app's default block).
- **Indent** → `InsertElem` of a nest token after the block's marker; **outdent** →
  `DeleteElem` of the block's highest-`OpId` live nest token.

Because these are the same ops, delta sync, undo/redo, GC, and the recently-fixed
`deps`-chaining all apply unchanged. (Undo of a split = delete the marker; undo of a
merge or outdent = re-insert the deleted element — the existing `ReInsert`/`ReGraft`
revival machinery already handles element re-creation with fresh ids.)

## Read model (`Crdt.RichText`)

`toSpans` currently returns `List Span` (a flat run of formatted text). Block
structure needs a nested read:

```elm
type alias Block =
    { type_ : String          -- app-defined; "" = the default block (from the block mark)
    , depth : Int              -- indent level = live nest-token count
    , spans : List Span        -- the block's inline content, as today
    }

toBlocks : RichNode -> List Block
```

Algorithm: walk the elements in Fugue order (as `toSpans` does); a **marker** closes
the current block and opens the next (its resolved `block` mark = the next block's
type, its live nest-token count = the next block's depth); **nest tokens** are
counted, not emitted; **chars** accumulate into the current block's spans via the
existing per-char active-mark flattening. Tombstoned markers/tokens are skipped.
`toSpans` stays as the single-block/inline view (still used for plain fields);
`toBlocks` is the new block-aware read. `Schema.richText` decodes to `List Block`
(or we keep both and add `S.richTextBlocks` — decide in review).

The `depth` is a flat integer; the *tree* it implies (a depth-2 item nesting under
the nearest preceding depth-1 item, Markdown-style) is inferred by the app/binding at
render time from the flat depths — a pure read convention, so it converges wherever
the depths do. The library stays flat-per-block; no parent pointers, no cycle risk.

## TipTap / demo binding (as built)

Two pragmatic decisions kept the binding tractable and the position mapping robust:

1. **One PM node per CRDT block — flat, not PM's nested lists.** Each block is a
   single custom `block` node (a paragraph variant) carrying `blockType` (the opaque
   CRDT string) and `depth` as attributes; headings/quotes/list-markers/indentation
   are pure **CSS** off `data-type`/`data-depth`. We deliberately do *not* fold depth
   into ProseMirror's genuinely-nested `bullet_list > list_item` trees. That keeps the
   PM↔CRDT mapping **one-block-to-one-block** — the block index is just `$pos.index(0)`
   and a char offset is `$pos.parentOffset` — with no nested-list position arithmetic,
   which is where a native-list binding gets bug-prone. The CRDT fully supports
   nesting; the editor renders it flat-with-indent. (Native PM nested lists are a
   possible future refinement, isolated to the binding.)
2. **Block edits are block-relative, and the CRDT does the offset math.** The editor
   reports intents by `blockIndex` + `charOffset` (split) or `blockIndex` (merge/
   setType/indent/outdent); Elm resolves the index to its marker `OpId` via
   `Ref.readBlocks`, and `OpDoc.splitBlock` resolves `(blockIndex, charOffset)` to the
   underlying element placement. So the JS never computes element positions.

Input routing: Enter (split), Backspace-at-block-start (merge), and Tab/Shift-Tab
(indent/outdent) are caught in a keydown handler and `preventDefault`'d — they emit
*only* an intent, so Elm applies the CRDT edit and pushes the new blocks back (block
intents re-render the editor; text/mark intents, which PM applied optimistically, do
not — see `isBlockIntent`). Toolbar buttons emit mark commands (via TipTap chains, so
they flow through `onTransaction` like keyboard shortcuts) and block-type intents.
Document char offsets for marks are computed by summing block text lengths (block
boundaries are not characters — matching the CRDT's char-only stream).

## Schema & constraints — why they are read-time policy, not merge guarantees

TipTap/ProseMirror let you declare a **schema**: node types, their attributes, and
**content rules** ("a `list_item` contains block content", "a `heading` holds only
inline content and cannot nest", "this mark is disallowed here"). A natural question
is whether this library should offer the same abstraction. Two halves, treated
differently:

- **Structure** (what types/marks exist) — already covered, and deliberately open:
  block type is an opaque string, marks are app-defined. The library needs no type
  registry.
- **Constraints** (nesting/content rules) — **cannot be enforced at the CRDT layer,
  by nature.** ProseMirror can be strict because it is a *local, sequential* editor:
  every transaction is validated before it applies, and an invalid one is rejected.
  A CRDT has no such gate — two peers editing concurrently can converge on a state
  that no single sequential edit would produce (that is the whole point). Example:
  peer A sets a block to `heading` while peer B concurrently indents it → a heading
  at depth 2. If "headings can't nest" were a *merge* rule there is nothing to do:
  the state has already converged on both replicas; you cannot reject it without
  breaking convergence.

So constraints must be **read-time normalization**, not write-time validation:
render an "invalid" state into a valid one **deterministically as a pure function of
the converged state** (e.g. `toBlocks`/the binding simply ignores `depth` on a
heading and renders it flat; the depth data may persist but is inert for that type).
Because every peer normalizes identically, the doc stays convergent — this is display
*policy* over convergent data, never data integrity. It is exactly what ProseMirror's
schema does with pasted/foreign content: coerce to a valid shape. In the demo, TipTap
already provides this layer, so we get schema enforcement "for free" on the binding
side without the library knowing any rules.

**Decision:** keep the library schema-light for block structure v1 — any block type,
any depth, all convergent — and let rules live in the app + TipTap binding. A future
*optional* declarative document-schema (a deterministic read-time normalization +
binding spec, explicitly **not** a merge guarantee) could be ergonomic, but it is out
of scope here and not committed to; this section records the reasoning so the option
isn't rediscovered from scratch.

## Test plan (`tests/BlockTests.elm`)

1. **Split** — split a paragraph mid-text; read two blocks; the char ids on both
   sides are unchanged (identity preserved).
2. **Merge** — delete a marker; the two blocks become one; concurrent edits in both
   halves survive.
3. **Type change convergence** — two peers set different block types on the same
   marker; converge to the higher-`OpId` type, both merge orders.
4. **Concurrent split + edit** — peer A splits a paragraph while peer B types in it;
   converge with B's text on the correct side of the split.
5. **Marks across a split** — a bold range spanning a split point stays bold in both
   resulting blocks (marks anchor to chars, not blocks).
6. **Empty doc / leading block** — no markers = one paragraph; round-trips.
7. **Indent/outdent + concurrency** — indent raises depth; outdent lowers it;
   depth clamps at 0. Concurrent indent + outdent on the same block cancel (net ±0);
   two concurrent outdents from depth 2 → depth 1 (both hit the highest-`OpId`
   token), converging both merge orders.
8. **Wire + undo** — split/merge/type/indent deltas sync to a peer; undo of each
   restores the prior block structure (incl. re-inserting a deleted marker/token).

## Risks / open questions

- ~~**Marker + nest-token encoding**~~ — RESOLVED, twice. Option (B) below (a dedicated
  node shape rather than a magic char) shipped first, as `Reg (PInt 0)`/`Reg (PInt 1)`; that
  still left the vocabulary unchecked, since nothing stopped an element claiming to be a
  marker or being neither. It is now a real sum type, `Node.RichElem` — the option this
  section could not reach at the time because element content was hard-wired to `Node`. See
  `design-docs/16-typed-sequence-content.md`.
- **Leading-block convention** — implicit leading paragraph vs mandatory leading
  marker; lean implicit.
- **Outdent-target rule** — "delete the highest-`OpId` live nest token" is the chosen
  merge-friendly rule (two concurrent outdents = one net outdent). Documented, not a
  blocker.
- **Position mapping** is the riskiest binding work — especially folding flat
  per-block depth into ProseMirror's genuinely-nested list nodes. Pin it with tests
  #4 (concurrent split + edit) and #7 (indent concurrency) through the real intent
  path — the properties that define correctness.
- **Scope discipline** — this doc is blocks + flat nesting over the *existing* marks
  layer. No new container, no new op kind, no library-defined block vocabulary; if
  any seems necessary, stop and reconsider.
