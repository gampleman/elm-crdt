# 10 — Rich text (marks) + a TipTap editor demo

**Status:** ✅ done. Part 1 (library marks layer) + Part 2 (TipTap demo) both
shipped. Depends on Fugue ordering (`design-docs/09`, done).

Part 2 shipped: a `<crdt-richtext>` custom element (`demo/editor/crdt-richtext.js`)
hosting a TipTap/ProseMirror editor; `renderRichText`/`richTextInput` ports bridge it
to Elm, which owns the CRDT. Tagged intents (`{tag:"text"}` / `{tag:"mark"}`) map to
`Ref.setRich`/`mark`/`unmark`; loop avoidance via a `crdtFromElm` transaction meta;
caret-preserving reconcile (skip replace when live editor spans already equal the
render). The demo is now two **tabs** (board + editor); the active tab is local view
state. esbuild bundles `index.js` + the editor into `bundle.js`. The demo's editor
path (setRich + mark → delta → peer) is pinned by `tests/DemoSyncTests.elm`.

Part 1 shipped: `Node.Rich` variant + mark types; `Crdt.RichText` (public — `Span`,
`MarkValue`, `toSpans`, cover/flatten); `OpLog.AddMark` op + materialize; `Json`/
`OpJson` wire; `Schema.richText` (kind `RichK`) reading `List Span`; `OpDoc.mark`/
`clearMark`/`setRichText` + `Ref.mark`/`unmark`/`setRich`; undo of a mark (`ReMark`).
Tested by `tests/RichTextCoreTests.elm` (10, pure flatten) + `tests/RichTextTests.elm`
(11, end-to-end incl. value-mark LWW convergence, mark-follows-concurrent-edit, delta
sync, undo/redo). Full suite 262/0.
**Roadmap item:** #1 (missing containers — rich text).
**Touches:** new `Crdt.RichText` layer + `Node.Rich` variant, `OpLog` (mark ops),
`Json`/`OpJson` (wire), `Schema`/`Ref` (a `richText` combinator + mark writes),
`Crdt.Cursor` (anchor sides), and a new demo editor built on **TipTap/ProseMirror**.

## Goal

Collaborative **formatted** text: the character sequence we already have (`Txt`, a
Fugue sequence) plus **marks** — bold, italic, underline, strike, code (boolean) and
link, color (value-carrying) — that survive concurrent editing and formatting and
converge. Demonstrated by a real production-grade editor (TipTap), not a toy, because
the library is meant for real local-first apps (and the intended downstream product
already uses TipTap, so the binding is needed regardless).

Two halves, in order:

1. **Library: a marks layer** (Peritext-style). This is the prerequisite — today
   there is nowhere to store "chars 3–7 are bold". Co-designed against the editor's
   needs but tested on its own.
2. **Demo: a TipTap ↔ elm-crdt binding** — a ProseMirror editor whose document is
   our CRDT, syncing over the existing relay, with remote presence carets.

## Part 1 — the marks layer (Peritext)

### Why Peritext, not inline formatting

The naive approach — store a `{bold : Bool}` on each character register — breaks
under concurrency: if Alice bolds "hello" while Bob types " world" at the end, whose
intent wins at the boundary, and does the new text inherit bold? Inline flags have no
answer. **Peritext** (Litt, Gentle, et al.) models a mark as a **range operation**
anchored to character **identities**, kept in a log and resolved per character at
read time. Because marks reference char `OpId`s (not offsets), they're already
Fugue-agnostic — which is exactly why we did `design-docs/09` first.

### Representation

A new `Node` variant so rich text is a distinct container (plain `Txt` stays for
un-formatted fields like titles):

```elm
type Node = … | Rich RichNode

type alias RichNode =
    { text  : Rga Node            -- the Fugue char sequence (as Txt today)
    , marks : Dict String MarkOp  -- append-only mark-op set, keyed by mark-op id
    }
```

`marks` is an **append-only set keyed by the mark op's `OpId`** — the same
semilattice shape as the tree move-set and the counter: `merge = Dict.union`, so
convergence of the *storage* is free. A mark op:

```elm
type alias MarkOp =
    { id    : OpId          -- this mark op's id (Lamport) — drives LWW at read time
    , type_ : String        -- "bold" | "italic" | "link" | "color" | …
    , value : Prim          -- PBool True to set, PNull to clear; PString for link/color
    , start : MarkAnchor
    , end   : MarkAnchor
    }

type alias MarkAnchor = { ref : Maybe OpId, side : Before | After }
```

- **Boolean marks** carry `value = PBool True` (mark on) and clear with a later op of
  `value = PNull` (mark off). Both compete by LWW.
- **Value marks** (link/color) carry `value = PString "..."`; clearing is `PNull`.
- `start.ref = Nothing` means "start of text"; `end.ref = Nothing` means "end".

### Anchor sides = boundary expansion (the subtle Peritext bit)

Whether a mark grows when you type at its edge is controlled by which side each
endpoint anchors to:

- **start** anchored `Before` char S, **end** anchored `After` char E → typing
  *inside* [S,E] inherits the mark; this is the **expand** behavior wanted for
  bold/italic/underline/strike (extend the format as you keep typing at the end).
- **end** anchored `Before` the char *after* E (i.e. a non-growing right edge) →
  typing at the boundary does **not** inherit — the **non-expand** behavior wanted
  for `code`, and for `link` (you don't want the URL to swallow the next word).

Concretely: a char C is covered by a mark with start anchor a_s and end anchor a_e
iff `a_s ≤ C` and `C ≤ a_e` in Fugue order, where `≤` respects the `Before`/`After`
side. This reuses the same order `Crdt.Rga.toElementsInOrder` already defines; we add
a comparison of a `MarkAnchor` against a char position.

Marks use their own `Node.MarkAnchor` (`{ ref, side : Before | After }`), not
`Crdt.Cursor.Anchor`. During implementation this turned out to make a speculative
`Cursor.Before` extension unnecessary: a selection `Range`'s two existing cursor
anchors (`Start | After OpId`) map directly onto mark start/end anchors
(`Start → {ref=Nothing, side=Before}`, `After X → {ref=Just X, side=After}`). A
`Before OpId` *cursor* case would only be needed for true sticky boundary-expansion,
which is editor-driven/deferred — so `Crdt.Cursor` is left unchanged for v1.

### Read model: flatten marks → spans

`RichText.toSpans : RichNode -> List Span` where

```elm
type alias Span = { text : String, marks : Dict String Prim }
```

Algorithm: walk the visible chars in Fugue order; for each char compute its **active
mark set** = for every mark `type_`, the `value` of the highest-`id` mark op whose
[start,end] covers this char (LWW per type per char — Peritext's resolution rule);
drop entries whose winning value is `PNull`. Group maximal runs of chars with an
equal mark set into spans. This is O(chars × marks) naively; fine for interactive
docs, and optimizable later (sort mark boundaries once) if needed — flagged, not
done.

**Merge quality note:** because resolution is per-character LWW by op id, concurrent
"bold [1,5]" and "unbold [3,7]" resolve deterministically (higher id wins on the
overlap 3–5), identically on every replica. No range splitting or normalization is
stored — the stored form is just the op set; splitting is a read-time artifact.

### Ops + wire

Two new `OpLog.Action`s:

```elm
| AddMark { container : Target, markId : OpId, type_ : String, value : Prim
          , start : MarkAnchor, end : MarkAnchor }
-- (removal is an AddMark with value = PNull; no separate RemoveMark needed)
```

Materialize: `AddMark` inserts the `MarkOp` into the target `Rich` node's `marks`
dict (keyed by `markId`). Idempotent (re-adding same id = no-op), commutative
(dict union) — a clean fit for the existing fold-from-base merge. `Json`/`OpJson`
gain `MarkOp`/`MarkAnchor` encoders (a clean wire break is fine, pre-1.0, as with
Fugue). Clock catch-up (`maxCounter`) must also walk mark-op ids and anchor refs.

### Schema / Ref surface

- `Schema.richText : Crdt Settable RichValue` (a new kind or reuse Settable), reading
  as a structured `RichValue` (spans, or a `{ text : String, marks : … }`), not a
  bare `String`.
- Edits via `Crdt.OpDoc` / `Crdt.Ref`:
  - text insert/delete already exist (`applyTextDiff`) — they operate on `.text`.
  - `OpDoc.addMark : Range -> String -> Prim -> …` and `clearMark : Range -> String
    -> …` (clear = AddMark PNull), addressing the range by two `Cursor`s.
- `Crdt.Ref` gets `mark` / `unmark` helpers on a rich-text ref, mirroring how
  `set`/`increment` work for other kinds.

### Tests (`tests/RichTextTests.elm`)

1. **Boolean mark basics** — bold a range, read spans; unbold a sub-range, read the
   split; bold survives an insert *inside* the range (expand), does *not* grow at a
   non-expand edge.
2. **Value mark LWW** — two peers set different links on the same range; converge to
   the higher-id link on the overlap, both merge orders.
3. **Mark + concurrent text edit** — bold [S,E], peer inserts inside [S,E]; new text
   is bold (anchors are identities, not offsets). Peer deletes S or E; mark still
   resolves (tombstones retained).
4. **Convergence / laws** — mark-op set merge is commutative/idempotent (Dict.union);
   full doc converges under random edit+mark interleavings, both orders.
5. **Wire round-trip** — encode/decode a `Rich` node with marks; delta sync a mark
   op to a peer.
6. **Flatten correctness** — span boundaries land exactly at char boundaries; PNull
   winners are omitted.

## Part 2 — the TipTap / ProseMirror demo binding

### The shape (y-prosemirror, but our ops)

TipTap wraps ProseMirror. The binding is a **custom element** (`<crdt-richtext>`)
that owns a ProseMirror `EditorView`, plus Elm ports carrying ops both ways. Elm
holds the CRDT as the **source of truth**; ProseMirror is a view + input device.

```
 ProseMirror transaction (user types/formats)
      │  (JS) translate steps → intents {insert|delete|addMark} with char offsets
      ▼
 port: editorChanged  ──►  Elm: apply via OpDoc (text diff / addMark) → new ops
      │                                        │
      │                                        ├─ broadcast ops over relay (existing)
      ▼                                        ▼
 Elm: doc → {spans}  ──►  port: docChanged  ──►  (JS) reconcile PM doc to spans
                                                  (replace range or set-doc), WITHOUT
                                                  re-emitting a transaction to Elm
```

**Loop avoidance:** a `docChanged` applied to PM must be flagged (a transaction meta)
so its resulting PM transaction is *not* sent back out `editorChanged`. Standard
y-prosemirror discipline.

### Position mapping — PM positions ↔ our char OpIds

ProseMirror positions count nodes/marks, not raw chars, so the binding maps at the
boundary:

- **On input (PM → Elm):** translate a PM transaction's changed range to a **visible
  char offset** in our text (PM gives text offsets within a textblock; for the
  single-paragraph demo this is near-1:1). Elm turns the offset into ops via the
  existing offset-based `applyTextDiff` / cursor helpers.
- **On render (Elm → PM):** we ship `{spans}` (text + mark set per run); the JS
  rebuilds the PM document fragment. Simpler and more robust than incremental step
  translation for v1; revisit if perf demands.

Scope for v1: **one paragraph** of inline-formatted text (no block structure —
headings, lists, blockquotes are out). That keeps position mapping ~1:1 and focuses
the demo on the marks CRDT, which is the point. Block structure is a documented
follow-up (it maps onto our `Seq`/`Tree`, not the marks layer).

### Presence carets (our job in both worlds — see the design discussion)

Local caret upkeep is ProseMirror's (it maps the selection through transactions).
**Remote** presence carets are ours: each peer broadcasts a `Crdt.Cursor` (char-OpId
anchored) on the existing awareness channel; to draw peer B's caret we resolve B's
cursor → our offset → a PM position → a PM **decoration** (a colored widget). To send
ours: read PM selection → our offset → `Crdt.Cursor`. So `Crdt.Cursor` *is* exercised
— for the multiplayer carets — even though PM handles the local one.

### Demo integration

**Decided: one app, with tabs.** The existing board and the new rich-text editor
become two tabs of the same Elm app (same relay, same identity, same presence
channel). The active tab is **local, non-replicated view state** — a nice incidental
showcase that not everything in a local-first app is a CRDT (it lives in the Elm
`Model`, never in the doc, never broadcast). The board and the rich-text doc are
independent CRDT fields of the one root document, so switching tabs is pure view
state; both keep converging in the background regardless of which tab is shown.

The TipTap `<crdt-richtext>` custom element mounts only while the editor tab is
active. Elm guards against the ProseMirror view existing when the tab is hidden
(custom-element lifecycle handles mount/unmount on show/hide).

### Dependencies

Adds `@tiptap/core`, `@tiptap/starter-kit` (or hand-picked PM packages:
`prosemirror-{model,state,view,transform}`) + a bundler step (esbuild) to the demo.
The library itself stays pure Elm with zero new deps — all TipTap lives in the demo.

## Build order

1. `Node.Rich` + `RichText` module (representation, `toSpans` flatten, mark cover
   test) — pure, no ops yet.
2. `AddMark` op + materialize + `Json`/`OpJson` wire + `maxCounter`.
3. `Schema.richText` + `OpDoc.addMark`/`clearMark` + `Ref` mark/unmark.
4. `Crdt.Cursor` `Before` anchor extension (shared by marks).
5. `tests/RichTextTests.elm` green; full suite + review + format.
6. Demo: `<crdt-richtext>` custom element + ports + TipTap wiring; bundler; presence
   carets; relay unchanged.

Each of 1–5 is a self-contained, testable library step; the demo (6) lands last, once
the marks API has earned its shape.

## Decisions needed before coding

1. **Mark set for v1** — ✅ resolved: **boolean** (bold, italic, underline, strike,
   code) **+ value** (link, color).
2. **Editor surface** — ✅ resolved: **TipTap / ProseMirror** (production-grade,
   real integration, matches the intended downstream product).
3. **Demo placement** — ✅ resolved: **one app with tabs** (board tab + rich-text
   tab); active tab is local view state, both docs converge in the background.
4. **Boolean-mark clear semantics** — model "unbold" as an `AddMark value=PNull`
   competing by LWW (proposed), vs a distinct `RemoveMark`. Lean PNull (fewer ops,
   uniform resolution). *Will finalize in code review of Part 1; not user-facing.*
