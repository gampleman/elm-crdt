# 03 — Stable cursors

**Status:** DONE
**Roadmap item:** #2 (the half that wasn't MoveElem)
**Depends on:** the op-log (`Crdt.OpDoc`), `Crdt.OpLog` `Target`, `Crdt.Rga`.

## Shipped

- **`Crdt.Cursor`** (public): `Anchor = Start | After OpId`, opaque `Cursor`
  (id-based `Target` + `Anchor`), `Range` as a pair of cursors (`range` /
  `rangeAnchor` / `rangeFocus`), `element` for item-identity, JSON codec.
- **`Crdt.OpDoc`**: `cursorAt : Path -> Int -> …` (make), `cursorOffset`
  (resolve), `cursorRange` (normalized `(start,end)`). Resolution uses
  **`Crdt.Rga.liveCountThrough`** (counts live elements at-or-before the anchor in
  RGA order) — robust across concurrent insert/delete *and* deletion of the
  anchor element itself (tombstones retained).
- **`Crdt.Presence.custom`**: a `FieldCodec` from any encoder/decoder, so the
  demo can carry a serialized `Cursor` on the awareness channel.
- **Demo**: a live **remote title caret** — each peer broadcasts a `Cursor`;
  viewers resolve it against their *own* doc and draw a colored bar at the offset
  (`ch`-approximation over the input, as planned).
- **Tests** (`tests/CursorTests.elm`, 8): round-trip, track-on-insert-before,
  stable-on-insert-after, survive-anchor-delete, convergence, **nested stability**
  (cursor into `todos[k].text` survives a todo inserted before it), range, JSON.

Out of scope as planned: pixel-accurate multi-font caret rendering (the
`ch`-approx is deliberate); selections beyond the `Range` type are not surfaced in
the demo.

### Post-ship fixes (caret not appearing)

First run showed no remote caret / no "who's here". Causes, all fixed:

1. **Presence wasn't sent in the `hello` reply.** On connect a joiner sends
   `hello`; the answering peer resent only its *document*, not its presence — so
   an earlier-joined peer never reached a later joiner. Now `hello` is answered
   with both doc **and** presence. (This also fixes the missing peer list, not
   just the caret.)
2. **Caret only set on keystroke.** Focusing the title now publishes a caret
   immediately (at the text end), not only on the next edit; blur/other-focus
   clears it.
3. **Caret had no reliable size/position.** Gave `.remote-caret` an explicit
   height + a blink animation and `.title-wrap { display:inline-block }` so the
   absolute caret has a positioned, sized box to sit in.

Behavioural note: a caret shows only while the *other* peer is **focused in the
title**. The local DOM caret position isn't read (`onInput` doesn't expose it), so
the broadcast caret sits at end-of-title — it demonstrates stable *tracking*
across concurrent edits, not the peer's exact mid-text offset. A `selectionStart`
port would make it exact; deliberately out of scope.

Library-level coverage: `tests/PresenceTests.elm` now round-trips an optional
`custom` field (the caret path) both present and absent, and merges a peer that
has one with a peer that doesn't. The `hello` fix itself is demo `update` wiring
(no demo test harness), so it's documented rather than unit-tested.

---

_Original plan below._

## Problem

A position in a collaborative document — a text caret, or "which list item is
this peer editing" — must stay meaningful as *other* replicas concurrently edit.
Today the demo identifies the edited field by **visible index** (`todo:0`,
`Path … |> index 0`). That breaks the moment a peer inserts/removes an earlier
item: index 0 now points at a different todo. We want a position that is **stable
by identity**, not by offset, and that **converges** (two peers resolve the same
cursor to the same place once their docs converge).

Scope chosen: **fully stable nested paths** (every `Index` step pinned to an
element id, not just the leaf) **and** a **live remote text caret** in the demo.

## Why this is tractable (unlike MoveElem)

Two facts make cursors a clean, convergent feature with **no new CRDT
machinery**:

1. **Every RGA element already has a permanent, immutable `OpId`.** A stable
   position is just "anchor to element X" — no positioning scheme to invent.
2. **Cursors are ephemeral presence data, not document state.** They ride the
   `Crdt.Presence` channel, never the op log. So there are *zero* merge /
   convergence obligations on the document: a cursor is just data a peer
   broadcasts, and each viewer *resolves* it locally against its own converged
   doc. `resolve` being a pure function of the converged RGA is all the
   "convergence" we need.

Contrast MoveElem, which had to *mutate* document order and hit origin cycles.
Cursors only *read* order.

## The key reuse: `Target` is already a stable nested path

`Crdt.OpDoc.resolve : Path -> OpDoc a -> Result Error (List TargetStep, Node)`
already walks a visible-index `Path` and produces an **id-based** target:

```elm
-- Crdt.OpLog
type TargetStep = IntoKey String | IntoElem OpId   -- map key by name, seq elem by id
type alias Target = List TargetStep
```

That `Target` *is* the stable nested path the design calls for — `IntoElem`
already stores an element `OpId`, not an index. So "fully stable nested paths"
is **mostly already built**; the work is:

- expose `Target` resolution as a cursor type with a **leaf anchor**, and
- add the **reverse** direction (`Target` + anchor → current visible `Path` /
  index) for rendering, mirroring `walk`.

## Design

New module `Crdt.Cursor` (exposed):

```elm
type Anchor
    = Start          -- before the first element (caret at offset 0)
    | After OpId     -- just after this element (caret after it, or "on" it)

type Cursor
    = Cursor (List TargetStep) Anchor   -- stable path into a seq/txt + position in it
```

`Crdt.OpDoc` gains the make/resolve pair (it owns `resolve`/`walk` and the live
state):

```elm
-- make: a visible-index Path (into a text/list) + a visible offset -> a stable Cursor
cursorAt : Path -> Int -> OpDoc a -> Result Error Cursor

-- resolve: where does this cursor point in the CURRENT doc?
--   Nothing if the container no longer exists; otherwise the live visible offset.
cursorOffset : Cursor -> OpDoc a -> Maybe Int

-- convenience for list-item identity ("peer is editing this item")
cursorElem : Cursor -> Maybe OpId       -- the anchored element, if After
```

### make (`cursorAt`)

Reuse `resolve` to turn the container `Path` into a stable `Target` and fetch its
RGA. Then turn the visible `offset` into an `Anchor`:

- `offset <= 0` → `Start`
- else → `After (idAtVisibleIndex (offset - 1) rga)` — the id of the element the
  caret sits *after*. (`Rga.idAtVisibleIndex` already exists.)

### resolve (`cursorOffset`)

Walk the `Target` into the current doc (a read-only variant of `walk` keyed by
`IntoElem`/`IntoKey` — no index translation needed since it's already id-based).
At the leaf RGA:

- `Start` → `0`
- `After id` → **1 + (number of live elements at-or-before `id` in RGA order)**.

The robustness comes from resolving against `Rga.toElementsInOrder` (which
*retains tombstones*): if the anchor element was deleted, it still has an order
position, so we count live elements before it and the caret lands at the nearest
surviving spot — exactly how Yjs/Loro keep a caret valid across a deletion of its
neighbor. `cursorOffset` clamps to `[0, visibleLength]`.

**Convergence:** `cursorOffset` is a pure function of the converged RGA order, so
any two replicas holding the same ops resolve a given `Cursor` to the same
offset. Tested as a property.

## Demo: presence by identity + a live remote caret

Two uses, both driven off the same `Cursor`:

1. **List-item presence (fixes the actual bug).** Replace the index-based
   `todo:<i>` field id with the todo's **stable element id**. `Cursor`'s
   `cursorElem` gives "which item"; highlight the todo whose element id matches,
   so the highlight follows the item across concurrent inserts/removes.

2. **Live remote text caret.** Each peer broadcasts, in its `Presence` cursor, a
   serialized `Cursor` for where it's editing. Viewers `cursorOffset` it against
   their own doc and draw a colored caret at that offset.

### The genuine risk: rendering a caret inside a native `<input>`

You **cannot** position arbitrary content inside an `<input>`'s text. This is the
fiddly part flagged earlier. Plan:

- **Render the collaborative title as a custom text view**, not an `<input>`: a
  row of `<span>`s (or a single positioned container) where we draw the text and
  can insert caret markers at any offset. Local editing still uses a real input
  (or a hidden input / `contenteditable`) for IME + keyboard; the caret overlay
  is a sibling positioned layer.
- **Fallback if that proves too fiddly:** a "ghost caret" — a thin colored bar
  positioned by an approximate per-character width (`ch` units) over the existing
  input. Visually imperfect with proportional fonts but cheap and robust; good
  enough to demonstrate the *library* feature even if the *rendering* is
  approximate.

I'll start with the `ch`-approximation over the existing input (small, low-risk,
proves the data path end-to-end), and only build the custom text view if we want
pixel accuracy. **The library/cursor correctness is independent of which
rendering we pick** — the demo rendering is the only thing in question.

### Presence wire format

`Crdt.Presence` already serializes per-peer state via a `Codec`. Add a `Cursor`
field to the demo's `Cursor` (awareness) record; serialize the `Cursor` as
`{ path : [steps], anchor }` reusing `Crdt.Json`'s `OpId` codec. (A small
`Crdt.Cursor` JSON encode/decode, or expose it through `Presence`'s codec
primitives — decide during implementation.)

## Modules touched

| Module | Change |
| --- | --- |
| `Crdt.Cursor` (new, exposed) | `Anchor`, `Cursor` types; `cursorElem`; JSON codec |
| `Crdt.OpDoc` | `cursorAt`, `cursorOffset` (reuse `resolve`/`walk`; add reverse leaf resolve) |
| `Crdt.OpLog` | maybe expose a read-only `Target` walk helper (or keep in OpDoc) |
| `Crdt.Rga` | likely nothing new — `idAtVisibleIndex` / `toElementsInOrder` suffice |
| demo `Main.elm` | presence carries a `Cursor`; list-item highlight by element id; caret overlay |

## Testing strategy (`tests/CursorTests.elm`)

The library half is fully testable without any UI:

1. **Round-trip identity:** `cursorAt path i doc |> cursorOffset … doc == Just i`
   for in-range `i`.
2. **Stable under concurrent insert before the anchor:** make a cursor at offset
   k; a *peer* inserts j elements before it; after merge, `cursorOffset` returns
   `k + j` (the caret tracked its character, not its index).
3. **Stable under insert after the anchor:** offset unchanged.
4. **Survives deletion of the anchor element:** delete the anchored element on a
   peer; after merge `cursorOffset` returns the nearest preceding live offset
   (never crashes, never `Nothing` just because the element is a tombstone).
5. **Convergence:** two replicas that converge resolve the same `Cursor` to the
   same offset (property over random edit interleavings).
6. **Nested stability:** a cursor into `todos[k].text`; a peer inserts a todo
   before index k; after merge the cursor still resolves into the *same* todo's
   text (this is the "fully stable nested path" payoff — the `IntoElem` for the
   todo is by id, so the outer reorder doesn't disturb it).
7. **JSON round-trip** of a `Cursor`.

## Risks

- **Caret rendering in the demo** (above) — the only real risk; isolated to the
  demo, with a documented `ch`-approximation fallback. Library correctness is
  independent of it.
- **Anchor `After` vs "on":** a caret is "between elements" (`After`), an
  item-presence is "on an element". Both are expressible from `After OpId` +
  `cursorElem`; keep one `Anchor` type, two read helpers.
- **Tombstone growth** is already a known, separate concern (GC todo); cursors
  rely on tombstone retention, which we already guarantee.

## Out of scope

- **Selections (ranges)** — a start+end pair of `Cursor`s; trivial extension once
  single cursors work, but not in this pass.
- **Pixel-accurate multi-font caret rendering** — the custom text view; only if
  the `ch`-approximation isn't good enough.
