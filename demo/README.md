# elm-crdt demo — collaborative board

A two-replica (well, N-replica) collaborative workspace — todos, rich-text files,
an outline tree, and settings — built on `gampleman/elm-crdt`'s **op-log core**
(`Crdt.OpDoc`). Each browser tab is a real,
independent replica: edits emit operations, tabs ship only the **new ops since
their last broadcast** (`OpDoc.encodeSince`) over a real WebSocket through a dumb
broadcast relay, and each client merges them. On connect, peers exchange full
state via a `hello` handshake so joiners catch up. Convergence is guaranteed by
the op-log on each client, not by the server.

It exercises the full library surface, organized into four **tabs** (which tab
you're on is local view state — not replicated — but it *is* broadcast on the
presence channel, so the tab bar and "who's here" list show where each peer is):

- **Todos** — a **movable list** (`S.movableList`) of todos; drag the `⠿` handle to
  reorder. Moves keep each todo's identity, so nested edits and cursors follow it.
  Todo `done` booleans are **lww**.
- **Files** — a **dict** (`S.dict`) of **rich-text** documents (`S.richText`):
  create a file, click to open it, edit its contents in a **TipTap/ProseMirror**
  editor. Inline formatting (bold/italic/underline/strike/code/link) is a **Peritext
  marks CRDT**, and **block structure** (paragraphs, headings, blockquote, list items,
  indent depth — Enter to split, Backspace to merge, Tab/Shift-Tab to indent) uses
  in-sequence block markers so concurrent split/merge/type/indent all converge. Open
  the same file in two tabs to edit together. See
  [`../docs/10`](../docs/10-rich-text.md) + [`../docs/11`](../docs/11-block-structure.md).
- **Outline** — a **movable tree** (`S.tree`): add/nest/re-parent nodes with the
  →/←/+/✕ controls. Concurrent re-parents that would form a cycle converge safely
  (Kleppmann's move algorithm), and siblings stay ordered via a fractional index.
- **Settings** — the board **title** (**text**, character-wise merge), its
  **status** (a **sum type**, `S.custom`), and a **likes counter** (`S.counter`;
  concurrent `+1`s from different replicas sum rather than clobber).

Cross-cutting:

- **presence** — live "who's here", which **tab** each peer is on (also shown as
  colored dots on the tab bar), and which field they're editing, with **stable
  cursors** (`Crdt.Cursor`) that track the right character across concurrent edits.
- **collaborative history** — named checkpoints capture a `Version` (a point in
  the shared op DAG); "preview" is true time-travel via `OpDoc.readAt`, and it
  stays meaningful across peers' concurrent edits.
- **history scrubber + restore** — a slider over the document's linear op history
  (`OpDoc.versionAt`) previews any past step; "restore to here" rewinds the live
  doc via `OpDoc.restoreTo`, which emits diff ops so the revert **syncs** to peers
  rather than rewinding only locally.
- **local undo/redo** — `OpDoc.undo` / `redo` invert *your own* edits as fresh
  ops, so a peer's concurrent edit survives your undo and the undo itself syncs. A
  typing session and a whole drag-reorder each collapse to one step.

## Run it

```sh
cd demo
npm install
npm start
```

`npm start` runs three things in parallel:

- the **relay** (`server/relay.js`) on `ws://localhost:8080`,
- a live-reloading **client** server on <http://localhost:8000> (via
  [`elm-live`](https://github.com/wking-io/elm-live)), which recompiles and hot-
  reloads the Elm app whenever you edit `src/`, and
- **esbuild** in watch mode, which bundles `index.js` + the TipTap editor
  (`editor/crdt-richtext.js`) into `bundle.js`.

It opens a tab automatically — open a **second** tab at the same URL to get a
second replica. Edit in one, watch the other converge. Toggle your network
offline and keep editing; on reconnect the tabs re-sync and converge
automatically.

The four tabs (Todos / Files / Outline / Settings) are described above. Switching
tabs is local view state (never in the document — a reminder that not everything in
a local-first app is a CRDT), but it's broadcast on the presence channel so peers
can see where you are; every tab's document keeps converging behind whichever is
hidden. The Files editor is a **TipTap / ProseMirror** view in a `<crdt-richtext>`
custom element: Elm owns the CRDT, the element reports edit intents up
(`richTextInput` port) and re-renders from spans pushed down (`renderRichText` port).
See [`../docs/10-rich-text.md`](../docs/10-rich-text.md).

To just build the app once (no server), run `npm run build` (builds `elm.js` and
`bundle.js`).

## Browser tests (multi-client)

`npm run test:e2e` runs the [Playwright](https://playwright.dev) suite in `e2e/`. It
builds the app, starts the relay + a static server, and drives **real browser
contexts as independent replicas** — the layer the pure-Elm tests can't reach: the
TipTap binding, the caret/reconcile JS, the port round-trip, and genuine two-client
convergence over a socket. It covers the reported first-load formatting bug, block
split/merge/list editing, and concurrent multi-client convergence. The relay runs on
an isolated port (`?relayPort=8091`) so a stray tab on the default `8080` relay can't
leak its document into a test replica. First run: `npx playwright install chromium`.

## How the pieces fit

```
 tab A (replica) --outgoing--> WebSocket --> relay --broadcast--> tab B
       ^                                                            |
       +----------------- incoming <-- WebSocket <------------------+

 library:  pure Elm, only ever produces/consumes Json.Value (Crdt.encode/decode)
 demo:     owns ports + WebSocket transport (Ports.elm, index.js, server/relay.js)
```

The library never imports a port; the demo never reimplements a CRDT.
