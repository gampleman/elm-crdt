# elm-crdt demo — collaborative board

A two-replica (well, N-replica) collaborative todo + notes board built on
`gampleman/elm-crdt`'s **op-log core** (`Crdt.OpDoc`). Each browser tab is a real,
independent replica: edits emit operations, tabs ship only the **new ops since
their last broadcast** (`OpDoc.encodeSince`) over a real WebSocket through a dumb
broadcast relay, and each client merges them. On connect, peers exchange full
state via a `hello` handshake so joiners catch up. Convergence is guaranteed by
the op-log on each client, not by the server.

It exercises the full library surface:

- **record** — the board (`title`, `todos`, `notes`)
- **text** — collaborative title and per-todo/per-note text (character-wise merge)
- **movable list** — the todo list (`S.movableList`); drag the `⠿` handle to
  reorder. Moves keep each todo's identity, so nested edits and cursors follow it.
- **dict** — free-form notes keyed by string (`S.dict`)
- **movable tree** — the outline (`S.tree`): add/nest/re-parent nodes with the
  →/←/+/✕ controls. Concurrent re-parents that would form a cycle converge safely
  (Kleppmann's move algorithm), and siblings stay ordered via a fractional index.
- **lww** — todo `done` booleans
- **presence** — live "who's here" + which field each peer is editing, with
  **stable cursors** (`Crdt.Cursor`) that track the right character across
  concurrent edits.
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

`npm start` runs two things in parallel:

- the **relay** (`server/relay.js`) on `ws://localhost:8080`, and
- a live-reloading **client** server on <http://localhost:8000> (via
  [`elm-live`](https://github.com/wking-io/elm-live)), which recompiles and hot-
  reloads the app whenever you edit `src/`.

It opens a tab automatically — open a **second** tab at the same URL to get a
second replica. Edit in one, watch the other converge. Toggle your network
offline and keep editing; on reconnect the tabs re-sync and converge
automatically.

To just build the app once (no server), run `npm run build`.

## How the pieces fit

```
 tab A (replica) --outgoing--> WebSocket --> relay --broadcast--> tab B
       ^                                                            |
       +----------------- incoming <-- WebSocket <------------------+

 library:  pure Elm, only ever produces/consumes Json.Value (Crdt.encode/decode)
 demo:     owns ports + WebSocket transport (Ports.elm, index.js, server/relay.js)
```

The library never imports a port; the demo never reimplements a CRDT.
