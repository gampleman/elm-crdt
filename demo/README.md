# elm-crdt demo — collaborative board

A two-replica (well, N-replica) collaborative todo + notes board built on
`gampleman/elm-crdt`. Each browser tab is a real, independent CRDT replica. Tabs
sync over a real WebSocket through a dumb broadcast relay; convergence is
guaranteed by CRDT `merge` on each client, not by the server.

It exercises the full library surface:

- **record** — the board (`title`, `todos`, `notes`)
- **text** — collaborative title and per-todo/per-note text (character-wise merge)
- **list** — the todo list (`Crdt.list` of todo records)
- **dict** — free-form notes keyed by string (`Crdt.dict`)
- **lww** — todo `done` booleans
- **presence** — live "who's here" + which field each peer is editing
- **history / version control** — named checkpoints, preview/restore old
  versions, and undo/redo

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
