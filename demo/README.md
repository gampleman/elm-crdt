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
- **list** — the todo list (`S.list` of todo records)
- **dict** — free-form notes keyed by string (`S.dict`)
- **lww** — todo `done` booleans
- **presence** — live "who's here" + which field each peer is editing
- **collaborative history** — named checkpoints capture a `Version` (a point in
  the shared op DAG); "preview" is true time-travel via `OpDoc.readAt`, and it
  stays meaningful across peers' concurrent edits.

> Note: `restore`, `undo`, and `redo` from the earlier state-based demo are
> temporarily gone — on the op-log they become syncing inverse-ops, which aren't
> built yet (tracked in `docs/02-oplog.md`). Time-travel *preview* is the
> collaborative replacement and is strictly more capable.

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
