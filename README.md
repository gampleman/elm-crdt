# elm-crdt

Pure-Elm [CRDTs](https://crdt.tech/): compose **records, lists, dictionaries,
collaborative text and last-write-wins registers** into JSON-like documents that
**converge** under concurrent editing — no server arbitration required.

Inspired by [Loro](https://loro.dev) and [Automerge](https://automerge.org), but
reimplemented from scratch in Elm with no ports, no WASM, and no native
dependencies. The library only ever produces and consumes `Json.Value`, so it is
completely transport-agnostic — bring your own WebSocket, `localStorage`,
HTTP, or anything else.

```elm
import Crdt exposing (Doc)
import Crdt.Schema as S exposing (Crdt)
import Crdt.Edit as E
import Crdt.Path as Path
import Crdt.Id


type alias Board =
    { title : String
    , todos : List String
    }


-- Describe the document once, combinator-style (like elm/json decoders).
schema : Crdt Board
schema =
    S.record Board
        |> S.field "title" .title S.text
        |> S.field "todos" .todos (S.list S.string)
        |> S.build


-- Two independent replicas edit concurrently…
alice : Doc
alice =
    Crdt.init (Crdt.Id.replica "alice") schema
        |> E.setText (Path.root |> Path.field "title") "Trip"
        |> Result.withDefault (Crdt.init (Crdt.Id.replica "alice") schema)


-- …then exchange state and merge. merge is commutative, associative, idempotent,
-- so the order states arrive in does not matter.
converged : Result Crdt.Error Board
converged =
    Crdt.read schema (Crdt.merge alice bob)
```

## Design in one paragraph

All replicated state lives in one uniform recursive type (`Crdt.Node`) with a
single monomorphic `merge`. A separate typed combinator layer (`Crdt.Schema`)
ties that state to your own Elm records — schemas only *read*, while all edits
go through path-addressed operations in `Crdt.Edit`, so convergence correctness
never depends on the codec. Sequences and text are backed by an RGA; map keys
carry last-write-wins presence cells so concurrent set-vs-remove is well-defined.

## Two document flavors

The library ships **two interchangeable document types over the same schema and
merge semantics**, so you can pick the trade-off you want:

- **`Crdt` (state-based)** — the document *is* the `Node` tree; `merge` is a
  structural join. Simplest model; full-state sync.
- **`Crdt.OpDoc` (op-log)** — the document is an operation log; state is a
  materialized read model. Adds **delta sync** (`encodeSince`), **collaborative
  time-travel** (`version` / `readAt`), and **named checkpoints**. This is what
  the demo runs on.

Both expose the same combinator schema (`Crdt.Schema`), path-addressed edits, a
`counter` PN-counter, and JSON transport.

## What's included

| Module | Purpose |
| --- | --- |
| `Crdt` | State-based document: `init`, `read`, `merge`, `encode`/`decode` |
| `Crdt.OpDoc` | Op-log document: edits + `merge`, delta sync (`encodeSince`), time-travel (`version`/`readAt`), checkpoints |
| `Crdt.Schema` | The `Crdt a` combinators: `record`/`field`, `list`, `dict`, `text`, `counter`, primitives |
| `Crdt.Edit` | Path-addressed edits for the state-based doc |
| `Crdt.Path` | Build paths into a document |
| `Crdt.Id` | Replica identifiers |
| `Crdt.Text` | Collaborative-text helpers over the RGA |
| `Crdt.Presence` | Ephemeral awareness (who's online, cursors) — a separate channel |
| `Crdt.History` | Local checkpoints, checkout/restore, undo/redo (state-based) |

## Demo

The [`demo/`](https://github.com/gampleman/elm-crdt/tree/main/demo) directory is
a real collaborative todo + notes board: each browser tab is a replica, syncing
over a WebSocket through a tiny broadcast relay, with live presence and version
history. See its
[README](https://github.com/gampleman/elm-crdt/tree/main/demo#readme) to run it.

## Development

This repo uses [mise](https://mise.jdx.dev/) for tooling and npm scripts for the
common tasks (mirroring [gampleman/elm-bench](https://github.com/gampleman/elm-bench)):

```sh
npm ci              # install elm, elm-format, elm-review, elm-test-rs, …
npm test            # compile + format check + elm-review + unit tests
npm run fix         # auto-format and apply elm-review fixes
npm run docs        # preview the package docs locally (elm-doc-preview)
npm run build:demo  # build the demo to demo/elm.js
```

To run the live collaborative demo with a relay + live-reload client server:

```sh
cd demo && npm install && npm start
```

## License

BSD-3-Clause.
