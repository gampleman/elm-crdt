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
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Schema as S
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Id


type alias Board =
    { title : String
    , todos : List String
    }


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , todos : Ref Board (S.ListK S.Fixed S.Settable String) (List String)
    }


-- Describe the document once, combinator-style (like elm/json decoders). The
-- builder hands back a typed `Ref` per field alongside the schema.
board : Ref.RecordRefs Board BoardRefs
board =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "todos" .todos (S.list S.text)
        |> Ref.build


-- Two independent replicas edit concurrently, through compile-checked refs…
alice : OpDoc Board
alice =
    OpDoc.init (Crdt.Id.replica "alice") board.schema
        |> Ref.set board.refs.title "Trip"
        |> Result.withDefault (OpDoc.init (Crdt.Id.replica "alice") board.schema)


-- …then exchange ops and merge. merge is commutative, associative, idempotent,
-- so the order they arrive in does not matter.
converged : Result S.Error Board
converged =
    OpDoc.read (OpDoc.merge alice bob)
```

## Design in one paragraph

All replicated state lives in one uniform recursive type (internal `Crdt.Node`)
derived from an operation log, with a single monomorphic `merge`. A separate typed
combinator layer (`Crdt.Schema`) ties that state to your own Elm records and hands
back typed `Crdt.Ref`s — schemas describe *reads*, refs drive *writes*, so
convergence correctness never depends on the codec and edits are compile-checked.
Sequences and text are backed by an RGA; map keys carry last-write-wins presence
cells so concurrent set-vs-remove is well-defined.

## The document, and type-safe writes

A document is a `Crdt.OpDoc` — an operation log with a materialized read model. It
gives you **delta sync** (`encodeSince`), **collaborative time-travel** (`version` /
`readAt` / `versionAt`), **restore** to a past version (`restoreTo`, which syncs
rather than rewinds locally), **local undo/redo** (`undo` / `redo`, op-inverting so
it converges), **garbage collection** (`gc`), and **named checkpoints**.

You describe the document once with `Crdt.Ref`'s `record` / `custom` builders (over
the `Crdt.Schema` leaves — text, a `counter` PN-counter, a **movable list**
`movableList`, lists, dicts) — and the builder hands back typed **`Crdt.Ref`s**
alongside the schema. All edits go through refs, so they are **compile-checked**:
`increment` on a non-counter, `move` on a non-movable list, or `set` with the wrong
value type are *type errors*, not runtime failures, and a field name is written once
(in the schema) so a typo can't happen. There is no stringly-typed path API.

```elm
board =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "votes" .votes S.counter
        |> Ref.build

doc |> Ref.set board.refs.title "Trip"      -- ✓
doc |> Ref.increment board.refs.title 1     -- ✗ compile error: title isn't a counter
```

## What's included

| Module | Purpose |
| --- | --- |
| `Crdt.OpDoc` | The document: `init`/`read`/`merge`, delta sync (`encodeSince`), time-travel (`version`/`readAt`/`versionAt`), `restoreTo`, local `undo`/`redo`, `gc`, checkpoints, JSON |
| `Crdt.Schema` | Leaf & container combinators: `text`, `counter`, `int`/`bool`/…, `list`, `movableList`, `dict`, `tree` — the pieces fields and elements are made of |
| `Crdt.Ref` | Schema-with-refs builders (`record`/`field`/`build`, `custom`/`variant0..3`/`buildCustom`) **and** the type-safe writes they enable (`set`/`over`/`increment`/`switch`/`append`/`move`/tree `addChild`/`moveInto`/…) |
| `Crdt.Tree` | Movable-tree read shape (`Forest`/`Item`) — the value a `tree` schema decodes to |
| `Crdt.Id` | Replica identifiers |
| `Crdt.Text` | Collaborative-text helpers over the RGA |
| `Crdt.Cursor` | Stable positions anchored to element identity (survive concurrent reorder/delete) |
| `Crdt.Presence` | Ephemeral awareness (who's online, cursors) — a separate channel |

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
