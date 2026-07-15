# elm-crdt

Pure-Elm [Conflict-free Replicated Data Types](https://crdt.tech/): these are data
structures that have the wonderful property that if you edit them concurrently
and then combine the edits in any order, they will always converge to the same
result.

Furthermore, they have this property under decentralization, meaning that they don't
need to rely on a central server to coordinate merging (but you can still use them
if you have a central server!).

This library allows you to relatively easily build applications supporting (some of) these
awesome features:

- **multiplayer collaboration:** make your app behave like Google Docs, Figma, or Linear
  where you can collaborate with others in real time.
- **offline capability:** spotty wifi? Make changes while offline, then sync when you
  get reception.
- **decentralized:** build an app that distributes its data over a generic relay like
  Dropbox or Bluetooth, no server required.
- **high performance:** because you can merge with other edits seamlessly, having stale
  data is much less of a problem. Because of that, you can load and cache all the data
  a user might need and then simply render the data from memory instead of waiting for
  HTTP. Then just receive updates over Websocket and update the UI.
- **complete edit history:** this library efficiently stores every change made to the
  document over time, making it easy to build a time travel UI.
- **local undo:** built in Undo manager for undoing only _your own_ edits.

Compared to some other libraries we also have some useful technical features:

- **delta sync:** sync only changes from some known point rather than the complete
  history. This can make sync messages extremely efficient.
- **collaborative text using [Fugue](https://arxiv.org/abs/2305.00583)**: high quality
  text CRDT that merges concurrent edits in a highly intuitive way.
- **rich text using [Peritext](https://www.inkandswitch.com/peritext/)**: a very high
  quality merge algorithm for formatted text.
- **movable trees** for representing hierarchical data.
- **type safety** both on the read and write paths, including built in support for Custom types.
- **schema evolution capabilities**

## Basic usage

#### 1. Define your Elm document

By document we usually mean the part of the model that will be persisted or synced.

```elm
type alias Board =
    { title : String
    , todos : List String
    }
```

#### 2. Define refs for writing

Refs are pointers into a datastructure that allow us to record edit intent that merges nicely in a type safe way.

```elm
import Crdt exposing (Ref)

type alias BoardRefs =
    { title : Ref Board Crdt.Settable String
    , todos : Ref Board (Crdt.ListK Crdt.Fixed Crdt.Settable String) (List String)
    }
```

#### 3. Define the schema

This teaches elm-crdt how to serialize, deserialize and merge your document.

You'll need to pay attention to use elements with appropriate merge semantics:

```elm
board : Crdt.RecordRefs Board BoardRefs
board =
    Crdt.record Board BoardRefs
        |> Crdt.field "title" .title Crdt.text
        |> Crdt.field "todos" .todos (Crdt.list Crdt.text)
        |> Crdt.build
```

This also uses our BoardRefs to actually populate the value.

#### 4. Makes some (concurrent edits)

```elm
import Crdt.Id

alice : OpDoc Board
alice =
    Crdt.init (Crdt.Id.replica "alice") board.schema
        |> Crdt.set board.refs.title "Hello "
        |> Result.withDefault (Crdt.init (Crdt.Id.replica "alice") board.schema)
```

and on another computer:

```elm
bob : OpDoc Board
bob =
    Crdt.init (Crdt.Id.replica "bob") board.schema
        |> Crdt.set board.refs.title "world"
        |> Result.withDefault (Crdt.init (Crdt.Id.replica "bob") board.schema)
```

#### 5. Merge them together

it doesn't matter on which computer or in what order

```elm
import Crdt.OpDoc as OpDoc

converged : Result Crdt.ReadError Board
converged =
    Crdt.read (OpDoc.merge alice bob)
    -- i.e. (OpDoc.merge bob alice) produces the same result

converged
    |> Result.map .title
    --> Ok "Hello world"
```

Of course in a more realistic example you would serialize these and send them over
some transport, as well as show UI presence, etc. To see this all in action,
checkout our [`/demo`](https://github.com/gampleman/elm-crdt/tree/main/demo).

## Implementation

All replicated state lives in one uniform recursive type (internal `Crdt.Node`)
derived from an operation log, with a single monomorphic `merge`. A separate typed
combinator layer (the `Crdt` module) ties that state to your own Elm records and hands
back typed **refs** — schemas describe _reads_, refs drive _writes_, so convergence
correctness never depends on the codec and edits are compile-checked. Sequences and
text are backed by an RGA; map keys carry last-write-wins presence cells so concurrent
set-vs-remove is well-defined.

## What's included

- **`Crdt`** — the front door. Describe a document from **primitives** (`text`, `richText`,
  `counter`, `int`/`float`/`string`/`bool`) and **containers** (`list`, `movableList`,
  `dict`, `tree`); assemble **records** and **custom types**; `init`/`read` a
  document; and drive every **type-safe write** — `set`, `over`, `increment`, `switch`,
  list `append`/`move`, dict `setKey`, tree `addChild`/`moveInto`, rich-text
  `mark`/`splitBlock`, and more.
- **`Crdt.OpDoc`** — everything else you do with a live document: `merge`, delta sync
  (`encodeSince`), collaborative time-travel (`version`/`versionAt`/`restoreTo`), local
  `undo`/`redo`, history `compact`ion, and named checkpoints.
- **`Crdt.Id`** — the two identity types the library hands back: `ReplicaId` (who a
  replica is) and `OpId` (stable handles to things inside a document).
- **`Crdt.Cursor`** — stable caret/selection positions anchored to element identity,
  so they survive concurrent reordering and deletion.
- **`Crdt.Presence`** — ephemeral "who's here and what are they doing" state (names,
  colours, cursors) kept on a separate channel from the document.
- **`Crdt.Tree`** — the value a `tree` schema reads as: a `Forest` of `Item`s.
- **`Crdt.RichText`** — the values a `richText` field reads as: formatted `Span`s and
  `Block`s.

## Demo

The [`demo/`](https://github.com/gampleman/elm-crdt/tree/main/demo) directory is
a real collaborative todo + notes board: each browser tab is a replica, syncing
over a WebSocket through a tiny broadcast relay, with live presence and version
history. See its
[README](https://github.com/gampleman/elm-crdt/tree/main/demo#readme) to run it.

## Development

```sh
npm ci              # install elm, elm-format, elm-review, elm-test-rs, …
npm test            # compile + format check + elm-review + unit tests
npm run fix         # auto-format and apply elm-review fixes
npm run docs        # preview the package docs locally (elm-doc-preview)
npm start           # run the demo
```

## Acknowledgements

Inspired by [Loro](https://loro.dev) and [Automerge](https://automerge.org).

## License

BSD-3-Clause.
