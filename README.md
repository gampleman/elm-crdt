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

Refs are pointers into a datastructure that allow us to record edit intent that merges nicely in a type safe way. They come bundled with the schema in one flat record — a `Ref` per field, plus a reserved `schema` field.

```elm
import Crdt exposing (Ref)

type alias BoardDoc =
    { title : Ref Board Crdt.Settable String
    , todos : Ref Board (Crdt.ListK Crdt.Fixed Crdt.Settable String) (List String)
    , schema : Crdt.Schema Crdt.Nested Board
    }
```

#### 3. Define the schema

This teaches elm-crdt how to serialize, deserialize and merge your document.

You'll need to pay attention to use elements with appropriate merge semantics:

```elm
board : BoardDoc
board =
    Crdt.record Board BoardDoc
        |> Crdt.field "title" .title Crdt.text
        |> Crdt.field "todos" .todos (Crdt.list Crdt.text).schema
        |> Crdt.build
```

`build` fills in the `schema` field; the rest are your edit handles.

#### 4. Makes some (concurrent edits)

```elm
import Crdt.Edit as Edit
import Crdt.Id

alice : Doc Board
alice =
    Crdt.init (Crdt.Id.replica "alice") board.schema
        |> Edit.set board.title "Hello "
        |> Result.withDefault (Crdt.init (Crdt.Id.replica "alice") board.schema)
```

and on another computer:

```elm
bob : Doc Board
bob =
    Crdt.init (Crdt.Id.replica "bob") board.schema
        |> Edit.set board.title "world"
        |> Result.withDefault (Crdt.init (Crdt.Id.replica "bob") board.schema)
```

#### 5. Merge them together

it doesn't matter on which computer or in what order

```elm
import Crdt.Doc as Doc

converged : Result Doc.ReadError Board
converged =
    Doc.read (Doc.merge alice bob)
    -- i.e. (Doc.merge bob alice) produces the same result

converged
    |> Result.map .title
    --> Ok "Hello world"
```

Of course in a more realistic example you would serialize these and send them over
some transport, as well as show UI presence, etc. To see this all in action,
checkout our [demo](#demo).

## Implementation

All replicated state lives in one uniform recursive type (internal `Crdt.Node`)
derived from an operation log, with a single monomorphic `merge`. A separate typed
combinator layer (the `Crdt` module) ties that state to your own Elm records and hands
back typed **refs** — schemas describe _reads_, refs drive _writes_, so convergence
correctness never depends on the codec and edits are compile-checked. Sequences and
text are backed by an RGA; map keys carry last-write-wins presence cells so concurrent
set-vs-remove is well-defined.

## What's included

- **`Crdt`**: describe your document and point into it.
  - Build a schema from **primitives** (`text`, `richText`, `counter`, `int`, `float`,
    `string`, `bool`) and **containers** (`list`, `movableList`, `dict`, `tree`); assemble
    **records** and **custom types**.
  - Get back typed **refs** naming the spots you can edit.
- **`Crdt.Edit`**: **type-safe writes** through those refs.
  The ref's kind makes a nonsensical edit a compile error.
- **`Crdt.Doc`**: work with a live document once you have one:
  - `read`/`readAt` its typed value (current, or at a past version)
  - `merge` two documents together
  - delta sync
  - collaborative time-travel
  - local `undo`/`redo`
  - history compaction
  - named checkpoints
  - git-like branching
- **`Crdt.Id`**: the two identity types the library hands back:
  - `ReplicaId` (who a replica is)
  - `OpId` (stable handles to things inside a document).
- **`Crdt.Cursor`**: stable caret/selection positions anchored to element identity,
  so they survive concurrent reordering and deletion; `cursorAt`/`cursorOffset` create
  and resolve them.
- **`Crdt.Presence`**: ephemeral "who's here and what are they doing" state (names,
  colours, cursors, etc.) kept on a separate channel from the document.
- **`Crdt.Tree`**: the value a `tree` schema reads as: a `Forest` of `Item`s.
- **`Crdt.RichText`**: the values a `richText` field reads as: formatted `Span`s and
  `Block`s.

## Demo

The [`demo/` directory](https://github.com/gampleman/elm-crdt/tree/main/demo) is
a real collaborative todo + notes board: each browser tab is a replica, syncing
over a WebSocket through a tiny broadcast relay, with live presence and version
history. [Try it live!](https://code.gampleman.eu/elm-crdt/)

## Development

```sh
npm ci         # install dev dependencies
npm test       # compile, format check, elm-review, tests
npm run fix    # auto-format and apply elm-review fixes
npm run docs   # preview the package docs locally
npm start      # run the demo
```

## Acknowledgements and comparisons

Inspired by [Loro](https://loro.dev) and [Automerge](https://automerge.org).

Speaking of those libraries, we have some [performance comparisons](https://github.com/gampleman/elm-crdt/tree/main/benchmarks/results/COMPARISON.md).

## License

BSD-3-Clause.
