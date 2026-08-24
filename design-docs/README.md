# Design docs

These are design docs written to assist in development of the library. They are largely AI written and frequently referenced in the source code,
so they may help understand some of the goals and peculiarities of the implementation.

These are _not_ user documentation, they are developer documentation. So if you are trying to use this library, these documents are unlikely to help you. However if you want to hack on this library and are scratching your head on why something works the way it does, you well may find the answer here.

## Reading guide

Are you interested in understanding how this library works and are wondering where to start? I suggest following this order:

1. Read and understand the public API. This will ensure you understand what we're trying to do.
2. Read the following in dependency order, starting with the relevant design doc:

| Doc                                                                                                                  | Source                                                                               |
| -------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| [08-tree.md](08-tree.md) for Frac's role                                                                             | [Id.Internal](../src/Crdt/Id/Internal.elm), [Frac](../src/Crdt/Frac.elm)             |
| [09-fugue.md](09-fugue.md) how we order sequences                                                                    | [Rga](../src/Crdt/Rga.elm)                                                           |
| [05-move.md](05-move.md)                                                                                             | [MoveList](../src/Crdt/MoveList.elm)                                                 |
| [08-tree.md](08-tree.md)                                                                                             | [Tree.Internal](../src/Crdt/Tree/Internal.elm)                                       |
| [16-typed-sequence-content.md](16-typed-sequence-content.md) for why `Txt` is an `Rga Node`                           | [Node](../src/Crdt/Node.elm)                                                         |
| [10-rich-text.md](10-rich-text.md), [11-block-structure.md](11-block-structure.md)                                   | [Text](../src/Crdt/Text.elm), [RichText.Internal](../src/Crdt/RichText/Internal.elm) |
| [02-oplog.md](02-oplog.md) for the DAG, frontiers, causal order                                                      | [OpLog](../src/Crdt/OpLog.elm)                                                       |
| [01-delta-sync.md](01-delta-sync.md)                                                                                 | [Json](../src/Crdt/Json.elm), [OpJson](../src/Crdt/OpJson.elm)                       |
| [03-stable-cursors.md](03-stable-cursors.md)                                                                         | [Cursor.Internal](../src/Crdt/Cursor/Internal.elm)                                   |
| [06-sum-types.md](06-sum-types.md), [13-migrations.md](13-migrations.md), [14-extensibility.md](14-extensibility.md) | [Schema.Internal](../src/Crdt/Schema/Internal.elm)                                   |

3. Read [Doc.Intenal](../src/Crdt/Doc/Internal.elm) along with the remaining design docs.
