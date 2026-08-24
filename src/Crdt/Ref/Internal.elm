module Crdt.Ref.Internal exposing (Ref(..))

{-| **Internal.** The `Ref` type, extracted to its own module so every layer that speaks
refs can reference it without a circular import: `Crdt` (where refs are built),
`Crdt.Edit` (where they are edited through), `Crdt.Doc` (where `touched`/`origins` query a
`Diff` with a ref) and `Crdt.Cursor`. `Crdt` re-exposes it as the public `Crdt.Ref` type
alias; users never import this module.

A `Ref` pairs a `Path` into the document with the sub-schema at that spot. It depends only
on lower layers (`Crdt.Path`, `Crdt.Schema.Internal`), so it sits below both `Crdt` and
`Crdt.Doc` in the dependency graph.

@docs Ref

-}

import Crdt.Path exposing (Path)
import Crdt.Schema.Internal as SI


{-| A typed pointer to one editable spot: `r` is the root type, `kind` the edit-capability
marker, `a` the value read. Opaque to users (built by the schema builders in `Crdt`).
-}
type Ref r kind a
    = Ref
        { path : Path
        , schema : SI.Crdt kind a
        }
