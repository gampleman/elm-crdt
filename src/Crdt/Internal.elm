module Crdt.Internal exposing
    ( Seed(..)
    , runSeed
    )

{-| Package-internal plumbing shared across the public modules (not exposed from the
package): the `Seed` type used to construct fresh `Node` subtrees.
-}

import Crdt.Id.Internal exposing (Ctx)
import Crdt.Node exposing (Node)


{-| An **opaque** builder of a fresh `Node` subtree, minting ids from the context.
Produced by `Crdt.Schema.Internal.with`, consumed by the edit APIs in
`Crdt.Doc.Internal`. Opaque so that the edit APIs can accept it without leaking the
internal `Node` type into the public package surface.
-}
type Seed
    = Seed (Ctx -> ( Node, Ctx ))


{-| Run a seed against a context.
-}
runSeed : Seed -> Ctx -> ( Node, Ctx )
runSeed (Seed f) =
    f
