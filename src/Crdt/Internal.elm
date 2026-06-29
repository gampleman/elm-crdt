module Crdt.Internal exposing
    ( Checkpoint
    , Doc(..)
    , DocData
    , History
    , Seed
    , ctx
    , history
    , make
    , root
    , setHistory
    , withRoot
    , withRootNoHistory
    )

{-| Package-internal plumbing shared across the public modules. Not exposed from
the package. Holds the `Doc` representation (so `Crdt`, `Crdt.Edit` and
`Crdt.History` can all see it without an import cycle) and the `Seed` alias used
to construct fresh subtrees.
-}

import Crdt.Id exposing (Ctx, ReplicaId)
import Crdt.Node exposing (Node)


{-| A function that builds a fresh `Node` subtree, minting ids from the context.
Produced by `Crdt.Schema.with` and consumed by `Crdt.Edit`.
-}
type alias Seed =
    Ctx -> ( Node, Ctx )


{-| A replica's document: the replicated root, the local clock/replica context,
and local-only history (undo/redo stacks + checkpoints).
-}
type Doc
    = Doc DocData


type alias DocData =
    { root : Node
    , ctx : Ctx
    , history : History
    }


{-| Local, non-replicated history. `past`/`future` are undo/redo snapshots of
the root; `checkpoints` are explicit named versions.
-}
type alias History =
    { past : List Node
    , future : List Node
    , checkpoints : List Checkpoint
    , nextVersion : Int
    }


{-| A named version of the document.
-}
type alias Checkpoint =
    { version : Int
    , message : String
    , author : ReplicaId
    , snapshot : Node
    }


emptyHistory : History
emptyHistory =
    { past = [], future = [], checkpoints = [], nextVersion = 1 }


make : Node -> Ctx -> Doc
make r c =
    Doc { root = r, ctx = c, history = emptyHistory }


root : Doc -> Node
root (Doc d) =
    d.root


ctx : Doc -> Ctx
ctx (Doc d) =
    d.ctx


history : Doc -> History
history (Doc d) =
    d.history


{-| Replace the root as the result of a local edit: push the previous root onto
the undo stack and clear the redo stack.
-}
withRoot : Node -> Ctx -> Doc -> Doc
withRoot newRoot newCtx (Doc d) =
    let
        h =
            d.history
    in
    Doc
        { d
            | root = newRoot
            , ctx = newCtx
            , history = { h | past = d.root :: h.past, future = [] }
        }


{-| Replace the root without touching history (used by `merge`, which is not a
locally-undoable action).
-}
withRootNoHistory : Node -> Ctx -> Doc -> Doc
withRootNoHistory newRoot newCtx (Doc d) =
    Doc { d | root = newRoot, ctx = newCtx }


setHistory : History -> Doc -> Doc
setHistory h (Doc d) =
    Doc { d | history = h }
