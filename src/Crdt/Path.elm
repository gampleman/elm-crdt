module Crdt.Path exposing
    ( Path, Seg(..)
    , root, field, key, index, node
    , segments
    )

{-| A path addresses a location inside a document's `Node` tree. Edits navigate
by `Path`; this is the bridge between the typed schema layer (which only reads)
and the mutation layer (which only writes).

Build paths left-to-right starting from `root`:

    Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

`field` and `key` both descend into a `Map` (records and dicts share the `Map`
node); they are distinct only to read intuitively at the call site. `index`
descends into a `Seq` by _visible_ position.

@docs Path, Seg
@docs root, field, key, index, node
@docs segments

-}

import Crdt.Id exposing (OpId)


{-| One step of a path.
-}
type Seg
    = Field String
    | Key String
    | Index Int
    | NodeId OpId


{-| A path is a sequence of segments from the document root.
-}
type Path
    = Path (List Seg)


{-| The empty path, addressing the document root.
-}
root : Path
root =
    Path []


{-| Descend into a record field by name.
-}
field : String -> Path -> Path
field name (Path segs) =
    Path (segs ++ [ Field name ])


{-| Descend into a dictionary entry by key.
-}
key : String -> Path -> Path
key k (Path segs) =
    Path (segs ++ [ Key k ])


{-| Descend into a sequence by visible index.
-}
index : Int -> Path -> Path
index i (Path segs) =
    Path (segs ++ [ Index i ])


{-| Descend into a tree node by its stable id (nodes are addressed by identity,
not visible position — unlike lists).
-}
node : OpId -> Path -> Path
node id (Path segs) =
    Path (segs ++ [ NodeId id ])


{-| The ordered list of segments.
-}
segments : Path -> List Seg
segments (Path segs) =
    segs
