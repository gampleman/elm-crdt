module Crdt.Path exposing
    ( Path, Seg(..)
    , root, field, key, index
    , segments, isRoot
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
@docs root, field, key, index
@docs segments, isRoot

-}


{-| One step of a path.
-}
type Seg
    = Field String
    | Key String
    | Index Int


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


{-| The ordered list of segments.
-}
segments : Path -> List Seg
segments (Path segs) =
    segs


{-| Whether this path addresses the root.
-}
isRoot : Path -> Bool
isRoot (Path segs) =
    List.isEmpty segs
