module Crdt.Schema exposing
    ( Crdt, Error, Seed
    , Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK
    , int, float, string, bool, text, counter, lww
    , list, movableList, dict, tree
    , with, decodeNode, emptyNode, errorToString
    )

{-| The combinator layer: a `Crdt kind a` describes a CRDT's shape and ties it to a
typed Elm value `a`, the way an `elm/json` decoder ties JSON to a value. Compose
the **leaves** here (`int`/`text`/`counter`/…) and **containers** (`list`/
`movableList`/`dict`) to build up field and element schemas.

**Records and sum types are built in `Crdt.Ref`** (`Ref.record` / `Ref.custom`),
because their builders hand back typed `Ref`s alongside the schema — and refs are
the only way to edit a document, so there is never a reason to build an aggregate
schema without them. This module is the leaf/container vocabulary those builders
consume; it deliberately does not expose a record/variant builder of its own.

A `Crdt kind a` carries three capabilities, all keyed off the uniform internal
`Node`: decode a node into `a`, construct an empty node, and seed a node from a
value. The `kind` phantom (`Settable`/`Counter`/`Nested`/`Variants`/`ListK`/`DictK`)
records how the value may be edited, so `Crdt.Ref` can reject nonsensical edits at
compile time; it never affects reads or merge.

@docs Crdt, Error, Seed
@docs Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK
@docs int, float, string, bool, text, counter, lww
@docs list, movableList, dict, tree
@docs with, decodeNode, emptyNode, errorToString

-}

import Crdt.Id exposing (Ctx)
import Crdt.Node exposing (Node)
import Crdt.Schema.Internal as I
import Crdt.Tree as Tree
import Dict exposing (Dict)


{-| A schema tying a typed value `a` to CRDT state, tagged with a phantom `kind`
describing how it may be edited (used by `Crdt.Ref`). Opaque.
-}
type alias Crdt kind a =
    I.Crdt kind a


{-| What can go wrong reading a value through a schema. Opaque; render with
`errorToString`.
-}
type alias Error =
    I.Error


{-| An opaque builder of a fresh subtree from a value, produced by `with`.
-}
type alias Seed =
    I.Seed


{-| Kind marker: an LWW register leaf; supports `set`.
-}
type alias Settable =
    I.Settable


{-| Kind marker: a PN-counter; supports `increment`.
-}
type alias Counter =
    I.Counter


{-| Kind marker: a record or map; edited by descending into fields/keys.
-}
type alias Nested =
    I.Nested


{-| Kind marker: a sum type over `a`; supports variant switching + payload refs.
-}
type alias Variants a =
    I.Variants a


{-| Kind marker: a list of `a` with element kind `ek` and movability `mv`.
-}
type alias ListK mv ek a =
    I.ListK mv ek a


{-| Movability marker for a plain `list` (no reordering).
-}
type alias Fixed =
    I.Fixed


{-| Movability marker for a `movableList` (supports `move`).
-}
type alias Movable =
    I.Movable


{-| Kind marker: a `dict` of `a` with value kind `vk`.
-}
type alias DictK vk a =
    I.DictK vk a


{-| Kind marker: a movable `tree` of `a` with node kind `ek`.
-}
type alias TreeK ek a =
    I.TreeK ek a


{-| An integer LWW register.
-}
int : Crdt Settable Int
int =
    I.int


{-| A float LWW register.
-}
float : Crdt Settable Float
float =
    I.float


{-| A string LWW register. For collaborative editing use `text`.
-}
string : Crdt Settable String
string =
    I.string


{-| A boolean LWW register.
-}
bool : Crdt Settable Bool
bool =
    I.bool


{-| Collaborative text, read as a `String`, backed by an RGA so concurrent edits
merge character-wise.
-}
text : Crdt Settable String
text =
    I.text


{-| A PN-counter, read as its integer total; concurrent increments sum.
-}
counter : Crdt Counter Int
counter =
    I.counter


{-| An explicit LWW marker (the identity — provided for readable schemas).
-}
lww : Crdt kind a -> Crdt kind a
lww =
    I.lww


{-| An ordered list of `a`, backed by an RGA.
-}
list : Crdt ek a -> Crdt (ListK Fixed ek a) (List a)
list =
    I.list


{-| A reorderable list of `a` (elements can be moved with `Crdt.Ref.move`, keeping
identity), backed by `Crdt.MoveList`.
-}
movableList : Crdt ek a -> Crdt (ListK Movable ek a) (List a)
movableList =
    I.movableList


{-| A dictionary of string keys to `a`. Key presence is LWW.
-}
dict : Crdt vk a -> Crdt (DictK vk a) (Dict String a)
dict =
    I.dict


{-| A movable **tree** of `a`: hierarchical, re-parentable, sibling-ordered data.
Reads as a `Crdt.Tree.Forest a`; edit through `Crdt.Ref`.
-}
tree : Crdt ek a -> Crdt (TreeK ek a) (Tree.Forest a)
tree =
    I.tree


{-| Seed a node from a value, producing an opaque `Seed` the edit APIs consume.

    todoSchema |> Crdt.Schema.with (Todo "pack" False)

-}
with : a -> Crdt kind a -> Seed
with =
    I.with


{-| Decode a node through a schema (used internally by `Crdt.OpDoc.read`).
-}
decodeNode : Crdt kind a -> Node -> Result Error a
decodeNode =
    I.decodeNode


{-| The empty node for a schema (used internally by `Crdt.OpDoc.init`).
-}
emptyNode : Crdt kind a -> Ctx -> ( Node, Ctx )
emptyNode =
    I.emptyNode


{-| Render a read error for display.
-}
errorToString : Error -> String
errorToString =
    I.errorToString
