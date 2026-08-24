module Crdt.Tree exposing
    ( Forest, Item
    , itemId, itemValue, itemChildren
    )

{-| The value you read out of a **tree** field — hierarchical, re-orderable data such
as an outline, a file tree, or threaded comments.

You describe a tree with `Crdt.tree`, edit it through `Crdt.Edit` (move a node
under a new parent, reorder it among its siblings, change its contents, or delete it),
and read it back as a `Forest` of `Item`s. Each item keeps a **stable id** across every
move, so the ids you get from a read are exactly what you pass back to the editing
functions in `Crdt.Edit` to say _which_ node to act on.

    view : Forest Comment -> Html msg
    view forest =
        Html.ul [] (List.map viewItem forest)

    viewItem : Item Comment -> Html msg
    viewItem item =
        Html.li []
            [ Html.text (Tree.itemValue item).body
            , view (Tree.itemChildren item)
            ]

Trees converge under concurrent editing without ever forming a cycle: if two people
concurrently move nodes in a way that would make a node its own ancestor, one of the
moves is dropped on merge and its node simply stays where it was. Siblings stay in a
consistent order for everyone.

@docs Forest, Item

@docs itemId, itemValue, itemChildren


# Converting to a rosetree

If you'd rather work with a richer tree type, a `Forest` maps cleanly onto a
[rosetree](https://package.elm-lang.org/packages/gampleman/elm-rosetree/latest/) — a
`Forest a` becomes a `List (Tree a)`, since a rosetree has a single root while a `Forest`
can have several. Build each one with the rosetree package's own `unfold` (`Rose.unfold`
below), seeding from an `Item`, so the construction stays stack-safe on deep trees:

    import Tree as Rose

    toRosetrees : Forest a -> List (Rose.Tree a)
    toRosetrees forest =
        List.map fromItem forest

    fromItem : Item a -> Rose.Tree a
    fromItem =
        Rose.unfold
            (\item ->
                ( Tree.itemValue item
                , Tree.itemChildren item
                )
            )

-}

import Crdt.Id exposing (OpId)
import Crdt.Tree.Internal as I


{-| A tree read as a nested value: an ordered list of top-level `Item`s, each carrying
its own children. This is what `Crdt.tree` reads as.
-}
type alias Forest a =
    I.Forest a


{-| One node of a read tree. Read its parts with `itemId`, `itemValue` and
`itemChildren` — it is opaque so that the tree can be extended without breaking your
code.
-}
type alias Item a =
    I.Item a


{-| The node's stable id. Pass this to the tree-editing functions in `Crdt.Edit`
(`Crdt.Edit.moveInto`, `Crdt.Edit.removeNode`, …) to say which node you mean; it does not change
when the node is moved or edited.
-}
itemId : Item a -> OpId
itemId =
    I.itemId


{-| The node's decoded value (whatever your schema's element type reads as).
-}
itemValue : Item a -> a
itemValue =
    I.itemValue


{-| The node's children, in sibling order — itself a `Forest`, so you recurse into it
the same way you render the top level.
-}
itemChildren : Item a -> Forest a
itemChildren =
    I.itemChildren
