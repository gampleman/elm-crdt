module Crdt.Edit exposing
    ( EditError(..), editErrorToString
    , set, over, increment, switch
    , append, insert, remove, move
    , setKey, removeKey
    , addChild, moveInto, moveBefore, moveAfter, removeNode
    , setRich, mark, unmark
    , readBlocks, readBlocksAt, setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock
    , contribute, retract
    , readAt
    )

{-| **Edit a document through the typed refs your schema hands you.** You describe the
document and get its refs in the `Crdt` module; you _change_ it here.

Every edit takes a `Ref` (naming the spot), whatever the operation needs, and the `Doc`,
and returns `Result EditError (Doc doc)` (`contribute` also hands back the contribution's
key, and the `read…` functions here return the value read). The ref's `kind` makes edits
**safe**: the compiler rejects a nonsensical operation — `increment` on text, `move` on a
non-movable list, `set` with the wrong value type — so most mistakes are compile errors,
not runtime surprises. What's left, an `EditError`, is a target that didn't resolve
against the current value; you can usually `Result.withDefault doc` past it.

    import Crdt
    import Crdt.Edit as Edit

    doc1 =
        Edit.set board.title "Trip plan" doc
            |> Result.withDefault doc

    doc2 =
        Edit.increment board.votes 1 doc1
            |> Result.withDefault doc1

    -- Edit.increment board.title  =>  a compile error


# Errors

@docs EditError, editErrorToString


# Simple values

@docs set, over, increment, switch


# Lists

`append` adds to the end, `insert` at any index (`0` prepends), `remove` deletes by index;
all work on any list. `move` (reorder) is reserved for a `movableList`.

@docs append, insert, remove, move


# Dictionaries

@docs setKey, removeKey


# Trees

Node ids come from reading the tree (`Crdt.Tree.itemId`); pass them to these.

@docs addChild, moveInto, moveBefore, moveAfter, removeNode


# Rich text

@docs setRich, mark, unmark
@docs readBlocks, readBlocksAt, setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock


# Your own CRDT types

@docs contribute, retract


# Reading a ref at a past version

@docs readAt

-}

import Crdt exposing (Counter, DictK, ListK, Movable, OpSetK, RichK, Schema, TreeK, Variants)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Doc.Internal as DocI
import Crdt.Id as Id
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node)
import Crdt.Ref.Internal exposing (Ref(..))
import Crdt.Rga as Rga
import Crdt.RichText as RichText exposing (MarkValue, Span)
import Crdt.Schema.Internal as SI
import Dict



-- ERRORS ----------------------------------------------------------------------


{-| Why an edit couldn't be applied — the error type the editing functions return.

The compile-time checks already rule out the _kind_ mistakes (you can't `increment` text,
or `move` a non-movable list — those don't compile). What's left are the two runtime ways
a target can fail to resolve against the document's **current** value, both of which you
can branch on:

  - `PathNotFound where_` — nothing lives at the spot the ref points to right now. Most
    often a list index or dictionary key that isn't there (perhaps a peer removed it), or
    a tree node id that has since been deleted. The `String` names the spot.
  - `WrongNodeType detail` — something is there, but it isn't the kind of value the edit
    expects (for instance a block edit aimed at a field that turned out not to be rich
    text). This points at corrupt or mismatched data rather than a normal race, and the
    `String` describes what was expected.

In practice most call sites treat either as "the edit didn't apply, keep the document as
it was" and `Result.withDefault doc` past it. Branch on the variants when you want to tell
the two apart — e.g. ignore a `PathNotFound` (a benign race) but log a `WrongNodeType` (a
real bug). Render either with `editErrorToString`.

-}
type EditError
    = PathNotFound String
    | WrongNodeType String


{-| A human-readable description of an `EditError`, for logging or a message. Branch on
the variants themselves if you need to react differently to each.
-}
editErrorToString : EditError -> String
editErrorToString err =
    case err of
        PathNotFound s ->
            "path not found: " ++ s

        WrongNodeType s ->
            "wrong node type: " ++ s


fromEditError : DocI.Error -> EditError
fromEditError err =
    case err of
        DocI.PathNotFound s ->
            PathNotFound s

        DocI.WrongNodeType s ->
            WrongNodeType s


mapEdit : Result DocI.Error a -> Result EditError a
mapEdit =
    Result.mapError fromEditError



-- SEEDERS RECOVERED FROM A CONTAINER REF --------------------------------------
-- The list/dict edit APIs seed a new element WITHOUT a separate element-schema arg by
-- recovering the element seeder from the CONTAINER schema the ref carries. `seedOneFrom`
-- (in Schema.Internal) seeds a singleton collection from the live clock and extracts the
-- lone element's node — stamp-sound. We supply the singleton-builder + extractor here
-- because we statically know the container's concrete kind. (Not usable for `tree`/`opSet`,
-- whose `seed` deliberately builds empty — those edits keep an explicit element bundle.)


seedListElement : Schema (ListK mv ek e) (List e) -> e -> SI.Seed
seedListElement listSchema value =
    SI.seedOneFrom (\v -> [ v ]) firstSeqElement listSchema value


seedDictValue : Schema (DictK vk v) (Dict.Dict String v) -> v -> SI.Seed
seedDictValue dictSchema value =
    SI.seedOneFrom (\v -> Dict.singleton "0" v) firstMapValue dictSchema value


firstSeqElement : Node -> Maybe Node
firstSeqElement node =
    case Node.asSeq node |> Maybe.andThen (Rga.elements >> List.head) |> Maybe.map .content of
        Just content ->
            -- plain list / text (`Seq`/`Txt`): an RGA element's content
            Just content

        Nothing ->
            -- movable list (`Mov`): the singleton's one cell's content
            Node.asMov node
                |> Maybe.andThen (MoveList.toEntries >> List.head)
                |> Maybe.map Tuple.second


firstMapValue : Node -> Maybe Node
firstMapValue node =
    Node.presentEntries node |> List.head |> Maybe.map Tuple.second



-- SIMPLE VALUES ---------------------------------------------------------------


{-| Set the value at a ref: a present spot is overwritten, emitting minimal ops (a text
spot still merges character-wise).

Through a **sum-type payload ref**, the write lands only while that variant is active. If
the variant was active at some point and isn't now, the payload key still exists and the
write is a harmless no-op on the read. But for a variant that has **never** been active the
key is absent entirely and there is nothing to write under, so you get
`Err (PathNotFound …)` — use `switch` to make the variant active first. Call sites that
don't care can `Result.withDefault doc` either way.

-}
set : Ref r kind a -> a -> Doc doc -> Result EditError (Doc doc)
set (Ref r) value doc =
    DocI.seedNodeAt r.path (SI.with value r.schema) doc
        |> mapEdit


{-| Modify the value at a ref: read it, apply `f`, write it back. No-op if the spot
doesn't currently resolve (e.g. an inactive variant payload).
-}
over : Ref r kind a -> (a -> a) -> Doc doc -> Result EditError (Doc doc)
over (Ref r) f doc =
    case DocI.subValue r.schema r.path doc of
        Ok current ->
            DocI.seedNodeAt r.path (SI.with (f current) r.schema) doc
                |> mapEdit

        Err _ ->
            Ok doc


{-| Add `delta` to a counter ref. Only compiles for a `Counter` ref, so
`increment` on a register or text is a **type error**.
-}
increment : Ref r Counter Int -> Int -> Doc doc -> Result EditError (Doc doc)
increment (Ref r) delta doc =
    DocI.increment r.path delta doc
        |> mapEdit


{-| Change which **variant** a custom-type value is, seeding the new variant's payload
from the value you pass. Only compiles for a `Variants` ref (one built by `Crdt.custom`).

The distinction to keep straight: `switch` moves between variants; a payload `Ref` (one
the `custom` builder handed you, reached via `Crdt.at`) edits _inside_ the variant that's
currently active. Given the `Status` type from `Crdt.custom`:

    -- switch the whole status from whatever it is to `Archived "old project"`:
    doc |> Edit.switch statusRef (Archived "old project")

    -- vs. edit the reason text *within* an already-Archived status (a no-op if the
    -- status is currently Active or Snooze):
    doc |> Edit.set (statusRef |> Crdt.at status.archivedReason) "new reason"

So reach for `switch` when the case changes (`Active` → `Archived`), and a payload ref
when the case stays the same but its contents change. Which variant is active merges
last-write-wins.

-}
switch : Ref r (Variants v) v -> v -> Doc doc -> Result EditError (Doc doc)
switch (Ref r) value doc =
    DocI.seedNodeAt r.path (SI.with value r.schema) doc
        |> mapEdit



-- LISTS -----------------------------------------------------------------------


{-| Append a value to the end of a list ref. The element schema is recovered from the list
ref itself (no schema argument needed). Compiles for any list (`ListK _ ek e`).
-}
append : Ref r (ListK mv ek e) (List e) -> e -> Doc doc -> Result EditError (Doc doc)
append (Ref r) value doc =
    DocI.listAppend r.path (seedListElement r.schema value) doc
        |> mapEdit


{-| Insert a value at **visible index `i`** of a list ref (`0` prepends; an `i` at or past
the end is the same as `append`), seeded through the element schema. Compiles for any list
(`ListK _ ek e`) — you don't need a `movableList` just to insert somewhere other than the
end (e.g. a "newest first" feed that prepends). Concurrent inserts at the same position
converge deterministically and keep each contiguous run intact ([Fugue](https://arxiv.org/abs/2305.00583)).

    -- prepend a new item (newest-first list):
    Edit.insert todosRef 0 todo doc

-}
insert : Ref r (ListK mv ek e) (List e) -> Int -> e -> Doc doc -> Result EditError (Doc doc)
insert (Ref r) i value doc =
    DocI.listInsert r.path i (seedListElement r.schema value) doc
        |> mapEdit


{-| Remove the element at visible index `i` from a list ref.
-}
remove : Ref r (ListK mv ek e) (List e) -> Int -> Doc doc -> Result EditError (Doc doc)
remove (Ref r) i doc =
    DocI.listRemove r.path i doc
        |> mapEdit


{-| Move the element at visible index `from` to index `to`. Compiles **only** for a
`Movable` list (`ListK Movable …`) — calling it on a plain `list` is a type error.
The moved item keeps its identity (nested edits and cursors follow it).
-}
move : Ref r (ListK Movable ek e) (List e) -> Int -> Int -> Doc doc -> Result EditError (Doc doc)
move (Ref r) from to doc =
    DocI.listMove r.path from to doc
        |> mapEdit



-- DICTIONARIES ----------------------------------------------------------------


{-| Set (create or overwrite) a dict key to a value. The value schema is recovered from the
dict ref itself (no schema argument needed). Compiles for a dict ref (`DictK vk v`).
-}
setKey : Ref r (DictK vk v) (Dict.Dict String v) -> String -> v -> Doc doc -> Result EditError (Doc doc)
setKey (Ref r) k value doc =
    DocI.setKey r.path k (seedDictValue r.schema value) doc
        |> mapEdit


{-| Remove a dict key (LWW tombstone).

Removing a key this replica has not observed does nothing: a delete races only against
writes it has actually seen, so a concurrent `setKey` of an unseen key wins.

-}
removeKey : Ref r (DictK vk v) dictType -> String -> Doc doc -> Result EditError (Doc doc)
removeKey (Ref r) k doc =
    DocI.removeKey r.path k doc
        |> mapEdit



-- TREES -----------------------------------------------------------------------


{-| Add a new node (seeded from `value`) as the last child of `parent` (`Nothing` =
a new root) in a tree ref.

Takes the **node bundle** (the same one you gave `Crdt.tree`) for the payload schema. Unlike
`append`/`setKey` — whose element schema the library recovers from the container ref — a
tree seeds an empty forest, so the node schema can't be recovered and is passed here.

-}
addChild : Ref r (TreeK ek a) forest -> Crdt.Crdt ek a s -> a -> Maybe Id.OpId -> Doc doc -> Result EditError (Doc doc)
addChild (Ref r) nodeBundle value parent doc =
    DocI.treeAddChild r.path parent (SI.with value nodeBundle.schema) doc
        |> mapEdit


{-| Re-parent `child` to be the last child of `parent` (`Nothing` = a root).
Cycle-forming moves are skipped (the node stays put), so this always converges.
-}
moveInto : Ref r (TreeK ek a) forest -> Id.OpId -> Maybe Id.OpId -> Doc doc -> Result EditError (Doc doc)
moveInto (Ref r) child parent doc =
    DocI.treeMoveInto r.path child parent doc
        |> mapEdit


{-| Move `child` to sit immediately before `sibling` (under the sibling's parent).
-}
moveBefore : Ref r (TreeK ek a) forest -> Id.OpId -> Id.OpId -> Doc doc -> Result EditError (Doc doc)
moveBefore (Ref r) child sibling doc =
    DocI.treeMoveBefore r.path child sibling doc
        |> mapEdit


{-| Move `child` to sit immediately after `sibling` (under the sibling's parent).
-}
moveAfter : Ref r (TreeK ek a) forest -> Id.OpId -> Id.OpId -> Doc doc -> Result EditError (Doc doc)
moveAfter (Ref r) child sibling doc =
    DocI.treeMoveAfter r.path child sibling doc
        |> mapEdit


{-| Remove a node and its subtree from a tree ref.
-}
removeNode : Ref r (TreeK ek a) forest -> Id.OpId -> Doc doc -> Result EditError (Doc doc)
removeNode (Ref r) child doc =
    DocI.treeRemove r.path child doc
        |> mapEdit



-- RICH TEXT -------------------------------------------------------------------


{-| Replace the character content of a **rich-text** ref (a minimal text diff, like
`set` on plain text). Marks are preserved and follow the surviving characters. Only
compiles for a `RichK` ref.
-}
setRich : Ref r RichK (List Span) -> String -> Doc doc -> Result EditError (Doc doc)
setRich (Ref r) value doc =
    DocI.setRichText r.path value doc
        |> mapEdit


{-| Read a rich-text ref as **blocks** (type + depth + spans + marker id), rather
than the flat `List Span` the schema decodes to. Used to resolve a block index to its
marker `OpId` for the block edits. See [`Crdt.RichText.Block`](Crdt-RichText#Block).
-}
readBlocks : Ref r RichK (List Span) -> Doc doc -> Result EditError (List RichText.Block)
readBlocks (Ref r) doc =
    DocI.readBlocks r.path doc
        |> mapEdit


{-| Read a rich-text ref as **blocks as of a past `Crdt.Doc.Version`** — the time-travel
counterpart of `readBlocks`, mirroring `Crdt.Doc.readAt`. A UI previewing history uses this
to feed the editor the blocks the document had at that version; the live document is
unchanged.
-}
readBlocksAt : Doc.Version -> Ref r RichK (List Span) -> Doc doc -> Result EditError (List RichText.Block)
readBlocksAt version (Ref r) doc =
    DocI.readBlocksAt version r.path doc
        |> mapEdit


{-| Apply a formatting mark of kind `type_` (e.g. `"bold"`, `"link"`) with `value`
over the visible character range `[from, to)` of a rich-text ref. Use
`RichText.Flag` for boolean marks (bold/italic/…) and `RichText.Value href` for value
marks (link/color). Only compiles for a `RichK` ref.
-}
mark : Ref r RichK (List Span) -> Int -> Int -> String -> MarkValue -> Doc doc -> Result EditError (Doc doc)
mark (Ref r) from to type_ value doc =
    DocI.mark r.path from to type_ (markPrim value) doc
        |> mapEdit


{-| Clear mark `type_` over the visible range `[from, to)` of a rich-text ref.
-}
unmark : Ref r RichK (List Span) -> Int -> Int -> String -> Doc doc -> Result EditError (Doc doc)
unmark (Ref r) from to type_ doc =
    DocI.clearMark r.path from to type_ doc
        |> mapEdit


{-| Split at a block-relative caret: `charOffset` characters into block `blockIndex`
(0 = the leading block). Inserts a block boundary there.
-}
splitBlock : Ref r RichK (List Span) -> Int -> Int -> Doc doc -> Result EditError (Doc doc)
splitBlock (Ref r) blockIndex charOffset doc =
    DocI.splitBlock r.path blockIndex charOffset doc
        |> mapEdit


{-| Replace the text of **block `blockIndex`** (a minimal diff scoped to that block's
characters), leaving marks and other blocks untouched. This is what an editor should
call per keystroke so typed text lands inside the right block (unlike `setRich`, which
diffs the whole document and can misplace text across a block boundary).
-}
setBlockText : Ref r RichK (List Span) -> Int -> String -> Doc doc -> Result EditError (Doc doc)
setBlockText (Ref r) blockIndex value doc =
    DocI.setBlockText r.path blockIndex value doc
        |> mapEdit


{-| Merge block `blockIndex` into the previous block (tombstones its marker). No-op on
block 0.
-}
mergeBlock : Ref r RichK (List Span) -> Int -> Doc doc -> Result EditError (Doc doc)
mergeBlock (Ref r) blockIndex doc =
    DocI.mergeBlock r.path blockIndex doc
        |> mapEdit


{-| Set (`Just t`) or clear (`Nothing`) the app-defined type of block `blockIndex`.
`type_` is an opaque string the library never interprets.
-}
setBlockType : Ref r RichK (List Span) -> Int -> Maybe String -> Doc doc -> Result EditError (Doc doc)
setBlockType (Ref r) blockIndex maybeType doc =
    DocI.setBlockType r.path blockIndex maybeType doc
        |> mapEdit


{-| Indent (raise depth by one) block `blockIndex`.
-}
indentBlock : Ref r RichK (List Span) -> Int -> Doc doc -> Result EditError (Doc doc)
indentBlock (Ref r) blockIndex doc =
    DocI.indentBlock r.path blockIndex doc
        |> mapEdit


{-| Outdent (lower depth by one, min 0) block `blockIndex`.
-}
outdentBlock : Ref r RichK (List Span) -> Int -> Doc doc -> Result EditError (Doc doc)
outdentBlock (Ref r) blockIndex doc =
    DocI.outdentBlock r.path blockIndex doc
        |> mapEdit


markPrim : MarkValue -> Node.Prim
markPrim value =
    case value of
        RichText.Flag ->
            Node.PBool True

        RichText.Value s ->
            Node.PString s



-- OP-SET (user-defined CRDT) --------------------------------------------------


{-| Add a **contribution** to a user-defined op-set CRDT (`Crdt.opSet`). Only compiles for
an `OpSetK` ref. Pass the same `contribution` schema you gave `opSet` and a contribution
value; it is written under a fresh op-id, so concurrent contributions from any replicas
all survive and the op-set's `fold` sees them all. Returns the contribution's key (its
op-id string), which you can keep to `retract` exactly that contribution later.

    case Edit.contribute board.highScore Crdt.int 42 doc of
        Ok ( key, doc1 ) ->
            -- keep `key` if you'll want to `retract` this contribution later
            ...

        Err _ ->
            ...

-}
contribute : Ref r (OpSetK ck c) a -> Crdt.Crdt ck c s -> c -> Doc doc -> Result EditError ( String, Doc doc )
contribute (Ref r) contributionBundle value doc =
    DocI.contribute r.path (SI.with value contributionBundle.schema) doc
        |> mapEdit


{-| Take back **one specific contribution** you previously added to an op-set, so it no
longer counts toward the folded value.

An op-set is a bag of individual contributions, each written under its own id (see
`Crdt.opSet` / `contribute`). `retract` removes one by that id — the `key` string that
`contribute` handed back — not by its value. So you must **keep the key** from the
`contribute` you want to be able to undo; retracting isn't "remove a `5` from the set",
it's "remove _the contribution I made at that moment_". Retracting a key you don't hold
(or one already retracted) is a harmless no-op.

Under the hood it's a last-write-wins presence flip on that contribution — it converges
like any edit and can itself be re-added, and it's what turns a grow-only op-set into one
you can shrink. After retraction the op-set's `fold` runs over the remaining
contributions only.

    -- a shopping cart as an add-wins set of item ids:
    cart =
        Crdt.opSet { contribution = Crdt.string, fold = dedupe }

    -- add an item, remembering the key so it can be removed later:
    ( appleKey, doc1 ) =
        Edit.contribute cartRef Crdt.string "apple" doc
            |> Result.withDefault ( "", doc )

    -- …the shopper removes it again:
    doc2 =
        Edit.retract cartRef appleKey doc1
            |> Result.withDefault doc1

If you want removal keyed by value rather than by contribution id, keep your own
`Dict value key` alongside the document and look the key up when removing.

-}
retract : Ref r (OpSetK ck c) a -> String -> Doc doc -> Result EditError (Doc doc)
retract (Ref r) contributionKey doc =
    DocI.retract r.path contributionKey doc
        |> mapEdit



-- READING A REF AT A PAST VERSION ---------------------------------------------


{-| Read the value a **single ref** pointed at as of a past `Crdt.Doc.Version` — the
ref-scoped counterpart of `Crdt.Doc.readAt` (which reads the whole document). Use it to
inspect one field's history without decoding the entire document — for example to find
where a past edit landed, or to preview a single field on a timeline.

`Err` if the ref doesn't resolve at that version (its container didn't exist yet) or the
stored data doesn't match the schema — the same conditions as an ordinary edit.

-}
readAt : Doc.Version -> Ref r kind a -> Doc doc -> Result EditError a
readAt version (Ref r) doc =
    DocI.subValueAt version r.schema r.path doc
        |> mapEdit
