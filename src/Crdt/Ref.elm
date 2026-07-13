module Crdt.Ref exposing
    ( Ref
    , at, variantPayload, index, key
    , set, over, increment, switch
    , setRich, mark, unmark, readBlocks
    , setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock
    , append, remove, move
    , setKey, removeKey
    , treeNode, addChild, moveInto, moveBefore, moveAfter, removeNode
    , cursorAt, cursorOffset
    , touched, origins
    , RecordRefs, record, field, aliasedField, build
    , CustomRefs, custom, variant0, variant1, variant2, variant3, catchAll, buildCustom
    )

{-| **Type-safe writes.** A `Ref r kind a` is a write-only optic — a typed pointer
from a document root `r` to an editable spot of type `a`, tagged with the CRDT
`kind` at that spot (`Settable`, `Counter`, `Variants`, …).

Where `Crdt.OpDoc`'s path-addressed edits (`setText (Path.root |> Path.field
"title")`) are checked only at runtime (`PathNotFound` / `WrongNodeType`), a `Ref`
is checked by the **compiler**: `increment` requires a `Counter` ref, `set` a value
of exactly the spot's type, and a field typo is impossible because the name is
written once — in the schema, which hands the refs back.

Build refs with the ref-emitting record builder (`record`/`field`/`build`),
compose them with `at`, and consume them with `set`/`over`/`increment`/`switch`.
Refs compile down to an ordinary `Crdt.Path`, so this is a typed layer over the
existing op-log runtime — no new merge semantics.

    board =
        record Board BoardRefs
            |> field "title" .title S.text
            |> field "votes" .votes S.counter
            |> build

    Ref.set board.refs.title "Trip" doc          -- compile-checked
    -- increment on board.refs.title => a type error, not a runtime WrongNodeType

@docs Ref
@docs at, variantPayload, index, key
@docs set, over, increment, switch
@docs setRich, mark, unmark, readBlocks
@docs setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock
@docs append, remove, move
@docs setKey, removeKey
@docs treeNode, addChild, moveInto, moveBefore, moveAfter, removeNode
@docs cursorAt, cursorOffset
@docs touched, origins
@docs RecordRefs, record, field, aliasedField, build
@docs CustomRefs, custom, variant0, variant1, variant2, variant3, catchAll, buildCustom

-}

import Crdt.Cursor exposing (Cursor)
import Crdt.Id as Id
import Crdt.Node as Node
import Crdt.OpDoc as OpDoc exposing (Error, OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.RichText as RichText exposing (MarkValue, Span)
import Crdt.Schema as S exposing (Crdt)
import Crdt.Schema.Internal as SI


{-| A write-only optic from root `r` to a spot of type `a` with edit-kind `kind`.
Holds the runtime `Path` it compiles to and the sub-schema at the spot (so `set` /
`over` can seed a value and read it back).
-}
type Ref r kind a
    = Ref
        { path : Path
        , schema : Crdt kind a
        }


{-| Compose refs: descend from a spot of type `sub` into a spot inside it. Reads as
navigation in a pipe (structural descent, not monadic sequencing):

    board.refs.status |> at status.refs.done

-}
at : Ref sub innerKind a -> Ref r outerKind sub -> Ref r innerKind a
at (Ref inner) (Ref outer) =
    Ref
        { path = appendPath outer.path inner.path
        , schema = inner.schema
        }


{-| A ref to positional argument `i` of a sum type's `variant`, with the argument's
schema. `set`/`over` through it edit the payload **iff** that variant is currently
active (silent no-op otherwise). The argument index and schema must match the
variant's declaration.

    statusDoneNote : Ref Status S.Settable String
    statusDoneNote =
        variantPayload "done" 0 S.text

-}
variantPayload : String -> Int -> Crdt kind a -> Ref parent kind a
variantPayload variant i schema =
    Ref
        { path =
            Path.root
                |> Path.field (SI.variantArgKey variant)
                |> Path.field (String.fromInt i)
        , schema = schema
        }


{-| A ref to list element at visible index `i`, given the element schema. Compiles
only for a list ref (`ListK _ ek _`), and the element schema's kind `ek` must match
the list's. Because Elm can't recover the element schema from the list ref, you
pass it — usually the same `Crdt` you gave `S.list`.

    Ref.set (todos |> Ref.index 0 todoSchema |> Ref.at todoRefs.done) True doc

The index is a runtime position (like a `Path` index): editing an out-of-range
element is a no-op, same as the path API.

-}
index : Int -> Crdt ek e -> Ref r (S.ListK mv ek e) (List e) -> Ref r ek e
index i elemSchema (Ref container) =
    Ref
        { path = container.path |> Path.index i
        , schema = elemSchema
        }


{-| A ref to dict value at `k`, given the value schema. Compiles only for a dict
ref (`DictK vk v`), and the value schema's kind `vk` must match the dict's. Editing
an absent key is a no-op (use `setKey` to create one).
-}
key : String -> Crdt vk v -> Ref r (S.DictK vk v) dictType -> Ref r vk v
key k valSchema (Ref container) =
    Ref
        { path = container.path |> Path.key k
        , schema = valSchema
        }


appendPath : Path -> Path -> Path
appendPath base sub =
    List.foldl stepInto base (Path.segments sub)


stepInto : Path.Seg -> Path -> Path
stepInto seg path =
    case seg of
        Path.Field n ->
            Path.field n path

        Path.Key n ->
            Path.key n path

        Path.Index i ->
            Path.index i path

        Path.NodeId id ->
            Path.node id path



-- EDITS -----------------------------------------------------------------------


{-| Set the value at a ref. Total across kinds: a present spot is overwritten; a
sum-type payload ref edits the payload iff that variant is active, else it is a
silent no-op. Emits minimal ops; a text spot still merges character-wise.
-}
set : Ref r kind a -> a -> OpDoc doc -> Result Error (OpDoc doc)
set (Ref r) value doc =
    OpDoc.seedNodeAt r.path (S.with value r.schema) doc


{-| Modify the value at a ref: read it, apply `f`, write it back. No-op if the spot
doesn't currently resolve (e.g. an inactive variant payload).
-}
over : Ref r kind a -> (a -> a) -> OpDoc doc -> Result Error (OpDoc doc)
over (Ref r) f doc =
    case OpDoc.subValue r.schema r.path doc of
        Ok current ->
            OpDoc.seedNodeAt r.path (S.with (f current) r.schema) doc

        Err _ ->
            Ok doc


{-| Add `delta` to a counter ref. Only compiles for a `Counter` ref, so
`increment` on a register or text is a **type error**.
-}
increment : Ref r S.Counter Int -> Int -> OpDoc doc -> Result Error (OpDoc doc)
increment (Ref r) delta doc =
    OpDoc.increment (refPath (Ref r)) delta doc


{-| Switch a sum type to a whole new value, selecting the active variant and
seeding its payload. Only compiles for a `Variants` ref. This _changes_ the
variant; `set` on a variant **payload** ref (via `variantPayload`/`at`) edits
within the currently-active variant instead.
-}
switch : Ref r (S.Variants v) v -> v -> OpDoc doc -> Result Error (OpDoc doc)
switch (Ref r) value doc =
    OpDoc.seedNodeAt r.path (S.with value r.schema) doc


{-| Replace the character content of a **rich-text** ref (a minimal text diff, like
`set` on plain text). Marks are preserved and follow the surviving characters. Only
compiles for a `RichK` ref.
-}
setRich : Ref r S.RichK (List Span) -> String -> OpDoc doc -> Result Error (OpDoc doc)
setRich (Ref r) value doc =
    OpDoc.setRichText r.path value doc


{-| Read a rich-text ref as **blocks** (type + depth + spans + marker id), rather
than the flat `List Span` the schema decodes to. Used to resolve a block index to its
marker `OpId` for the block edits. See `Crdt.RichText.toBlocks`.
-}
readBlocks : Ref r S.RichK (List Span) -> OpDoc doc -> Result Error (List RichText.Block)
readBlocks (Ref r) doc =
    OpDoc.readBlocks r.path doc


{-| Apply a formatting mark of kind `type_` (e.g. `"bold"`, `"link"`) with `value`
over the visible character range `[from, to)` of a rich-text ref. Use
`RichText.Flag` for boolean marks (bold/italic/…) and `RichText.Value href` for value
marks (link/color). Only compiles for a `RichK` ref.
-}
mark : Ref r S.RichK (List Span) -> Int -> Int -> String -> MarkValue -> OpDoc doc -> Result Error (OpDoc doc)
mark (Ref r) from to type_ value doc =
    OpDoc.mark r.path from to type_ (markPrim value) doc


{-| Clear mark `type_` over the visible range `[from, to)` of a rich-text ref.
-}
unmark : Ref r S.RichK (List Span) -> Int -> Int -> String -> OpDoc doc -> Result Error (OpDoc doc)
unmark (Ref r) from to type_ doc =
    OpDoc.clearMark r.path from to type_ doc


{-| Split at a block-relative caret: `charOffset` characters into block `blockIndex`
(0 = the leading block). Inserts a block boundary there. See `docs/11`.
-}
splitBlock : Ref r S.RichK (List Span) -> Int -> Int -> OpDoc doc -> Result Error (OpDoc doc)
splitBlock (Ref r) blockIndex charOffset doc =
    OpDoc.splitBlock r.path blockIndex charOffset doc


{-| Replace the text of **block `blockIndex`** (a minimal diff scoped to that block's
characters), leaving marks and other blocks untouched. This is what an editor should
call per keystroke so typed text lands inside the right block (unlike `setRich`, which
diffs the whole document and can misplace text across a block boundary).
-}
setBlockText : Ref r S.RichK (List Span) -> Int -> String -> OpDoc doc -> Result Error (OpDoc doc)
setBlockText (Ref r) blockIndex value doc =
    OpDoc.setBlockText r.path blockIndex value doc


{-| Merge block `blockIndex` into the previous block (tombstones its marker). No-op on
block 0. See `docs/11`.
-}
mergeBlock : Ref r S.RichK (List Span) -> Int -> OpDoc doc -> Result Error (OpDoc doc)
mergeBlock (Ref r) blockIndex doc =
    OpDoc.mergeBlock r.path blockIndex doc


{-| Set (`Just t`) or clear (`Nothing`) the app-defined type of block `blockIndex`.
`type_` is an opaque string the library never interprets.
-}
setBlockType : Ref r S.RichK (List Span) -> Int -> Maybe String -> OpDoc doc -> Result Error (OpDoc doc)
setBlockType (Ref r) blockIndex maybeType doc =
    OpDoc.setBlockType r.path blockIndex maybeType doc


{-| Indent (raise depth by one) block `blockIndex`.
-}
indentBlock : Ref r S.RichK (List Span) -> Int -> OpDoc doc -> Result Error (OpDoc doc)
indentBlock (Ref r) blockIndex doc =
    OpDoc.indentBlock r.path blockIndex doc


{-| Outdent (lower depth by one, min 0) block `blockIndex`.
-}
outdentBlock : Ref r S.RichK (List Span) -> Int -> OpDoc doc -> Result Error (OpDoc doc)
outdentBlock (Ref r) blockIndex doc =
    OpDoc.outdentBlock r.path blockIndex doc


markPrim : MarkValue -> Node.Prim
markPrim value =
    case value of
        RichText.Flag ->
            Node.PBool True

        RichText.Value s ->
            Node.PString s


{-| Did the spot this `ref` points at — or anything under it, or a container it lives in
— change in `diff`, and if so by whom? The typed front door to a merge/ingest `Diff`
(`OpDoc.mergeWithDiff` / `decodeWithDiff`): you ask with the same refs you write through,
and no untyped path is ever exposed. `doc` is the post-merge document (used to resolve
the ref to its identity-addressed location).

    ( doc1, diff ) =
        OpDoc.mergeWithDiff model.doc incoming

    -- only re-read todos if they actually changed (else keep the old reference so a
    -- `lazy` view over them doesn't re-render)
    todos =
        case Ref.touched board.refs.todos doc1 diff of
            Just _ ->
                Ref.readList ...

            Nothing ->
                model.todos

-}
touched : Ref r kind a -> OpDoc doc -> OpDoc.Diff -> Maybe OpDoc.Origin
touched (Ref r) doc diff =
    OpDoc.diffTouches r.path doc diff


{-| Every `Origin` that contributed a change to `diff` — a quick "was there any remote
edit, and whose?" without threading refs.
-}
origins : OpDoc.Diff -> List OpDoc.Origin
origins diff =
    OpDoc.diffOrigins diff


{-| The underlying path of a ref (used internally).
-}
refPath : Ref r kind a -> Path
refPath (Ref r) =
    r.path



-- LIST OPS --------------------------------------------------------------------


{-| Append a value to the end of a list ref, seeded through the element schema.
Compiles for any list (`ListK _ ek e`).
-}
append : Crdt ek e -> e -> Ref r (S.ListK mv ek e) (List e) -> OpDoc doc -> Result Error (OpDoc doc)
append elemSchema value (Ref r) doc =
    OpDoc.listAppend r.path (S.with value elemSchema) doc


{-| Remove the element at visible index `i` from a list ref.
-}
remove : Int -> Ref r (S.ListK mv ek e) (List e) -> OpDoc doc -> Result Error (OpDoc doc)
remove i (Ref r) doc =
    OpDoc.listRemove r.path i doc


{-| Move the element at visible index `from` to index `to`. Compiles **only** for a
`Movable` list (`ListK Movable …`) — calling it on a plain `list` is a type error.
The moved item keeps its identity (nested edits and cursors follow it).
-}
move : Int -> Int -> Ref r (S.ListK S.Movable ek e) (List e) -> OpDoc doc -> Result Error (OpDoc doc)
move from to (Ref r) doc =
    OpDoc.listMove r.path from to doc



-- DICT OPS --------------------------------------------------------------------


{-| Set (create or overwrite) a dict key to a value, seeded through the value
schema. Compiles for a dict ref (`DictK vk v`).
-}
setKey : Crdt vk v -> String -> v -> Ref r (S.DictK vk v) dictType -> OpDoc doc -> Result Error (OpDoc doc)
setKey valSchema k value (Ref r) doc =
    OpDoc.setKey r.path k (S.with value valSchema) doc


{-| Remove a dict key (LWW tombstone).
-}
removeKey : String -> Ref r (S.DictK vk v) dictType -> OpDoc doc -> Result Error (OpDoc doc)
removeKey k (Ref r) doc =
    OpDoc.removeKey r.path k doc



-- TREE OPS --------------------------------------------------------------------
-- Node ids come from reading the tree (`Crdt.Tree.itemId`); pass them to these.


{-| A payload ref for tree node `nodeId`, given the node schema. Compose into it or
`set`/`over` its content: `treeNode id nodeSchema treeRef |> Ref.at nodeRefs.title`.
Editing a node not currently present is a no-op.
-}
treeNode : Id.OpId -> Crdt ek a -> Ref r (S.TreeK ek a) forest -> Ref r ek a
treeNode nodeId nodeSchema (Ref container) =
    Ref
        { path = container.path |> Path.node nodeId
        , schema = nodeSchema
        }


{-| Add a new node (seeded from `value`) as the last child of `parent` (`Nothing` =
a new root) in a tree ref.
-}
addChild : Crdt ek a -> a -> Maybe Id.OpId -> Ref r (S.TreeK ek a) forest -> OpDoc doc -> Result Error (OpDoc doc)
addChild nodeSchema value parent (Ref r) doc =
    OpDoc.treeAddChild r.path parent (S.with value nodeSchema) doc


{-| Re-parent `child` to be the last child of `parent` (`Nothing` = a root).
Cycle-forming moves are skipped (the node stays put), so this always converges.
-}
moveInto : Id.OpId -> Maybe Id.OpId -> Ref r (S.TreeK ek a) forest -> OpDoc doc -> Result Error (OpDoc doc)
moveInto child parent (Ref r) doc =
    OpDoc.treeMoveInto r.path child parent doc


{-| Move `child` to sit immediately before `sibling` (under the sibling's parent).
-}
moveBefore : Id.OpId -> Id.OpId -> Ref r (S.TreeK ek a) forest -> OpDoc doc -> Result Error (OpDoc doc)
moveBefore child sibling (Ref r) doc =
    OpDoc.treeMoveBefore r.path child sibling doc


{-| Move `child` to sit immediately after `sibling` (under the sibling's parent).
-}
moveAfter : Id.OpId -> Id.OpId -> Ref r (S.TreeK ek a) forest -> OpDoc doc -> Result Error (OpDoc doc)
moveAfter child sibling (Ref r) doc =
    OpDoc.treeMoveAfter r.path child sibling doc


{-| Remove a node and its subtree from a tree ref.
-}
removeNode : Id.OpId -> Ref r (S.TreeK ek a) forest -> OpDoc doc -> Result Error (OpDoc doc)
removeNode child (Ref r) doc =
    OpDoc.treeRemove r.path child doc



-- CURSORS ---------------------------------------------------------------------


{-| A stable cursor at visible `offset` in a text ref (`Settable String` backed by
`Txt`). Wraps `OpDoc.cursorAt` with a typed ref instead of a `Path`.
-}
cursorAt : Ref r S.Settable String -> Int -> OpDoc doc -> Result Error Cursor
cursorAt (Ref r) offset doc =
    OpDoc.cursorAt r.path offset doc


{-| Resolve a cursor back to its current visible offset (see `OpDoc.cursorOffset`).
-}
cursorOffset : Cursor -> OpDoc doc -> Maybe Int
cursorOffset =
    OpDoc.cursorOffset



-- RECORD BUILDER (emits refs) -------------------------------------------------


{-| Convenience alias: the `{ schema, refs }` a ref-emitting builder returns.
-}
type alias RecordRefs a refs =
    { schema : Crdt S.Nested a, refs : refs }


{-| In-progress ref-emitting record schema: the underlying `SI.RecordBuilder` plus
a partially-applied `…Refs` assembler receiving each field's `Ref` in turn.
-}
type RecordRefsBuilder r full a refs
    = RecordRefsBuilder
        { builder : SI.RecordBuilder full a
        , refs : refs
        }


{-| Begin a ref-emitting record from **two** constructors: the value constructor
(assembles the decoded `a`, as in `SI.record`) and the refs assembler (assembles
your `…Refs` record from the field refs, in declaration order). Each `field` feeds
the field's value to the first and its `Ref` to the second.

    record Board BoardRefs
        |> field "title" .title S.text
        |> ...
        |> build

-}
record : (a -> b) -> refsAssembler -> RecordRefsBuilder r full (a -> b) refsAssembler
record ctor refsAssembler =
    RecordRefsBuilder
        { builder = SI.record ctor
        , refs = refsAssembler
        }


{-| Add a field: key, getter, kinded schema. Threads a `Ref r kind f` bound to the
field into the refs assembler.
-}
field :
    String
    -> (full -> f)
    -> Crdt kind f
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
field name getter schema (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.field name getter schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = schema })
        }


{-| Add a field that also reads from **older names** (`aliases`) when `name` is absent —
a schema-evolution rename (see `docs/13`). Reads prefer `name`, then each alias in order;
writes and the field's `Ref` always target `name`, so once written the old key becomes an
inert extra key. Peers on the old and new schema converge on the same document and each
reads it through its own names.

    record Todo TodoRefs
        |> aliasedField "done" [ "complete" ] .done S.bool
        |> ...

-}
aliasedField :
    String
    -> List String
    -> (full -> f)
    -> Crdt kind f
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
aliasedField name aliases getter schema (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.aliasedField name aliases getter schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = schema })
        }


{-| Finish: returns `{ schema, refs }`.
-}
build : RecordRefsBuilder r a a refs -> RecordRefs a refs
build (RecordRefsBuilder rb) =
    { schema = SI.build rb.builder
    , refs = rb.refs
    }



-- CUSTOM (SUM) BUILDER (emits refs) -------------------------------------------


{-| Convenience alias: the `{ schema, refs }` a ref-emitting sum builder returns.
-}
type alias CustomRefs a refs =
    { schema : Crdt (S.Variants a) a, refs : refs }


{-| In-progress ref-emitting custom (sum) schema: the underlying `SI.CustomBuilder`
plus a partially-applied `…Refs` assembler receiving each variant's payload ref(s).
-}
type CustomRefsBuilder value match refs
    = CustomRefsBuilder
        { builder : SI.CustomBuilder match value
        , refs : refs
        }


{-| Begin a ref-emitting sum type from the dispatcher (as `SI.custom`) and a refs
assembler (assembles your payload-`…Refs` record from the per-variant payload refs,
in declaration order; nullary variants contribute nothing).

    status =
        custom
            (\planning active archived v ->
                case v of
                    Planning ->
                        planning

                    Active ->
                        active

                    Archived s ->
                        archived s
            )
            StatusRefs
            |> variant0 "planning" Planning
            |> variant0 "active" Active
            |> variant1 "archived" Archived S.text
            |> buildCustom

-}
custom : match -> refsAssembler -> CustomRefsBuilder value match refsAssembler
custom match refsAssembler =
    CustomRefsBuilder
        { builder = SI.custom match
        , refs = refsAssembler
        }


{-| A nullary variant — no payload, so it contributes no ref.
-}
variant0 :
    String
    -> value
    -> CustomRefsBuilder value (SI.VariantSeed -> b) refs
    -> CustomRefsBuilder value b refs
variant0 name ctorValue (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant0 name ctorValue cb.builder
        , refs = cb.refs
        }


{-| A one-argument variant. Threads a payload ref (rooted at the sum type) into the
refs assembler; `set`/`over` through it edit iff this variant is active.
-}
variant1 :
    String
    -> (t1 -> value)
    -> Crdt k1 t1
    -> CustomRefsBuilder value ((t1 -> SI.VariantSeed) -> b) (Ref value k1 t1 -> rest)
    -> CustomRefsBuilder value b rest
variant1 name ctor schema (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant1 name ctor schema cb.builder
        , refs = cb.refs (variantPayload name 0 schema)
        }


{-| A two-argument variant. Threads two payload refs.
-}
variant2 :
    String
    -> (t1 -> t2 -> value)
    -> Crdt k1 t1
    -> Crdt k2 t2
    -> CustomRefsBuilder value ((t1 -> t2 -> SI.VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> rest)
    -> CustomRefsBuilder value b rest
variant2 name ctor s1 s2 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant2 name ctor s1 s2 cb.builder
        , refs = cb.refs (variantPayload name 0 s1) (variantPayload name 1 s2)
        }


{-| A three-argument variant. Threads three payload refs.
-}
variant3 :
    String
    -> (t1 -> t2 -> t3 -> value)
    -> Crdt k1 t1
    -> Crdt k2 t2
    -> Crdt k3 t3
    -> CustomRefsBuilder value ((t1 -> t2 -> t3 -> SI.VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> Ref value k3 t3 -> rest)
    -> CustomRefsBuilder value b rest
variant3 name ctor s1 s2 s3 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant3 name ctor s1 s2 s3 cb.builder
        , refs = cb.refs (variantPayload name 0 s1) (variantPayload name 1 s2) (variantPayload name 2 s3)
        }


{-| A **catch-all variant** for schema evolution: unknown `$tag`s (a variant a newer
peer added) decode to `ctor tag` instead of failing the read (see `docs/13`). Consumes
one dispatcher handler (`String -> SI.VariantSeed`) like a variant but threads **no**
payload ref — the raw tag isn't an editable payload — so the refs assembler is unchanged.

    custom (\active done unknown v -> ...) Refs
        |> variant0 "active" Active
        |> variant1 "done" Done S.text
        |> catchAll "unknown" Unknown
        |> buildCustom

-}
catchAll :
    String
    -> (String -> value)
    -> CustomRefsBuilder value ((String -> SI.VariantSeed) -> b) refs
    -> CustomRefsBuilder value b refs
catchAll placeholderTag ctor (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.catchAll placeholderTag ctor cb.builder
        , refs = cb.refs
        }


{-| Finish a ref-emitting sum type: returns `{ schema, refs }`.
-}
buildCustom : CustomRefsBuilder value (value -> SI.VariantSeed) refs -> CustomRefs value refs
buildCustom (CustomRefsBuilder cb) =
    { schema = SI.buildCustom cb.builder
    , refs = cb.refs
    }
