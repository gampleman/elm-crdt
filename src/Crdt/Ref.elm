module Crdt.Ref exposing
    ( Ref
    , at, variantPayload, index, key
    , set, over, increment, switch
    , append, remove, move
    , setKey, removeKey
    , cursorAt, cursorOffset
    , RecordRefs, record, field, build
    , CustomRefs, custom, variant0, variant1, variant2, variant3, buildCustom
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
@docs append, remove, move
@docs setKey, removeKey
@docs cursorAt, cursorOffset
@docs RecordRefs, record, field, build
@docs CustomRefs, custom, variant0, variant1, variant2, variant3, buildCustom

-}

import Crdt.Cursor exposing (Cursor)
import Crdt.OpDoc as OpDoc exposing (Error, OpDoc)
import Crdt.Path as Path exposing (Path)
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


{-| Finish a ref-emitting sum type: returns `{ schema, refs }`.
-}
buildCustom : CustomRefsBuilder value (value -> SI.VariantSeed) refs -> CustomRefs value refs
buildCustom (CustomRefsBuilder cb) =
    { schema = SI.buildCustom cb.builder
    , refs = cb.refs
    }
