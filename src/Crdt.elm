module Crdt exposing
    ( init, read, readAt
    , Schema
    , int, float, string, bool, counter, register
    , text, richText
    , list, movableList, dict, tree
    , opSet
    , Ref
    , RecordRefs, record, field, build
    , CustomRefs, custom, variant0, variant1, variant2, variant3, buildCustom
    , optional, withDefault, map
    , aliasedField, catchAll
    , Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
    , VariantSeed
    , set, over, increment, switch
    , append, remove, move
    , setKey, removeKey
    , treeNode, addChild, moveInto, moveBefore, moveAfter, removeNode
    , setRich, mark, unmark
    , readBlocks, setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock
    , contribute, retract
    , at, index, key
    , cursorAt, cursorOffset
    , touched, origins
    , EditError(..), editErrorToString
    , ReadError, readErrorToString
    )

{-| **Describe your collaborative document, and edit it type-safely.** This is the main
module: you meet it first and reach for it most.

Working with a document is two activities, and they fit together here:

1.  **Describe its shape once, as a schema.** A `Schema` value says both what Elm type a
    piece of data reads as and how concurrent edits to it merge — the collaborative
    analogue of a JSON decoder. You build it from **leaves** (`text`, `counter`, `int`,
    …) and **containers** (`list`, `dict`, `tree`, …), assembled into **records**
    (`record`/`field`/`build`) and **custom types** (`custom`/`variant…`).

2.  **Edit through refs.** The record and custom builders hand you back typed **`Ref`s**
    alongside the schema — one per field or payload. A `Ref` is a pointer to one editable
    spot, and you edit by passing it to a function like `set` or `increment`.

**Why not just edit the plain record and hand it back?** Because you can't recover an
edit's _intent_ from before/after values, and a CRDT needs that intent to merge. If a
list goes from `[ a, b ]` to `[ a, c, b ]`, did you insert `c`, or replace `b` with `c`
and append `b`? Concurrent replicas resolve those two stories differently, so a value
diff would guess — and peers could guess differently and diverge. If a counter reads `5`
both before and after, was there no edit, or a `+3` and a `-3` that a teammate's
concurrent `+1` must still be added to? Only the _operation_ (`insert c after a`,
`increment by 3`) carries what merge needs. A `Ref` names the spot and the edit function
names the operation, so the library records intent directly instead of reverse-engineering
it — which is also why there is no `a -> Doc a` "save" function.

Threading edits through refs also makes them **safe**: each ref carries the kind of data
it points at, so the compiler rejects a nonsensical edit — `increment` on text, `move` on
a non-movable list, `set` with the wrong value type — and a field name, written once in
the schema, can never be mistyped. Mistakes are compile errors, not runtime surprises.

    import Crdt

    type alias Board =
        { title : String, votes : Int }

    type alias BoardRefs =
        { title : Crdt.Ref Board Crdt.Settable String
        , votes : Crdt.Ref Board Crdt.Counter Int
        }

    board : Crdt.RecordRefs Board BoardRefs
    board =
        Crdt.record Board BoardRefs
            |> Crdt.field "title" .title Crdt.text
            |> Crdt.field "votes" .votes Crdt.counter
            |> Crdt.build

    -- `board.schema` creates the document; `board.refs` are your edit handles:
    doc =
        Crdt.init (Crdt.Id.replica "alice") board.schema

    doc1 =
        Crdt.set board.refs.title "Trip plan" doc

    doc2 =
        Crdt.increment board.refs.votes 1 doc1

    -- Crdt.increment board.refs.title  =>  a compile error

The one thing to understand about **choosing a leaf** is how it merges when two people
edit it at once: an `int`/`string`/`bool` is _last-write-wins_ (the latest edit wins), a
`counter` _sums_ concurrent increments, and `text`/`richText` merge _character by
character_ so two people typing never clobber each other.

Every editing function returns a `Result EditError (Doc doc)`; the compile-time checks
make failures rare, and you can usually `Result.withDefault` past one.


# Creating and reading a document

`init` turns a schema into a live document for one replica; `read` gets the current typed
value back out; `readAt` reads it as it stood at a past `Crdt.Doc.Version`. Everything
else you do with a document — syncing, history, undo — lives in `Crdt.Doc`.

@docs init, read, readAt


# Describing the shape: the schema

@docs Schema


# Simple values

The building blocks. Registers (`int`/`float`/`string`/`bool`) are last-write-wins — a
concurrent edit is resolved by keeping the most recent write. `counter` instead sums
concurrent changes, and `register` stores a value of **any** type as a single
last-write-wins cell.

Registers are the simplest thing to make collaborative, and a good way to adopt CRDTs
incrementally: take an existing model and, field by field, describe each as a register in
a `record`. Reach for `register` when a field's type isn't one of the built-in leaves (a
custom enum, a tuple, a small record you're happy to treat as one indivisible unit).

@docs int, float, string, bool, counter, register


# Collaborative text

@docs text, richText


# Collections

@docs list, movableList, dict, tree


# Defining your own CRDT

@docs opSet


# The ref type

@docs Ref


# Building a record

@docs RecordRefs, record, field, build


# Building a custom type

@docs CustomRefs, custom, variant0, variant1, variant2, variant3, buildCustom


# Evolving a schema over time

Once you have a schema, releases will change it — but with local-first sync, documents
written by **every past version** are still out there, and peers on old and new schemas
edit the same document at once. So a schema change can't be a one-shot migration: a newer
schema must read older documents, an older schema must survive newer ones, and both must
converge. These combinators make a read **tolerant** of a shape it didn't expect, rather
than failing.

The safe, non-breaking changes and the tool for each:

  - **Add a field** — wrap it in `optional` (reads `Nothing` when missing) or
    `withDefault` (reads a default when missing), so old documents that lack it still
    read. A plain required field would error on them.
  - **Rename a field** — `aliasedField` reads the old name too, so peers on either name
    share one document.
  - **Add a variant to a custom type** — declare a `catchAll` so an older peer reads a
    newer variant's tag as a fallback (preserving it) instead of erroring.
  - **Change a value's representation** (rename an enum, `Int` ↔ `String`) — `map` it in
    both directions.

Removing a field or variant, or changing a field's merge semantics (e.g. `counter` →
register), are **not** safe to do in place — old peers keep writing the old shape. Treat
those as a new field alongside the old one.

@docs optional, withDefault, map
@docs aliasedField, catchAll


# Edit-capability markers

Each schema is tagged with a marker describing how its value may be **edited** (a counter
can be `increment`ed, text typed into, and so on). You rarely write these yourself — they
appear in the `Ref` types the builders infer — but they are what lets the compiler reject
a nonsensical edit. They never affect how data reads or merges.

@docs Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
@docs VariantSeed


# Editing simple values

@docs set, over, increment, switch


# Editing lists

New elements are added at the end with `append`. **To insert elsewhere**, use a
`movableList` and follow the `append` with a `move` to the target index — the element is
created at the end, then slid into place, and because a `movableList` element keeps its
identity the move survives concurrent edits. (A plain `list` has no `move`, so it only
grows at the end; reach for `movableList` whenever order is something users control.)

    -- insert `todo` as the new element at index 2 of a movable list:
    doc
        |> Crdt.append todoSchema todo todosRef
        |> Result.andThen
            (\d ->
                Crdt.move (List.length (currentTodos d) - 1) 2 todosRef d
            )

@docs append, remove, move


# Editing dictionaries

@docs setKey, removeKey


# Editing trees

@docs treeNode, addChild, moveInto, moveBefore, moveAfter, removeNode


# Editing text and rich text

@docs setRich, mark, unmark
@docs readBlocks, setBlockText, splitBlock, mergeBlock, setBlockType, indentBlock, outdentBlock


# Editing your own CRDT types

@docs contribute, retract


# Pointing into parts of the document

@docs at, index, key


# Cursors that survive concurrent edits

@docs cursorAt, cursorOffset


# Reacting to what a merge changed

@docs touched, origins


# Errors

@docs EditError, editErrorToString
@docs ReadError, readErrorToString

-}

import Crdt.Cursor exposing (Cursor)
import Crdt.Doc as Doc exposing (Diff, Doc, Origin)
import Crdt.Doc.Internal as DocI
import Crdt.Id as Id
import Crdt.Node as Node
import Crdt.Path as Path exposing (Path)
import Crdt.RichText as RichText exposing (MarkValue, Span)
import Crdt.Schema.Internal as SI
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE



-- CREATING AND READING A DOCUMENT ---------------------------------------------


{-| Create a fresh, empty document from a schema, owned by a replica. Give each
participant (each browser tab, device, or server) a **distinct** `Crdt.Id.replica` so
their edits never collide.

    doc =
        Crdt.init (Crdt.Id.replica "alice-laptop") board.schema

-}
init : Id.ReplicaId -> Schema kind a -> Doc a
init =
    DocI.init


{-| Read the document's current value through its schema. `Err` only if the stored data
can't be interpreted by the schema (a genuinely corrupt document); render the error with
`readErrorToString`.
-}
read : Doc a -> Result ReadError a
read =
    DocI.read


{-| Read the value as it stood at a past `Crdt.Doc.Version` — true time-travel, without
disturbing the live document. Great for previews and history views.
-}
readAt : Doc.Version -> Doc a -> Result ReadError a
readAt =
    DocI.readAt



-- SCHEMA: DESCRIBING THE DOCUMENT ---------------------------------------------


{-| A description of a piece of collaborative data — what Elm type it reads as, and how
concurrent edits to it merge. Compose small `Schema` values (leaves and containers) into
the shape of your whole document, the way you compose `elm/json` decoders. The `kind`
tag records how the value may be edited (see the edit-capability markers); `a` is the Elm
type it reads as. Opaque.
-}
type alias Schema kind a =
    SI.Crdt kind a


{-| Kind marker: an LWW register leaf; supports `set`.
-}
type alias Settable =
    SI.Settable


{-| Kind marker: a PN-counter; supports `increment`.
-}
type alias Counter =
    SI.Counter


{-| Kind marker: a record or map; edited by descending into fields/keys.
-}
type alias Nested =
    SI.Nested


{-| Kind marker: a sum type over `a`; supports variant switching + payload refs.
-}
type alias Variants a =
    SI.Variants a


{-| Kind marker: a list of `a` with element kind `ek` and movability `mv`.
-}
type alias ListK mv ek a =
    SI.ListK mv ek a


{-| Movability marker for a plain `list` (no reordering).
-}
type alias Fixed =
    SI.Fixed


{-| Movability marker for a `movableList` (supports `move`).
-}
type alias Movable =
    SI.Movable


{-| Kind marker: a `dict` of `a` with value kind `vk`.
-}
type alias DictK vk a =
    SI.DictK vk a


{-| Kind marker: a movable `tree` of `a` with node kind `ek`.
-}
type alias TreeK ek a =
    SI.TreeK ek a


{-| Kind marker: rich (formatted) text; supports text edits + `mark`/`unmark`.
-}
type alias RichK =
    SI.RichK


{-| Kind marker: a user-defined **op-set** CRDT over contribution type `c` (see `opSet`);
edited only via `contribute`/`retract`.
-}
type alias OpSetK c =
    SI.OpSetK c


{-| The opaque "make a fresh variant payload" value the choice-type builders
(`variant0`/`variant1`/… and `catchAll`) produce and thread for you. You never build one
by hand; it only shows up in the inferred types of those builders.
-}
type alias VariantSeed =
    SI.VariantSeed


{-| An integer LWW register.
-}
int : Schema Settable Int
int =
    SI.int


{-| A float LWW register.
-}
float : Schema Settable Float
float =
    SI.float


{-| A string LWW register. For collaborative editing use `text`.
-}
string : Schema Settable String
string =
    SI.string


{-| A boolean LWW register.
-}
bool : Schema Settable Bool
bool =
    SI.bool


{-| Collaborative text, read as a `String`, backed by an RGA so concurrent edits merge
character-wise.
-}
text : Schema Settable String
text =
    SI.text


{-| A counter — an integer edited only with `increment` (add or subtract a delta), read
as its running total.

Reach for it instead of `int` when the value is a **running tally that several people
change at once** — a vote count, a stock level, a like button. Concurrent increments
**add up**: if two people each `increment` by 1 at the same time, the total goes up by 2.
An `int` register is last-write-wins, so under the same race one of the two `+1`s would be
silently lost — fine for a value someone _sets_ (a title, a status), wrong for one that
accumulates. If you never have concurrent changes, or you want to set an exact number
rather than nudge it, use `int`.

(It's a [PN-counter](https://crdt.tech/counters) — "positive-negative", since deltas can
go either way.)

-}
counter : Schema Counter Int
counter =
    SI.counter


{-| A last-write-wins register holding a value of **any** type, stored as JSON. Give a
`default` (what a fresh document reads before anyone writes the field), a JSON encoder,
and a decoder:

    import Json.Decode as D
    import Json.Encode as E

    type Priority
        = Low
        | High

    priority : Schema Settable Priority
    priority =
        Crdt.register Low encodePriority priorityDecoder

The whole value is one cell: two people editing it concurrently don't merge field by
field — the most recent write wins wholesale. Use it for a field whose type isn't a
built-in leaf, or that you're happy to treat as one indivisible unit; reach for `record`,
`list`, `dict` or `text` when you want the parts to merge independently.

-}
register : a -> (a -> JE.Value) -> JD.Decoder a -> Schema Settable a
register =
    SI.register


{-| Make a field's schema **optional** for schema evolution: it reads `Nothing` when the
field is absent (a document written before the field existed) instead of failing, and
`Just v` when present. Seeding `Nothing` writes no value.

    |> Crdt.field "priority" .priority (Crdt.optional prioritySchema)

-}
optional : Schema kind a -> Schema kind (Maybe a)
optional =
    SI.optional


{-| Give a field's schema a **default** read when the field is absent: it reads `default`
instead of failing, without changing the value type. The real value is minted on first
write.

    |> Crdt.field "priority" .priority (Crdt.withDefault Medium prioritySchema)

It also absorbs the other "the shape evolved" case: if `default` wraps a **custom type**
and the stored value is a variant this schema doesn't know (a newer peer added it), the
read is `default` rather than an error — the collapse-to-a-usable-value strategy for
forward compatibility. (`catchAll` is the alternative that keeps the unknown tag instead.
Note that re-saving a collapsed value with `switch` loses the original tag, whereas simply
holding and syncing it preserves it.) Genuinely malformed data still errors.

-}
withDefault : a -> Schema kind a -> Schema kind a
withDefault =
    SI.withDefault


{-| Transform the value a schema reads and seeds, in **both** directions (`to` on read,
`from` on seed) — for evolving a value's shape (re-spell an enum, int↔string) while the
stored data is unchanged. Both directions are required so seeding round-trips.

    Crdt.map statusFromString statusToString Crdt.string

-}
map : (a -> b) -> (b -> a) -> Schema kind a -> Schema kind b
map =
    SI.map


{-| An ordered list of `a`, backed by an RGA.
-}
list : Schema ek a -> Schema (ListK Fixed ek a) (List a)
list =
    SI.list


{-| A reorderable list of `a` (elements can be moved with `move`, keeping identity).
-}
movableList : Schema ek a -> Schema (ListK Movable ek a) (List a)
movableList =
    SI.movableList


{-| A dictionary of string keys to `a`. Key presence is LWW.
-}
dict : Schema vk a -> Schema (DictK vk a) (Dict String a)
dict =
    SI.dict


{-| A movable **tree** of `a`: hierarchical, re-parentable, sibling-ordered data. Reads
as a `Crdt.Tree.Forest a`.
-}
tree : Schema ek a -> Schema (TreeK ek a) (Tree.Forest a)
tree =
    SI.tree


{-| Collaborative **rich (formatted) text**: a character sequence plus Peritext marks.
Reads as a list of `Crdt.RichText.Span`s; edit with text edits plus `mark`/`unmark`.
-}
richText : Schema RichK (List RichText.Span)
richText =
    SI.richText


{-| Define your **own CRDT type** as an operation-based set: contributions (each written
under its own op-id via `contribute`) folded into a value at read. Convergence is free
(merge unions contributions); the semantics is your `fold`, which must be a pure,
order-independent function of the contribution set.

    -- a grow-only max register
    maxRegister : Crdt.Schema (Crdt.OpSetK Int) Int
    maxRegister =
        Crdt.opSet { contribution = Crdt.int, fold = List.maximum >> Maybe.withDefault 0 }

-}
opSet : { contribution : Schema ck c, fold : List c -> a } -> Schema (OpSetK c) a
opSet =
    SI.opSet



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


{-| Why reading a document through its schema failed — returned by `read` and `readAt`.
Unlike an `EditError` this is not a race: it means the stored data doesn't match the
schema (a genuinely corrupt or incompatible document). Opaque; render with
`readErrorToString`.
-}
type alias ReadError =
    SI.Error


{-| A human-readable description of a `ReadError`.
-}
readErrorToString : ReadError -> String
readErrorToString =
    SI.errorToString


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


{-| A typed pointer to one editable spot inside a document — a field, a list element, a
dictionary value, a variant's payload. You get refs from the `record` and `custom`
builders (as `board.refs.title`) and pass them to the edit functions (`set`, `increment`,
…) to say _where_ to edit.

The three type parameters are what make edits safe, and you rarely write them by hand:

  - `r` is the type at the **root** the ref was built from (e.g. your `Board` record), so
    refs from different documents can't be mixed up;
  - `kind` is **how the spot may be edited** — `Settable`, `Counter`, `RichK`, and so on
    — which is what lets the compiler accept `increment` only on a `Counter` ref;
  - `a` is the value the spot **reads as**.

Opaque: build refs with the builders, compose them with `at`/`index`/`key`, and edit
through them.

-}
type Ref r kind a
    = Ref
        { path : Path
        , schema : Schema kind a
        }


{-| Compose refs: descend from a spot of type `sub` into a spot inside it. Reads as
navigation in a pipe:

    board.refs.status |> at status.refs.done

-}
at : Ref sub innerKind a -> Ref r outerKind sub -> Ref r innerKind a
at (Ref inner) (Ref outer) =
    Ref
        { path = appendPath outer.path inner.path
        , schema = inner.schema
        }



{- A ref to positional argument `i` of a custom type's `variant`, with the argument's
   schema. `set`/`over` through it edit the payload iff that variant is currently active
   (silent no-op otherwise). Internal: the `variant1`/`variant2`/`variant3` builders call
   this to construct the payload refs they hand back in the `CustomRefs`, so the argument
   index and schema always match the declaration. Not exposed — users get these refs from
   the builder, never build them by hand.
-}


variantPayload : String -> Int -> Schema kind a -> Ref parent kind a
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
pass it — usually the same `Schema` you gave `Crdt.list`.

    Crdt.set
        (todos
            |> Crdt.index 0 todoSchema
            |> Crdt.at todoRefs.done
        )
        True
        doc

The index is a runtime position: editing an out-of-range element is a no-op.

-}
index : Int -> Schema ek e -> Ref r (ListK mv ek e) (List e) -> Ref r ek e
index i elemSchema (Ref container) =
    Ref
        { path = container.path |> Path.index i
        , schema = elemSchema
        }


{-| A ref to dict value at `k`, given the value schema. Compiles only for a dict
ref (`DictK vk v`), and the value schema's kind `vk` must match the dict's. Editing
an absent key is a no-op (use `setKey` to create one).
-}
key : String -> Schema vk v -> Ref r (DictK vk v) dictType -> Ref r vk v
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
    DocI.increment (refPath (Ref r)) delta doc
        |> mapEdit


{-| Change which **variant** a custom-type value is, seeding the new variant's payload
from the value you pass. Only compiles for a `Variants` ref (one built by `custom`).

The distinction to keep straight: `switch` moves between variants; a payload `Ref` (one
the `custom` builder handed you, reached via `at`) edits _inside_ the variant that's
currently active. Given the `Status` type from `custom`:

    -- switch the whole status from whatever it is to `Archived "old project"`:
    doc |> Crdt.switch statusRef (Archived "old project")

    -- vs. edit the reason text *within* an already-Archived status (a no-op if the
    -- status is currently Planning or Active):
    doc |> Crdt.set (statusRef |> Crdt.at status.refs.archivedReason) "new reason"

So reach for `switch` when the case changes (`Active` → `Archived`), and a payload ref
when the case stays the same but its contents change. Which variant is active merges
last-write-wins.

-}
switch : Ref r (Variants v) v -> v -> Doc doc -> Result EditError (Doc doc)
switch (Ref r) value doc =
    DocI.seedNodeAt r.path (SI.with value r.schema) doc
        |> mapEdit


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
marker `OpId` for the block edits. See `Crdt.RichText.Block`.
-}
readBlocks : Ref r RichK (List Span) -> Doc doc -> Result EditError (List RichText.Block)
readBlocks (Ref r) doc =
    DocI.readBlocks r.path doc
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


{-| Did the spot this `ref` points at — or anything under it, or a container it lives in
— change in `diff`, and if so who changed it? You ask with the same refs you write
through; the `Origin` in a `Just` tells you whether the change was `Local` or from a
particular remote replica. `doc` is the post-merge document.

Use it to _react_ to an incoming change (your `view` already reflects the new state):
for instance, to flash a highlight on a field a collaborator just touched.

    ( doc1, diff ) =
        Doc.mergeWithDiff model.doc incoming

    highlightTitle =
        case Crdt.touched board.refs.title doc1 diff of
            Just origin ->
                not (Doc.isLocal origin)

            Nothing ->
                False

-}
touched : Ref r kind a -> Doc doc -> Diff -> Maybe Origin
touched (Ref r) doc diff =
    DocI.diffTouches r.path doc diff


{-| Every `Origin` that contributed a change to `diff` — a quick "was there any remote
edit, and whose?" without threading refs.
-}
origins : Diff -> List Origin
origins diff =
    DocI.diffOrigins diff


{-| The underlying path of a ref (used internally).
-}
refPath : Ref r kind a -> Path
refPath (Ref r) =
    r.path



-- LIST OPS --------------------------------------------------------------------


{-| Append a value to the end of a list ref, seeded through the element schema.
Compiles for any list (`ListK _ ek e`).
-}
append : Schema ek e -> e -> Ref r (ListK mv ek e) (List e) -> Doc doc -> Result EditError (Doc doc)
append elemSchema value (Ref r) doc =
    DocI.listAppend r.path (SI.with value elemSchema) doc
        |> mapEdit


{-| Remove the element at visible index `i` from a list ref.
-}
remove : Int -> Ref r (ListK mv ek e) (List e) -> Doc doc -> Result EditError (Doc doc)
remove i (Ref r) doc =
    DocI.listRemove r.path i doc
        |> mapEdit


{-| Move the element at visible index `from` to index `to`. Compiles **only** for a
`Movable` list (`ListK Movable …`) — calling it on a plain `list` is a type error.
The moved item keeps its identity (nested edits and cursors follow it).
-}
move : Int -> Int -> Ref r (ListK Movable ek e) (List e) -> Doc doc -> Result EditError (Doc doc)
move from to (Ref r) doc =
    DocI.listMove r.path from to doc
        |> mapEdit



-- DICT OPS --------------------------------------------------------------------


{-| Set (create or overwrite) a dict key to a value, seeded through the value
schema. Compiles for a dict ref (`DictK vk v`).
-}
setKey : Schema vk v -> String -> v -> Ref r (DictK vk v) dictType -> Doc doc -> Result EditError (Doc doc)
setKey valSchema k value (Ref r) doc =
    DocI.setKey r.path k (SI.with value valSchema) doc
        |> mapEdit


{-| Remove a dict key (LWW tombstone).
-}
removeKey : String -> Ref r (DictK vk v) dictType -> Doc doc -> Result EditError (Doc doc)
removeKey k (Ref r) doc =
    DocI.removeKey r.path k doc
        |> mapEdit



-- OP-SET (user-defined CRDT) --------------------------------------------------


{-| Add a **contribution** to a user-defined op-set CRDT (`opSet`). Only compiles for an
`OpSetK` ref. Pass the same `contribution` schema you gave `opSet` and a contribution
value; it is written under a fresh op-id, so concurrent contributions from any replicas
all survive and the op-set's `fold` sees them all. Returns the contribution's key (its
op-id string), which you can keep to `retract` exactly that contribution later.

    ( key, doc1 ) =
        Crdt.contribute Crdt.int 42 board.refs.highScore doc

-}
contribute : Schema ck c -> c -> Ref r (OpSetK c) a -> Doc doc -> Result EditError ( String, Doc doc )
contribute contributionSchema value (Ref r) doc =
    DocI.contribute r.path (SI.with value contributionSchema) doc
        |> mapEdit


{-| Take back **one specific contribution** you previously added to an op-set, so it no
longer counts toward the folded value.

An op-set is a bag of individual contributions, each written under its own id (see
`opSet` / `contribute`). `retract` removes one by that id — the `key` string that
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
        Crdt.contribute Crdt.string "apple" cartRef doc

    -- …the shopper removes it again:
    doc2 =
        Crdt.retract appleKey cartRef doc1
            |> Result.withDefault doc1

If you want removal keyed by value rather than by contribution id, keep your own
`Dict value key` alongside the document and look the key up when removing.

-}
retract : String -> Ref r (OpSetK c) a -> Doc doc -> Result EditError (Doc doc)
retract contributionKey (Ref r) doc =
    DocI.retract r.path contributionKey doc
        |> mapEdit



-- TREE OPS --------------------------------------------------------------------
-- Node ids come from reading the tree (`Crdt.Tree.itemId`); pass them to these.


{-| A payload ref for tree node `nodeId`, given the node schema. Compose into it or
`set`/`over` its content: `treeNode id nodeSchema treeRef |> Ref.at nodeRefs.title`.
Editing a node not currently present is a no-op.
-}
treeNode : Id.OpId -> Schema ek a -> Ref r (TreeK ek a) forest -> Ref r ek a
treeNode nodeId nodeSchema (Ref container) =
    Ref
        { path = container.path |> Path.node nodeId
        , schema = nodeSchema
        }


{-| Add a new node (seeded from `value`) as the last child of `parent` (`Nothing` =
a new root) in a tree ref.
-}
addChild : Schema ek a -> a -> Maybe Id.OpId -> Ref r (TreeK ek a) forest -> Doc doc -> Result EditError (Doc doc)
addChild nodeSchema value parent (Ref r) doc =
    DocI.treeAddChild r.path parent (SI.with value nodeSchema) doc
        |> mapEdit


{-| Re-parent `child` to be the last child of `parent` (`Nothing` = a root).
Cycle-forming moves are skipped (the node stays put), so this always converges.
-}
moveInto : Id.OpId -> Maybe Id.OpId -> Ref r (TreeK ek a) forest -> Doc doc -> Result EditError (Doc doc)
moveInto child parent (Ref r) doc =
    DocI.treeMoveInto r.path child parent doc
        |> mapEdit


{-| Move `child` to sit immediately before `sibling` (under the sibling's parent).
-}
moveBefore : Id.OpId -> Id.OpId -> Ref r (TreeK ek a) forest -> Doc doc -> Result EditError (Doc doc)
moveBefore child sibling (Ref r) doc =
    DocI.treeMoveBefore r.path child sibling doc
        |> mapEdit


{-| Move `child` to sit immediately after `sibling` (under the sibling's parent).
-}
moveAfter : Id.OpId -> Id.OpId -> Ref r (TreeK ek a) forest -> Doc doc -> Result EditError (Doc doc)
moveAfter child sibling (Ref r) doc =
    DocI.treeMoveAfter r.path child sibling doc
        |> mapEdit


{-| Remove a node and its subtree from a tree ref.
-}
removeNode : Id.OpId -> Ref r (TreeK ek a) forest -> Doc doc -> Result EditError (Doc doc)
removeNode child (Ref r) doc =
    DocI.treeRemove r.path child doc
        |> mapEdit



-- CURSORS ---------------------------------------------------------------------


{-| Make a stable `Crdt.Cursor` at character `offset` in a text field. Because the cursor
is anchored to the characters around it (not to the number), it keeps pointing at the
same spot as the text is edited concurrently — see `Crdt.Cursor`. Broadcast it on the
`Crdt.Presence` channel to show collaborators' carets.
-}
cursorAt : Ref r Settable String -> Int -> Doc doc -> Result EditError Cursor
cursorAt (Ref r) offset doc =
    DocI.cursorAt r.path offset doc
        |> mapEdit


{-| Resolve a cursor back to its current character offset in this document, or `Nothing`
if it no longer points anywhere (its text is gone). The inverse of `cursorAt`.
-}
cursorOffset : Cursor -> Doc doc -> Maybe Int
cursorOffset =
    DocI.cursorOffset



-- RECORD BUILDER (emits refs) -------------------------------------------------


{-| What `build` returns: your record's `schema` (pass it to `Crdt.init`) paired with a
`refs` record of one typed `Ref` per field (use them to edit).
-}
type alias RecordRefs a refs =
    { schema : Schema Nested a, refs : refs }


{-| A record schema part-way through `record … |> field … |> field …`. You never name
this type; it just threads between the pipe steps until `build`.
-}
type RecordRefsBuilder r full a refs
    = RecordRefsBuilder
        { builder : SI.RecordBuilder full a
        , refs : refs
        }


{-| Start building a record schema. Describe a record type field by field, and get back
both a schema and a matching set of typed refs to edit through.

`record` takes **two** constructors: your record's own constructor (which reassembles the
decoded value), and a second one for the refs record — usually a type alias's constructor
whose fields are all `Ref`s. Each `field` you pipe in feeds the field's value to the first
and its ref to the second, so at the end `build` hands you `{ schema, refs }`.


    type alias Board =
        { title : String, votes : Int }

    -- one Ref per field, in the same order:
    type alias BoardRefs =
        { title : Ref Board Crdt.Settable String
        , votes : Ref Board Crdt.Counter Int
        }

    board : Crdt.RecordRefs Board BoardRefs
    board =
        Crdt.record Board BoardRefs
            |> Crdt.field "title" .title Crdt.text
            |> Crdt.field "votes" .votes Crdt.counter
            |> Crdt.build

    -- now: `board.schema` for `Crdt.init`, `board.refs.title` to edit the title.

Records nest: a field's schema can itself be another record's `.schema`, and you reach
into it with `at` (see `at`).

-}
record : (a -> b) -> refsAssembler -> RecordRefsBuilder r full (a -> b) refsAssembler
record ctor refsAssembler =
    RecordRefsBuilder
        { builder = SI.record ctor
        , refs = refsAssembler
        }


{-| Add a field to a `record` schema: its name, a getter, and the field's own schema.
Threads a typed `Ref` for the field into your refs record.
-}
field :
    String
    -> (full -> f)
    -> Schema kind f
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
field name getter schema (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.field name getter schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = schema })
        }


{-| Add a field that also reads from **older names** (`aliases`) when `name` is absent —
a schema-evolution rename. Reads prefer `name`, then each alias in order;
writes and the field's `Ref` always target `name`, so once written the old key becomes an
inert extra key. Peers on the old and new schema converge on the same document and each
reads it through its own names.

    record Todo TodoRefs
        |> aliasedField "done" [ "complete" ] .done Crdt.bool
        |> ...

-}
aliasedField :
    String
    -> List String
    -> (full -> f)
    -> Schema kind f
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
aliasedField name aliases getter schema (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.aliasedField name aliases getter schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = schema })
        }


{-| Finish a `record` schema: returns `{ schema, refs }`.
-}
build : RecordRefsBuilder r a a refs -> RecordRefs a refs
build (RecordRefsBuilder rb) =
    { schema = SI.build rb.builder
    , refs = rb.refs
    }



-- CUSTOM (SUM) BUILDER (emits refs) -------------------------------------------


{-| What `buildCustom` returns: your custom type's `schema` paired with a `refs` record
holding one `Ref` per variant **payload** (nullary variants have no payload, so they
contribute nothing).
-}
type alias CustomRefs a refs =
    { schema : Schema (Variants a) a, refs : refs }


{-| A custom-type schema part-way through `custom … |> variant… |> variant…`. You never
name this type; it threads between the pipe steps until `buildCustom`.
-}
type CustomRefsBuilder value match refs
    = CustomRefsBuilder
        { builder : SI.CustomBuilder match value
        , refs : refs
        }


{-| Start building a schema for a **custom type** (a union / "sum" type — one of several
variants, some carrying data). Like `record`, it takes two things: a **matcher** that
says which variant a value is, and a refs constructor for the payload refs.

The matcher is given one handler per variant (in declaration order) plus the value, and
returns the matching handler — it is just a `case` turned inside out:

    type Status
        = Planning
        | Active
        | Archived String

    -- refs only for variants that carry data — here, Archived's reason:
    type alias StatusRefs =
        { archivedReason : Ref Status Crdt.Settable String }

    status : Crdt.CustomRefs Status StatusRefs
    status =
        Crdt.custom
            (\planning active archived value ->
                case value of
                    Planning ->
                        planning

                    Active ->
                        active

                    Archived reason ->
                        archived reason
            )
            StatusRefs
            |> Crdt.variant0 "planning" Planning
            |> Crdt.variant0 "active" Active
            |> Crdt.variant1 "archived" Archived Crdt.text
            |> Crdt.buildCustom

Use `switch` to change which variant a value is, and the payload refs (via `at`) to edit
inside the active variant. Which variant is active is last-write-wins.

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
    -> CustomRefsBuilder value (VariantSeed -> b) refs
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
    -> Schema k1 t1
    -> CustomRefsBuilder value ((t1 -> VariantSeed) -> b) (Ref value k1 t1 -> rest)
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
    -> Schema k1 t1
    -> Schema k2 t2
    -> CustomRefsBuilder value ((t1 -> t2 -> VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> rest)
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
    -> Schema k1 t1
    -> Schema k2 t2
    -> Schema k3 t3
    -> CustomRefsBuilder value ((t1 -> t2 -> t3 -> VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> Ref value k3 t3 -> rest)
    -> CustomRefsBuilder value b rest
variant3 name ctor s1 s2 s3 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant3 name ctor s1 s2 s3 cb.builder
        , refs = cb.refs (variantPayload name 0 s1) (variantPayload name 1 s2) (variantPayload name 2 s3)
        }


{-| A **catch-all variant** that **keeps the unknown tag** so you can render it. When a
custom type's stored `$tag` names a variant this schema doesn't know — one a newer peer
added — it decodes to `ctor tag`, carrying the raw tag string, instead of failing the
read.

Give it the constructor of a variant that holds a `String`. Because it keeps the tag, your
`view` can tell a genuine value from a from-the-future one and show, say, a greyed-out
"unsupported — please update" placeholder rather than pretending it's something else:

    type Status
        = Active
        | Done String
        | Unsupported String -- carries the unknown tag

    Crdt.custom
        (\active done unsupported v ->
            case v of
                Active ->
                    active

                Done note ->
                    done note

                Unsupported tag ->
                    unsupported tag
        )
        Refs
        |> Crdt.variant0 "active" Active
        |> Crdt.variant1 "done" Done Crdt.text
        |> Crdt.catchAll Unsupported
        |> Crdt.buildCustom

The unknown value is **not corrupted by merely holding it**: sync ships the original
operations, so an old peer that reads, holds, and re-syncs it forwards the untouched
`$tag` along, and a peer still on the new schema reads the real variant. It is only lost
if the old peer `switch`es it to a variant it _does_ know — so don't normalize an
`Unsupported` value back to a known one in your update logic.

It is a real variant of your type, so your `custom` matcher must handle it, but it carries
no editable payload, so it threads no ref.

If you'd rather **not** surface the unknown value at all and just collapse it to a
sensible existing one, skip the catch-all and wrap the schema in `withDefault` instead
(`withDefault Active statusSchema`) — simpler, but the tag is then lost the moment the
value is re-saved. And if your clients are versioned in lockstep (e.g. a server pins
everyone to one build), an unknown tag is a _bug_, not evolution — use neither, and let
the read error surface it.

-}
catchAll :
    (String -> value)
    -> CustomRefsBuilder value ((String -> VariantSeed) -> b) refs
    -> CustomRefsBuilder value b refs
catchAll ctor (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.catchAll ctor cb.builder
        , refs = cb.refs
        }


{-| Finish a `custom` schema: returns `{ schema, refs }`.
-}
buildCustom : CustomRefsBuilder value (value -> VariantSeed) refs -> CustomRefs value refs
buildCustom (CustomRefsBuilder cb) =
    { schema = SI.buildCustom cb.builder
    , refs = cb.refs
    }
