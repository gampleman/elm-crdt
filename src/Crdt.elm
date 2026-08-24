module Crdt exposing
    ( Crdt, Schema
    , int, float, string, bool, counter, register
    , text, richText
    , list, movableList, dict, tree
    , opSet
    , record, field, build
    , tuple
    , custom, variant0, variant1, variant2, variant3, buildCustom
    , map
    , optional, withDefault
    , aliasedField, catchAll
    , init
    , Ref
    , at
    , Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
    , VariantSeed
    )

{-| **Describe your collaborative document, and edit it type-safely.** By document we generally
mean the part of your application model you want to sync/collaborate on.

**Describe its shape as a schema.** A `Schema` value says both what Elm type a
piece of data reads as and how concurrent edits to it merge. Think of it as the collaborative
analogue of a JSON decoder.

**Edit through refs.** The record and custom builders hand you back typed `Ref`s
alongside the schema — one per field or payload. A `Ref` is a pointer to one editable
spot, and you edit by passing it to a function like `set` or `increment`.

_Why not just edit the plain record and hand it back?_ Because you can't recover an
edit's _intent_ from before/after values, and a CRDT needs that intent to merge. If a
list goes from `[ a, b ]` to `[ a, c, b ]`, did you insert `c`, or replace `b` with `c`
and append `b`? Concurrent replicas resolve those two stories differently, so a value
diff would guess and you might end up with odd results. If a counter reads `5`
both before and after, was there no edit, or a `+3` and a `-3` that a teammate's
concurrent `+1` must still be added to? Only the _operation_ (`insert c after a`,
`increment by 3`) carries what merge needs. A `Ref` names the spot and the edit function
names the operation, so the library records intent directly instead of reverse-engineering
it, which is also why there is no `a -> Doc a` "save" function.

Threading edits through refs also makes them safe: each ref carries the kind of data
it points at, so the compiler rejects a nonsensical edit, such as an `increment` on text,
and a field name, written once in the schema, can never be mistyped.

    import Crdt
    import Crdt.Edit as Edit
    import Crdt.Id

    type alias Board =
        { title : String, votes : Int }

    -- one flat bundle: a Ref per field plus a reserved `schema`
    type alias BoardDoc =
        { title : Crdt.Ref Board Crdt.Settable String
        , votes : Crdt.Ref Board Crdt.Counter Int
        , schema : Crdt.Schema Crdt.Nested Board
        }

    board : BoardDoc
    board =
        Crdt.record Board BoardDoc
            |> Crdt.field "title" .title Crdt.text
            |> Crdt.field "votes" .votes Crdt.counter
            |> Crdt.build

    -- `board.schema` creates the document
    -- `board.title` / `board.votes` are your edit handles (each edit returns a
    -- `Result`, since a ref can fail to resolve against the current value):
    doc =
        Crdt.init (Crdt.Id.replica "alice") board.schema

    doc1 =
        Edit.set board.title "Trip plan" doc
            |> Result.withDefault doc

    doc2 =
        Edit.increment board.votes 1 doc1
            |> Result.withDefault doc1

    -- Edit.increment board.title  =>  a compile error


# Schema

A difference between this schema and a typical codec library is that you have
to decide the correct merge semantics for your data types. The basic `int`, `string`,
`bool` are _last-write-wins (LWW) registers_, meaning that if there are concurrent edits,
the last edit wins (and the previous one is overwritten, although it is still accessible
through history). A `counter` on the other hand _sums_ concurrent modifications.

For example if you are modelling a position in a graphic editor, a LWW register is probably
what you want: if I move a box to x=40px, and you move it to x=20px at the same time,
either 40px or 20px are reasonable outcomes, but 60px is not. However, if you have a shared
shopping cart and I want 2 tacos and you want 3 tacos, we do want to end up with 5 tacos
in total.

The same applies for `string` vs `text` - both are represented as a simple string, but
`string` is a LWW register, vs `text` merges edits. `string` is useful for things like
IDs or URLs, where having both just breaks, `text` is much better for human readable text.

Every builder below hands back a **bundle** — a `Crdt`. It always carries the `.schema`
itself, plus whatever handles that kind of data offers: a container's element composer, or
(for a record / custom type) one `Ref` per field. That's the one type you'll see throughout:

@docs Crdt, Schema


## Simple values

@docs int, float, string, bool, counter, register


## Collaborative text

@docs text, richText


## Collections

@docs list, movableList, dict, tree


## Defining your own CRDT

@docs opSet


## Building a record

@docs record, field, build


## Pairing two schemas

@docs tuple


## Building a custom type

@docs custom, variant0, variant1, variant2, variant3, buildCustom


## Giving a field your own type

@docs map


## Evolving a schema over time

Once you have a schema, releases will change it.

If you are writing an application with a centralized server, incrementing a protocol version
field and forcing clients to reload may well be the easiest option.

However, if you want true offline capability or decentralization, documents
written by past versions are still out there, and peers on old and new schemas
may edit the same document at once. So a schema change shouldn't be a one-shot migration: a newer
schema should read older documents, an older schema must survive newer ones, and both must
converge. These combinators make a read _tolerant_ of a shape it didn't expect, rather
than failing.

The safe, non-breaking changes and the tool for each:

  - **Add a field** — wrap it in `optional` (reads `Nothing` when missing) or
    `withDefault` (reads a default when missing), so old documents that lack it still
    read. A plain required field would error on them.
  - **Rename a field** — `aliasedField` reads the old name too, so peers on either name
    share one document.
  - **Add a variant to a custom type** — declare a `catchAll` so an older peer reads a
    newer variant's tag as a fallback (preserving it) instead of erroring.
  - **Change a value's representation** (rename an enum, `Int` ↔ `String`) — `map` between
    the old stored form and the new one.

Removing a field or variant, or changing a field's merge semantics (e.g. `counter` →
register), are _not_ safe to do in place. Treat those as a new field alongside the old one.

@docs optional, withDefault
@docs aliasedField, catchAll


# Creating a document

`init` turns a schema into a live document for one replica. Reading the value back out
(`Crdt.Doc.read` / `readAt`) and everything else you do with a document — syncing, history,
undo — lives in `Crdt.Doc`; changing it lives in `Crdt.Edit`.

@docs init


# Pointing into a document

A `Ref` names a spot inside a document — the title field, a list element, a tree node's
payload. The builders hand you a flat bundle: your field refs, a reserved `.schema`, and —
for containers — a composer (`.index` / `.key` / `.node`) that points at an element with
its schema already captured. You compose deeper with `at`, then pass the ref to a
`Crdt.Edit` function. A ref carries a phantom `kind` marker, so the compiler rejects a
nonsensical edit (`increment` on text, `move` on a plain list) before it runs.

    board.todos |> todoList.index 0 |> Crdt.at todo.done

@docs Ref

@docs at


## Edit-capability markers

Each schema is tagged with a marker describing how its value may be **edited** (a counter
can be `increment`ed, text typed into, and so on). You rarely write these yourself — they
appear in the `Ref` types the builders infer — but they are what lets the compiler reject
a nonsensical edit. They never affect how data reads or merges.

@docs Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
@docs VariantSeed

-}

import Crdt.Doc exposing (Doc)
import Crdt.Doc.Internal as DocI
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Ref.Internal as RefI exposing (Ref(..))
import Crdt.RichText as RichText
import Crdt.Schema.Internal as SI
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE



-- CREATING A DOCUMENT ---------------------------------------------------------


{-| Create a fresh, empty document from a schema, owned by a replica. Give each
participant (each browser tab, device, or server) a **distinct** `Crdt.Id.replica` so
their edits never collide.

    doc =
        Crdt.init (Crdt.Id.replica "alice-laptop") board.schema

-}
init : Id.ReplicaId -> Schema kind a -> Doc a
init =
    DocI.init



-- SCHEMA: DESCRIBING THE DOCUMENT ---------------------------------------------


{-| A description of a piece of collaborative data — what Elm type it reads as, and how
concurrent edits to it merge. Compose small `Schema` values (leaves and containers) into
the shape of your whole document. The `kind`
phantom tag records how the value may be edited (see [Edit-capability markers](#Settable)); `a` is
the Elm type it reads as.
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


{-| Kind marker: a user-defined **op-set** CRDT whose contributions have kind `ck` and type
`c` (see [`opSet`](#opSet)); edited only via `contribute`/`retract`. Carrying the
contribution's kind is what makes `contribute` reject a differently-merging schema of the
same type (a `counter` where an `int` register was declared).
-}
type alias OpSetK ck c =
    SI.OpSetK ck c


{-| The opaque "make a fresh variant payload" value the choice-type builders
(`variant0`/`variant1`/… and `catchAll`) produce and thread for you. You never build one
by hand; it only shows up in the inferred types of those builders.
-}
type alias VariantSeed =
    SI.VariantSeed


{-| Everything a builder hands back: a **bundle**. It always carries `.schema` (the piece of
collaborative data itself), plus whatever extra handles that kind of data offers —
collected in the `setters` row:

  - a **primitive** has no extras: `Crdt Settable Int {}` — just `.schema`.
  - a **container** adds a composer that points at an element with the element schema
    already captured: a list's `{ index : … }`, a dict's `{ key : … }`, a tree's
    `{ node : … }`.
  - a **record** or **custom type** you build adds one `Ref` per field or payload, so its
    bundle is your own flat alias (`{ title = …, votes = …, schema = … }`).

Because the type is an extensible record, anything that just needs a schema accepts _any_
bundle: `field`, the containers, `optional`/`withDefault`/`map` all take `Crdt kind a s`
and ignore the extras. That's why you never pass a schema separately.

-}
type alias Crdt kind a setters =
    { setters | schema : Schema kind a }


leaf : Schema kind a -> Crdt kind a {}
leaf s =
    { schema = s }


{-| An integer LWW register.
-}
int : Crdt Settable Int {}
int =
    leaf SI.int


{-| A float LWW register.
-}
float : Crdt Settable Float {}
float =
    leaf SI.float


{-| A string LWW register. For collaborative editing use `text`.
-}
string : Crdt Settable String {}
string =
    leaf SI.string


{-| A boolean LWW register.
-}
bool : Crdt Settable Bool {}
bool =
    leaf SI.bool


{-| Collaborative plain text, read as a `String`. Every character is tracked
individually, so two people typing at once merge character by character rather than
one overwriting the other — the thing a `string` register (last-write-wins) can't do.
You edit it like any other field, with `set` to the new whole string; the library diffs
old→new into the minimal insert/delete operations, so a `set` from `"cat"` to `"cart"`
is understood as "insert `r`", not a wholesale replace.

Ordering is [Fugue](https://arxiv.org/abs/2305.00583) (Weidner & Kleppmann, 2023),
which guarantees maximal non-interleaving: when two people type a run of characters at
the same spot concurrently, each run stays a contiguous block after merging. You get
`"helloworld"`, never `"hwoelrllod"`. (Fugue refines the classic
[RGA](https://doi.org/10.1016/j.jpdc.2010.12.006) sequence CRDT, whose single anchor per
element permits that interleaving.)

-}
text : Crdt Settable String {}
text =
    leaf SI.text


{-| A counter — an integer edited only with `increment` (add or subtract a delta), read
as its running total.

Reach for it instead of `int` when the value is a running tally that several people
change at once: a vote count, a stock level, a like button. Concurrent increments
add up: if two people each `increment` by 1 at the same time, the total goes up by 2.
An `int` register is last-write-wins, so under the same race one of the two `+1`s would be
silently lost.

Implemented as a [PN-counter](https://www.cs.utexas.edu/~rossbach/cs380p/papers/Counters.html#pn-counter---increment-and-decrement-counter).

-}
counter : Crdt Counter Int {}
counter =
    leaf SI.counter


{-| A last-write-wins register holding a value of any type, stored as JSON. Give a
`default` (what a fresh document reads before anyone writes the field), a JSON encoder,
and a decoder:

    import Json.Decode as D
    import Json.Encode as E

    type Priority
        = Low
        | High

    priority : Crdt.Crdt Crdt.Settable Priority {}
    priority =
        Crdt.register Low encodePriority priorityDecoder

The whole value is one cell: two people editing it concurrently don't merge field by
field. Use it for a field whose type isn't a
built-in primitive, or that you're happy to treat as one indivisible unit.

This can be used as a way to start slowly making an existing application collaborative.
Start from the top of the document, and make a record there, with all values registers.
Then you can keep doing this conversion until your whole app can do granular merges.

-}
register : a -> (a -> JE.Value) -> JD.Decoder a -> Crdt Settable a {}
register default encode decoder =
    leaf (SI.register default encode decoder)


{-| Make a field's schema optional for schema evolution: it reads `Nothing` when the
field is absent (a document written before the field existed) instead of failing, and
`Just v` when present. Seeding `Nothing` writes no value.

    |> Crdt.field "priority" .priority (Crdt.optional prioritySchema)

-}
optional : Crdt kind a s -> Crdt kind (Maybe a) {}
optional bundle =
    leaf (SI.optional bundle.schema)


{-| Give a field's schema a default read when the field is absent: it reads `default`
instead of failing, without changing the value type. The real value is minted on first
write.

    |> Crdt.field "priority" .priority (Crdt.withDefault Medium prioritySchema)

It also absorbs the other "the shape evolved" case: if `default` wraps a custom type
and the stored value is a variant this schema doesn't know (a newer peer added it), the
read is `default` rather than an error. (`catchAll` is the alternative that keeps the unknown tag instead.
Note that re-saving a collapsed value with `switch` loses the original tag, whereas simply
holding and syncing it preserves it.) Genuinely malformed data still errors.

-}
withDefault : a -> Crdt kind a s -> Crdt kind a {}
withDefault default bundle =
    leaf (SI.withDefault default bundle.schema)


{-| Transform the value a schema reads and seeds, in **both** directions (`to` on read,
`from` on seed). It's the general tool for raising one of the built-in representations
into a domain type of your own (and lowering it back to store), so you can model a field
as the type your app actually uses while it merges as a primitive underneath.

    -- store an instant as milliseconds (a plain LWW `int`),
    -- read it as a `Time.Posix`:
    time : Crdt.Crdt Crdt.Settable Time.Posix {}
    time =
        Crdt.map Time.millisToPosix
            Time.posixToMillis
            Crdt.int

The merge semantics are the underlying schema's: mapping `int` gives a last-write-wins
instant; mapping `counter` would give something that still sums. Both directions are
required so a seeded value round-trips (`from` then `to` must be identity).

`map` also serves schema evolution re-spelling an enum, or moving a field from `Int`
to `String` while the stored bytes stay put by mapping between the old representation and
the new domain type.

-}
map : (a -> b) -> (b -> a) -> Crdt kind a s -> Crdt kind b {}
map to from bundle =
    leaf (SI.map to from bundle.schema)


{-| An ordered, growable list of `a` (each element is itself described by a sub-schema). Elements keep a stable identity,
so a nested edit or a cursor into one survives other people's concurrent insertions and
deletions elsewhere in the list.

Ordering (and concurrent-insert convergence) is [Fugue](https://arxiv.org/abs/2305.00583),
the same sequence CRDT `text` uses: two people appending at the same spot get both
elements, in a deterministic order, with each contiguous run kept intact. Elements can't be
reordered either, that's what `movableList` is for.

-}
list :
    Crdt ek a s
    -> Crdt (ListK Fixed ek a) (List a) { index : Int -> Ref r (ListK mv ek a) (List a) -> Ref r ek a }
list elem =
    { schema = SI.list elem.schema
    , index = \i (RefI.Ref c) -> RefI.Ref { path = c.path |> Path.index i, schema = elem.schema }
    }


{-| Like `list`, but elements can also be reordered with `move` while keeping their
identity. A nested edit, or a cursor, anchored to an item follows it to its new position.
Use it for a hand-orderable list (a Kanban column, a playlist, a to-do list you drag to
rank); use plain `list` when order is fixed by insertion and you never move items
(`movableList` consumes roughly twice the per item in-memory bookkeeping of `list`,
so if you don't need it it is more efficient to just use `list`).

Moves converge without the cycles a naive "re-point the previous element" scheme would
create: an element's position is a stable value, and the latest move wins by
last-write-wins. Concurrent moves of the same element resolve to one deterministic
position.

-}
movableList :
    Crdt ek a s
    -> Crdt (ListK Movable ek a) (List a) { index : Int -> Ref r (ListK mv ek a) (List a) -> Ref r ek a }
movableList elem =
    { schema = SI.movableList elem.schema
    , index = \i (RefI.Ref c) -> RefI.Ref { path = c.path |> Path.index i, schema = elem.schema }
    }


{-| A dictionary from string keys to values of `a`, read as an Elm `Dict`. Set and remove
keys with `setKey` / `removeKey`; each value is described by a sub-schema and merges by its
own rules, so a dict of `text` merges each entry character-wise.

Key **presence** is last-write-wins by timestamp, which makes the set-vs-remove race
well-defined: concurrent `setKey` and `removeKey` on the same key resolve by stamp, so the
later of the two wins. Editing _inside_ a value is not a presence write, so it does not
resurrect a key a peer removed concurrently — the removal stands, though the value it held
is kept, so `setKey`ing the key again brings back whatever that value has since merged to
(with the new value written over it, by the same rules as any other write).

`removeKey` on a key this replica has never seen does nothing: there is no value to
tombstone, and a delete races only against writes it has actually observed, so a concurrent
`setKey` of an unseen key wins.

-}
dict :
    Crdt vk a s
    -> Crdt (DictK vk a) (Dict String a) { key : String -> Ref r (DictK vk a) (Dict String a) -> Ref r vk a }
dict val =
    { schema = SI.dict val.schema
    , key = \k (RefI.Ref c) -> RefI.Ref { path = c.path |> Path.key k, schema = val.schema }
    }


{-| A movable tree of `a`: hierarchical, re-parentable, sibling-ordered data (an
outline, a file tree, threaded comments). Reads as a `Crdt.Tree.Forest a`.

Re-parenting is the hard part — two people could concurrently move A under B and B under A,
which a naive merge would turn into a detached cycle. The tree uses Kleppmann et al.'s
[replicated-tree move operation](https://martin.kleppmann.com/2021/10/07/crdt-tree-move-operation.html):
moves are applied in a deterministic global order and any move that would create a cycle is
skipped identically on every replica, so all replicas derive the same tree. Sibling order
is a [fractional index](https://www.figma.com/blog/realtime-editing-of-ordered-sequences/)
(a position value, not an "after node X" pointer), which keeps concurrent reorders from
cycling.

-}
tree :
    Crdt ek a s
    -> Crdt (TreeK ek a) (Tree.Forest a) { node : Id.OpId -> Ref r (TreeK ek a) (Tree.Forest a) -> Ref r ek a }
tree nodeBundle =
    { schema = SI.tree nodeBundle.schema
    , node = \nodeId (RefI.Ref c) -> RefI.Ref { path = c.path |> Path.node nodeId, schema = nodeBundle.schema }
    }


{-| Collaborative rich (formatted) text: a character sequence (merged character-wise,
like `text`) plus a set of formatting **marks** such as bold, italic, headings, list
items layered over character ranges. Reads as a list of `Crdt.RichText.Span`s (runs of
text sharing the same formatting); edit the text with ordinary text edits and the
formatting with `mark` / `unmark`.

Marks are [Peritext](https://www.inkandswitch.com/peritext/) ranges: each mark is anchored
to the character identities at its ends, not to numeric offsets, so a mark stays
attached to the right span as text is inserted or deleted around it — and a bold applied
concurrently with an insertion at its boundary merges the way a human would expect.

-}
richText : Crdt RichK (List RichText.Span) {}
richText =
    leaf SI.richText


{-| Define your own CRDT type when none of the built-ins fit. It's an
operation-based set: each `contribute` writes one contribution (tagged with its own
op-id), merge simply **unions** the contributions, and a read `fold`s the whole set into
your value. Because union is commutative, associative, and idempotent, convergence is free
— you only supply the `fold`, which must be a pure, order-independent function of the set
(it sees the contributions in no particular order, possibly with duplicates already
deduped).

This generalizes the built-in accretive CRDTs: `counter` is `opSet` with `fold = List.sum`;
a max-register, a grow-only set, or an add-wins tally are all a few lines. It cannot express
things that need coordinated _removal_ or a custom _sequence_ order — those aren't a plain
union-of-contributions.

    -- a grow-only max register
    maxRegister : Crdt.Crdt (Crdt.OpSetK Crdt.Settable Int) Int {}
    maxRegister =
        Crdt.opSet
            { contribution = Crdt.int
            , fold = List.maximum >> Maybe.withDefault 0
            }

-}
opSet : { contribution : Crdt ck c s, fold : List c -> a } -> Crdt (OpSetK ck c) a {}
opSet config =
    leaf (SI.opSet { contribution = config.contribution.schema, fold = config.fold })


{-| A typed pointer to one editable spot inside a document — a field, a list element, a
dictionary value, a variant's payload. You get refs from the `record` and `custom`
builders (as `board.title`) and pass them to the edit functions (`set`, `increment`,
…) to say _where_ to edit.

The three type parameters are what make edits safe, and you rarely write them by hand:

  - `r` is the type at the **root** the ref was built from (e.g. your `Board` record), so
    refs from different documents can't be mixed up;
  - `kind` is **how the spot may be edited** — `Settable`, `Counter`, `RichK`, and so on
    — which is what lets the compiler accept `increment` only on a `Counter` ref;
  - `a` is the value the spot **reads as**.

Opaque: build refs with the builders, compose them with `at` (and a container bundle's
`.index`/`.key`/`.node`), and edit through them.

-}
type alias Ref r kind a =
    RefI.Ref r kind a


{-| Compose refs: descend from a spot of type `sub` into a spot inside it. Reads as
navigation in a pipe:

    board.status |> at status.archivedReason

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
   this to construct the payload refs they thread into the bundle, so the argument index
   and schema always match the declaration. Not exposed — users get these refs from the
   builder, never build them by hand.
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



-- RECORD BUILDER (emits refs) -------------------------------------------------


{-| A record schema part-way through `record … |> field … |> field …`. You never name
this type; it just threads between the pipe steps until `build`.
-}
type RecordRefsBuilder r full a refs
    = RecordRefsBuilder
        { builder : SI.RecordBuilder full a
        , refs : refs
        }


{-| Start building a record schema. Describe a record type field by field, and get back one
**flat bundle**: a typed `Ref` per field plus a reserved `.schema`.

`record` takes **two** constructors: your record's own constructor (which reassembles the
decoded value), and a second one for the bundle — a type alias whose fields are the `Ref`s
in field order, followed by a `schema` field. Each `field` you pipe in feeds the field's
value to the first and its ref to the second, and `build` supplies the finished schema as
the bundle's last field.


    type alias Board =
        { title : String, votes : Int }

    -- one Ref per field, in the same order, then `schema` last:
    type alias BoardDoc =
        { title : Crdt.Ref Board Crdt.Settable String
        , votes : Crdt.Ref Board Crdt.Counter Int
        , schema : Crdt.Schema Crdt.Nested Board
        }

    board : BoardDoc
    board =
        Crdt.record Board BoardDoc
            |> Crdt.field "title" .title Crdt.text
            |> Crdt.field "votes" .votes Crdt.counter
            |> Crdt.build

    -- now: `board.schema` for `Crdt.init`,
    -- `board.title` to edit the title.

Records nest: a field's schema can itself be another record's `.schema`, and you reach
into it with [`at`](#at).

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
    -> Crdt kind f s
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
field name getter bundle (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.field name getter bundle.schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = bundle.schema })
        }


{-| Add a field that also reads from older names (`aliases`) when `name` is absent —
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
    -> Crdt kind f s
    -> RecordRefsBuilder r full (f -> b) (Ref r kind f -> rest)
    -> RecordRefsBuilder r full b rest
aliasedField name aliases getter bundle (RecordRefsBuilder rb) =
    RecordRefsBuilder
        { builder = SI.aliasedField name aliases getter bundle.schema rb.builder
        , refs = rb.refs (Ref { path = Path.root |> Path.field name, schema = bundle.schema })
        }


{-| Finish a `record` schema. Feeds the built schema as the **last** argument to the refs
assembler, producing one **flat** bundle: your `Ref`s plus a reserved `.schema` field. So
`build` returns `{ …fieldRefs, schema }` — there is no separate `.refs` wrapper.
-}
build : RecordRefsBuilder r a a (Schema Nested a -> unified) -> unified
build (RecordRefsBuilder rb) =
    rb.refs (SI.build rb.builder)


{-| Combine two schema bundles into a pair `( a, b )` where each half keeps its own merge
semantics.

Returns a flat bundle `{ first, second, schema }` — a `Ref` for each component and the
schema to place in a document.

Prefer a named `record` when the two components have meaningful names.

-}
tuple :
    Crdt k1 a s1
    -> Crdt k2 b s2
    ->
        { first : Ref ( a, b ) k1 a
        , second : Ref ( a, b ) k2 b
        , schema : Schema Nested ( a, b )
        }
tuple firstBundle secondBundle =
    { first = Ref { path = Path.root |> Path.field "0", schema = firstBundle.schema }
    , second = Ref { path = Path.root |> Path.field "1", schema = secondBundle.schema }
    , schema =
        SI.record Tuple.pair
            |> SI.field "0" Tuple.first firstBundle.schema
            |> SI.field "1" Tuple.second secondBundle.schema
            |> SI.build
    }



-- CUSTOM (SUM) BUILDER (emits refs) -------------------------------------------


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
returns the matching handler — it is just a `case` turned inside out. Use
`variant0`/`variant1`/`variant2`/`variant3` for a variant carrying that many arguments,
each argument getting its own schema (and its own payload ref):

    type Status
        = Active
        | Archived String
        | Snooze String Time.Posix

    -- flat bundle: a payload ref per data-carrying variant arg, then `schema` last
    type alias StatusDoc =
        { archivedReason : Crdt.Ref Status Crdt.Settable String
        , snoozeReason : Crdt.Ref Status Crdt.Settable String
        , snoozeUntil : Crdt.Ref Status Crdt.Settable Time.Posix
        , schema : Crdt.Schema (Crdt.Variants Status) Status
        }

    status : StatusDoc
    status =
        Crdt.custom
            (\active archived snooze value ->
                case value of
                    Active ->
                        active

                    Archived reason ->
                        archived reason

                    Snooze reason until ->
                        snooze reason until
            )
            StatusDoc
            |> Crdt.variant0 "active" Active
            |> Crdt.variant1 "archived" Archived Crdt.text
            |> Crdt.variant2 "snooze" Snooze Crdt.text time
            |> Crdt.buildCustom

    time : Crdt.Crdt Crdt.Settable Time.Posix {}
    time =
        Crdt.map Time.millisToPosix
            Time.posixToMillis
            Crdt.int

Each `variant*` step contributes exactly its arguments' refs to the `StatusDoc`
constructor, in order — so `variant2 "snooze" Snooze …` supplies `snoozeReason` then
`snoozeUntil`. (The `Snooze` payload also shows `map` in action: `time` stores an instant
as a plain `int` but reads it as a `Time.Posix`.) Use `switch` to change which variant a
value is, and the payload refs (via `at`) to edit inside the active variant. Which variant
is active is last-write-wins.

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
    -> Crdt k1 t1 s1
    -> CustomRefsBuilder value ((t1 -> VariantSeed) -> b) (Ref value k1 t1 -> rest)
    -> CustomRefsBuilder value b rest
variant1 name ctor b1 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant1 name ctor b1.schema cb.builder
        , refs = cb.refs (variantPayload name 0 b1.schema)
        }


{-| A two-argument variant. Threads two payload refs.
-}
variant2 :
    String
    -> (t1 -> t2 -> value)
    -> Crdt k1 t1 s1
    -> Crdt k2 t2 s2
    -> CustomRefsBuilder value ((t1 -> t2 -> VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> rest)
    -> CustomRefsBuilder value b rest
variant2 name ctor b1 b2 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant2 name ctor b1.schema b2.schema cb.builder
        , refs = cb.refs (variantPayload name 0 b1.schema) (variantPayload name 1 b2.schema)
        }


{-| A three-argument variant. Threads three payload refs.
-}
variant3 :
    String
    -> (t1 -> t2 -> t3 -> value)
    -> Crdt k1 t1 s1
    -> Crdt k2 t2 s2
    -> Crdt k3 t3 s3
    -> CustomRefsBuilder value ((t1 -> t2 -> t3 -> VariantSeed) -> b) (Ref value k1 t1 -> Ref value k2 t2 -> Ref value k3 t3 -> rest)
    -> CustomRefsBuilder value b rest
variant3 name ctor b1 b2 b3 (CustomRefsBuilder cb) =
    CustomRefsBuilder
        { builder = SI.variant3 name ctor b1.schema b2.schema b3.schema cb.builder
        , refs = cb.refs (variantPayload name 0 b1.schema) (variantPayload name 1 b2.schema) (variantPayload name 2 b3.schema)
        }


{-| A catch-all variant that keeps the unknown tag so you can render it. When a
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

The unknown value is _not corrupted_ by merely holding it: sync ships the original
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


{-| Finish a `custom` schema. Like `build`, feeds the built schema as the **last** argument
to the refs assembler, producing one **flat** bundle: the payload `Ref`s plus a reserved
`.schema` field.
-}
buildCustom : CustomRefsBuilder value (value -> VariantSeed) (Schema (Variants value) value -> unified) -> unified
buildCustom (CustomRefsBuilder cb) =
    cb.refs (SI.buildCustom cb.builder)
