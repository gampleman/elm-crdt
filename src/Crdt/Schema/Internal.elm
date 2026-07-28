module Crdt.Schema.Internal exposing
    ( Crdt, Error(..), Seed
    , Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
    , int, float, string, bool, text, counter, register
    , optional, withDefault, map
    , list, movableList, dict, tree, richText, opSet
    , record, field, aliasedField, build, RecordBuilder
    , CustomBuilder, VariantSeed, custom, variant0, variant1, variant2, variant3, catchAll, buildCustom
    , variantArgKey, tagKey
    , with, seedOneFrom, decodeNode, emptyNode, errorToString
    )

{-| The combinator layer: a `Crdt a` describes a CRDT's shape and ties it to a
typed Elm value `a`, the way an `elm/json` decoder ties JSON to a value. Compose
them to build records, lists, dicts and text out of the primitives.

A `Crdt a` carries three capabilities, all keyed off the uniform `Node`:

  - **decode** a `Node` into a typed `a` (the read path the demo renders from);
  - construct an **empty** `Node` for a fresh document;
  - **seed** a `Node` from a concrete value (used by `with`, so edits like
    "append this todo" can mint a whole subtree).

There is deliberately no `encode : a -> Node` that reconciles with existing
state — that is the hard diff problem. All in-place mutation goes through the
op-log edit APIs in `Crdt.Doc.Internal` (surfaced as `Crdt`), decoupled from this layer.

@docs Crdt, Error, Seed
@docs Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK, TreeK, RichK, OpSetK
@docs int, float, string, bool, text, counter, register
@docs optional, withDefault, map
@docs list, movableList, dict, tree, richText, opSet
@docs record, field, aliasedField, build, RecordBuilder
@docs CustomBuilder, VariantSeed, custom, variant0, variant1, variant2, variant3, catchAll, buildCustom
@docs variantArgKey, tagKey
@docs with, seedOneFrom, decodeNode, emptyNode, errorToString

-}

import Crdt.Id.Internal as Id exposing (Ctx)
import Crdt.Internal as I
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Crdt.RichText exposing (Span)
import Crdt.RichText.Internal as RichText
import Crdt.Text as Text
import Crdt.Tree.Internal as Tree
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE


{-| An opaque builder of a fresh subtree from a value, produced by `with` and
consumed by the edit APIs. Re-exported from `Crdt.Internal` so the edit APIs can
name it without leaking the internal `Node` type.
-}
type alias Seed =
    I.Seed


{-| A schema tying a typed value `a` to CRDT `Node` state, tagged with a phantom
`kind` describing how it may be _edited_ (used by the `Crdt` module to reject nonsensical
edits at compile time — e.g. `increment` on text). The `kind` never affects reads
or merge; it is erased at runtime.
-}
type Crdt kind a
    = Crdt
        { decode : Node -> Result Error a
        , empty : Ctx -> ( Node, Ctx )
        , seed : a -> Ctx -> ( Node, Ctx )

        -- Schema-evolution support (see docs/13). What this schema reads when its slot
        -- is **absent** from its parent (a record field the writer's schema didn't have
        -- yet): `Nothing` = required, absence is a `MissingField` error (the default for
        -- every leaf/container); `Just v` = absence reads as `v` (set by `optional` /
        -- `withDefault`, so an added field is non-breaking on old documents). Consulted
        -- only by `field`; absence is meaningless for a top-level read or a dict/list
        -- element (which are simply not present in the collection).
        , whenAbsent : Maybe a
        }


{-| Kind marker (phantom — never constructed): an LWW register leaf; supports `set`.
The `Never` argument makes it uninhabited without tripping unused-constructor lint.
-}
type Settable
    = Settable Never


{-| Kind marker: a PN-counter; supports `increment`.
-}
type Counter
    = Counter Never


{-| Kind marker: a record or map; edited only by descending into fields/keys.
-}
type Nested
    = Nested Never


{-| Kind marker: a sum type over `a`; supports variant switching + payload refs.
-}
type Variants a
    = Variants Never


{-| Kind marker: a list of `a` with element kind `ek` and movability `mv`
(`Fixed` for `list`, `Movable` for `movableList`). Supports append/remove +
element refs; `move` additionally requires `Movable`.
-}
type ListK mv ek a
    = ListK Never


{-| Movability marker for a plain `list` (no reordering).
-}
type Fixed
    = Fixed Never


{-| Movability marker for a `movableList` (supports `move`).
-}
type Movable
    = Movable Never


{-| Kind marker: a `dict` of `a` with value kind `vk`; key set/remove + value refs.
-}
type DictK vk a
    = DictK Never


{-| Kind marker: a movable `tree` of `a` with node kind `ek`; supports
add/move/remove of nodes + per-node payload refs.
-}
type TreeK ek a
    = TreeK Never


{-| Kind marker: rich (formatted) text; supports text edits + `mark`/`unmark`.
-}
type RichK
    = RichK Never


{-| Kind marker: a user-defined **op-set** CRDT over contribution type `c` (see
`opSet` / `docs/14`). Its only edit verbs are `Crdt.contribute`/`retract` — it
is not `set`/`increment`-able — so the phantom carries `c` to type those verbs.
-}
type OpSetK c
    = OpSetK Never


{-| What can go wrong reading a `Node` through a schema.

`UnknownVariant` is special: it means a custom type's stored `$tag` names no declared
variant — the forward-compatibility case where a newer peer wrote a variant this schema
doesn't know. It is kept distinct from `BadValue` (genuinely malformed data) so that
`withDefault`/`catchAll` can tolerate _just_ it while real errors still fail loudly.

-}
type Error
    = TypeMismatch String
    | MissingField String
    | BadValue String
    | UnknownVariant String


{-| Render an error for display.
-}
errorToString : Error -> String
errorToString err =
    case err of
        TypeMismatch s ->
            "type mismatch: " ++ s

        MissingField s ->
            "missing field: " ++ s

        BadValue s ->
            "bad value: " ++ s

        UnknownVariant s ->
            "unknown variant: " ++ s



-- DRIVERS (used by Crdt.Doc.Internal) ----------------------------------------


{-| Decode a node through a schema.
-}
decodeNode : Crdt kind a -> Node -> Result Error a
decodeNode (Crdt c) =
    c.decode


{-| The empty node for a schema.
-}
emptyNode : Crdt kind a -> Ctx -> ( Node, Ctx )
emptyNode (Crdt c) =
    c.empty


{-| Seed a node from a value, producing an opaque `Seed` that the `Crdt.Doc.Internal`
edit APIs pass to `listAppend` / `setKey`.

    todoSchema |> S.with (Todo "pack" False)

-}
with : a -> Crdt kind a -> Seed
with value (Crdt c) =
    I.Seed (c.seed value)


{-| Seed a **single element** from a _container_ schema (list/movableList/dict/tree),
recovering the element seeder without needing the element schema separately — so the edit
APIs that create an element can drop their element-schema argument and take the container
ref instead.

We can't pull a typed `element -> Seed` out of the opaque container `Crdt` (Elm can't
destructure the element type out of the container's `kind`), so we go through the
container's own `seed`: `mkSingleton value` builds the one-element collection value that
`seed` accepts, we seed it **from the live `Ctx`** (so stamps are minted from the
document's clock — stamp-sound, no duplicate OpIds under concurrent edits), then `extract`
pulls the lone element's content node back out. The caller in `Doc.Internal` supplies
`mkSingleton`/`extract` because it statically knows the container's concrete kind.

-}
seedOneFrom : (element -> collection) -> (Node -> Maybe Node) -> Crdt containerKind collection -> element -> Seed
seedOneFrom mkSingleton extract (Crdt c) value =
    I.Seed
        (\ctx ->
            let
                ( containerNode, ctx1 ) =
                    c.seed (mkSingleton value) ctx
            in
            case extract containerNode of
                Just elementNode ->
                    ( elementNode, ctx1 )

                Nothing ->
                    -- unreachable for a valid container schema; keep total
                    ( containerNode, ctx1 )
        )



-- PRIMITIVES -----------------------------------------------------------------


primReg : (Prim -> Result Error a) -> (a -> Prim) -> Prim -> Crdt Settable a
primReg fromPrim toPrim emptyPrim =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asPrim node of
                    Just p ->
                        fromPrim p

                    Nothing ->
                        Err (TypeMismatch "expected a register")
        , empty =
            \ctx ->
                let
                    ( id, ctx1 ) =
                        Id.nextId ctx
                in
                ( Node.reg emptyPrim id, ctx1 )
        , seed =
            \value ctx ->
                let
                    ( id, ctx1 ) =
                        Id.nextId ctx
                in
                ( Node.reg (toPrim value) id, ctx1 )
        }


{-| An integer LWW register.
-}
int : Crdt Settable Int
int =
    primReg
        (\p ->
            case p of
                PInt n ->
                    Ok n

                _ ->
                    Err (BadValue "expected int")
        )
        PInt
        (PInt 0)


{-| A float LWW register.
-}
float : Crdt Settable Float
float =
    primReg
        (\p ->
            case p of
                PFloat n ->
                    Ok n

                _ ->
                    Err (BadValue "expected float")
        )
        PFloat
        (PFloat 0)


{-| A string LWW register. For collaborative editing use `text` instead.
-}
string : Crdt Settable String
string =
    primReg
        (\p ->
            case p of
                PString s ->
                    Ok s

                _ ->
                    Err (BadValue "expected string")
        )
        PString
        (PString "")


{-| A boolean LWW register.
-}
bool : Crdt Settable Bool
bool =
    primReg
        (\p ->
            case p of
                PBool b ->
                    Ok b

                _ ->
                    Err (BadValue "expected bool")
        )
        PBool
        (PBool False)


{-| An LWW register holding an **arbitrary** value, stored as JSON. Give a `default`
(what a fresh document reads before any write), an `encode`, and a `decoder`. The whole
value is one last-write-wins cell: a concurrent edit replaces it wholesale (latest write
wins), it does not merge structurally.
-}
register : a -> (a -> JE.Value) -> JD.Decoder a -> Crdt Settable a
register default encode decoder =
    let
        toPrim value =
            PString (JE.encode 0 (encode value))

        fromPrim prim =
            case prim of
                PString s ->
                    JD.decodeString decoder s
                        |> Result.mapError (\e -> BadValue (JD.errorToString e))

                _ ->
                    Err (BadValue "expected a JSON-encoded register")
    in
    Crdt
        { -- Like the primitive registers, a `register` is seeded into the empty node
          -- (below), so it is present from the start and merges by plain SetReg LWW.
          -- Absence-tolerance for schema evolution is opt-in via `withDefault`.
          whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asPrim node of
                    Just p ->
                        fromPrim p

                    Nothing ->
                        Err (TypeMismatch "expected a register")
        , empty =
            \ctx ->
                let
                    ( id, ctx1 ) =
                        Id.nextId ctx
                in
                ( Node.reg (toPrim default) id, ctx1 )
        , seed =
            \value ctx ->
                let
                    ( id, ctx1 ) =
                        Id.nextId ctx
                in
                ( Node.reg (toPrim value) id, ctx1 )
        }



-- SCHEMA EVOLUTION -----------------------------------------------------------
--
-- Read-time tolerance so a document written by one schema version reads sensibly under
-- another (see docs/13). These evolve the *decoder*, never the data: the Node tree
-- already syncs unknown fields losslessly (merge is schema-blind), so the only place
-- drift bites is read, and these make read tolerant. All are pure read policy — they
-- never gate a write or a merge, so convergence is untouched.


{-| Make a schema **optional**: reads `Nothing` when the value is a null sentinel or the
field is absent (a document that predates the field), and `Just v` when present, instead
of failing. Seeding `Nothing` writes a null register; `Just v` seeds the inner value.

    -- adding `priority` to an existing record is non-breaking:
    |> field "priority" .priority (S.optional prioritySchema)

`Maybe` is represented **uniformly as a map** `{ "just" : <inner> }`: `Nothing` is the
empty map, `Just v` has the `"just"` key present. This keeps the node shape constant (a
`Map`) whether present or not, so the edit/diff engine transitions between `Nothing` and
`Just` by flipping one key's presence — the same machinery dicts use — rather than
swapping node kinds (which the diff engine leaves untouched). The empty base is the empty
map, so a newer peer that adds this field, then merges an older peer's document, reads
`Nothing`; the map key's presence stamps are minted at write time, so a real `Just`
write outranks the base by LWW.

-}
optional : Crdt kind a -> Crdt kind (Maybe a)
optional (Crdt inner) =
    let
        justKey =
            "just"
    in
    Crdt
        { whenAbsent = Just Nothing
        , decode =
            \node ->
                case Node.asMap node of
                    Just entries ->
                        case Dict.get justKey entries of
                            Just e ->
                                if e.present then
                                    case inner.decode e.value |> Result.map Just of
                                        -- an unknown custom-type tag (a newer peer's
                                        -- variant) reads as `Nothing`, like an absent
                                        -- value — the same evolution tolerance as
                                        -- `withDefault`.
                                        Err (UnknownVariant _) ->
                                            Ok Nothing

                                        result ->
                                            result

                                else
                                    Ok Nothing

                            Nothing ->
                                Ok Nothing

                    Nothing ->
                        -- tolerate a legacy/foreign non-map node by reading it as the
                        -- inner value (so an older doc that stored the bare value still
                        -- reads as `Just`).
                        inner.decode node |> Result.map Just
        , empty = \ctx -> ( Node.mapFromEntries Dict.empty, ctx )
        , seed =
            \value ctx ->
                case value of
                    Nothing ->
                        ( Node.mapFromEntries Dict.empty, ctx )

                    Just v ->
                        let
                            ( innerNode, ctx1 ) =
                                inner.seed v ctx

                            ( stamp, ctx2 ) =
                                Id.nextId ctx1
                        in
                        ( Node.mapFromEntries (Dict.singleton justKey (Node.entry stamp True innerNode)), ctx2 )
        }


{-| Give a schema a **default** read when its field is absent (an older document that
predates the field): the field reads `default` instead of `MissingField`. Unlike
`optional` the value type is unchanged, so it drops into an existing schema without
rewrapping the record in `Maybe`. This is Cambria's "add field with default" as a pure
read rule — the default is what old data reads as, and the real value is minted on first
write.

    |> field "priority" .priority (S.withDefault Medium prioritySchema)

**How this works across peers** (see docs/13): the empty base **seeds the `default`
value**, not the inner leaf's arbitrary empty (`0`/`False`/`""`). Since every replica
builds its own base from its own schema, a newer peer that adds this field seeds it as
`default`; when it merges an older peer's document (which lacks the field), the field is
already present _as the default_ — so it reads `default`, exactly as absence should. And
it stays convergent: a base seed's stamp is minted at `init` (a low Lamport counter),
so any real write outranks it by LWW — the default is only ever the pre-write value, and
two peers both showing the unwritten default agree because the _value_ agrees.

It also covers the sibling evolution case for **custom types**: a stored `$tag` naming a
variant this schema doesn't know (a newer peer added it) reads as `default` instead of
failing — the same "the shape evolved" tolerance as an absent field. A genuinely
**undecodable** node (wrong node kind, malformed value) still surfaces its error; only
absence and unknown-variant are absorbed. `catchAll` is the alternative for custom types
when you'd rather keep the unknown tag than collapse it to a default.

-}
withDefault : a -> Crdt kind a -> Crdt kind a
withDefault default (Crdt inner) =
    Crdt
        { whenAbsent = Just default
        , decode =
            \node ->
                case inner.decode node of
                    Err (UnknownVariant _) ->
                        Ok default

                    result ->
                        result
        , empty = \ctx -> inner.seed default ctx
        , seed = inner.seed
        }


{-| Transform the value a schema reads/seeds, in **both directions** (`to` on read,
`from` on seed). Covers value-shape evolution — re-spelling an enum, int↔string, a unit
change — while keeping the stored `Node` unchanged. Both directions are required (unlike
Cambria's one-way `convert`) so seeding round-trips coherently; a non-invertible change
(two fields folded into one) is not a `map` — that would be a Layer-4 lens (docs/13).

    -- store an enum as a string, read it as a custom type:
    S.map statusFromString statusToString S.string

-}
map : (a -> b) -> (b -> a) -> Crdt kind a -> Crdt kind b
map to from (Crdt inner) =
    Crdt
        { whenAbsent = Maybe.map to inner.whenAbsent
        , decode = \node -> inner.decode node |> Result.map to
        , empty = inner.empty
        , seed = \value ctx -> inner.seed (from value) ctx
        }



-- COUNTER --------------------------------------------------------------------


{-| A PN-counter, read as its integer total. Unlike an `int` register (which is
last-write-wins, so concurrent `+1`/`+1` collapses to 1), concurrent increments
from different replicas **sum** — `+1` and `+1` give 2. Use `Crdt.increment` to change it.
-}
counter : Crdt Counter Int
counter =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asCounter node of
                    Just n ->
                        Ok n

                    Nothing ->
                        Err (TypeMismatch "expected counter")
        , empty = \ctx -> ( Node.counter Dict.empty, ctx )
        , seed =
            \value ctx ->
                if value == 0 then
                    ( Node.counter Dict.empty, ctx )

                else
                    let
                        ( stamp, ctx1 ) =
                            Id.nextId ctx
                    in
                    ( Node.counter (Dict.singleton (Id.opIdToString stamp) (Node.increment stamp value)), ctx1 )
        }



-- TEXT -----------------------------------------------------------------------


{-| Collaborative text, read as a `String`. Backed by an RGA of characters so
concurrent edits merge character-wise.
-}
text : Crdt Settable String
text =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asTxt node of
                    Just rga ->
                        Ok (Text.toString rga)

                    Nothing ->
                        Err (TypeMismatch "expected text")
        , empty = \ctx -> ( Node.txt Rga.empty, ctx )
        , seed =
            \value ctx ->
                let
                    ( rga, ctx1 ) =
                        Text.fromString ctx value
                in
                ( Node.txt rga, ctx1 )
        }


{-| Collaborative **rich** (formatted) text, read as a list of `Span`s (maximal runs
sharing formatting). Backed by a Fugue character sequence plus a Peritext mark set;
edited by text insert/delete plus `mark`/`unmark` (see the `Crdt` module).
-}
richText : Crdt RichK (List Span)
richText =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asRich node of
                    Just r ->
                        Ok (RichText.toSpans r)

                    Nothing ->
                        Err (TypeMismatch "expected rich text")
        , empty = \ctx -> ( Node.rich RichText.empty, ctx )
        , seed =
            \spans ctx ->
                let
                    ( r, ctx1 ) =
                        RichText.fromSpans ctx spans
                in
                ( Node.rich r, ctx1 )
        }



-- LIST -----------------------------------------------------------------------


{-| An ordered list of `a`, backed by an RGA. Concurrent inserts from different
replicas all survive and converge to a deterministic order.
-}
list : Crdt ek a -> Crdt (ListK Fixed ek a) (List a)
list (Crdt elem) =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asSeq node of
                    Just rga ->
                        Rga.toList rga
                            |> List.map elem.decode
                            |> combine

                    Nothing ->
                        Err (TypeMismatch "expected list")
        , empty = \ctx -> ( Node.seq Rga.empty, ctx )
        , seed =
            \values ctx ->
                let
                    ( rga, ctx1 ) =
                        List.foldl
                            (\value ( acc, c, origin ) ->
                                let
                                    ( childNode, c1 ) =
                                        elem.seed value c

                                    ( acc1, c2 ) =
                                        Rga.insertAfter c1 origin childNode acc

                                    newId =
                                        Rga.lastVisibleId acc1
                                in
                                ( acc1, c2, newId )
                            )
                            ( Rga.empty, ctx, Nothing )
                            values
                            |> (\( acc, c, _ ) -> ( acc, c ))
                in
                ( Node.seq rga, ctx1 )
        }


{-| A **reorderable** list of `a` — like `list`, but items can be moved with
`Crdt.Doc.listMove` and keep their identity (nested edits and cursors follow a
moved item). Backed by `Crdt.MoveList`. Reads as a plain `List a` in order.
-}
movableList : Crdt ek a -> Crdt (ListK Movable ek a) (List a)
movableList (Crdt elem) =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asMov node of
                    Just ml ->
                        MoveList.toList ml
                            |> List.map elem.decode
                            |> combine

                    Nothing ->
                        Err (TypeMismatch "expected movable list")
        , empty = \ctx -> ( Node.mov MoveList.empty, ctx )
        , seed =
            \values ctx ->
                let
                    ( ml, ctx1, _ ) =
                        List.foldl
                            (\value ( acc, c, afterCell ) ->
                                let
                                    ( childNode, c1 ) =
                                        elem.seed value c

                                    ( vid, c2 ) =
                                        Id.nextId c1
                                in
                                ( MoveList.insert vid afterCell childNode acc, c2, Just vid )
                            )
                            ( MoveList.empty, ctx, Nothing )
                            values
                in
                ( Node.mov ml, ctx1 )
        }



-- TREE -----------------------------------------------------------------------


{-| A movable **tree** of `a`: hierarchical, re-parentable, sibling-ordered data
backed by `Crdt.Tree`. Reads as a `Crdt.Tree.Forest a` (ordered nested items with
stable ids). Edit it through the `Crdt` module (`addChild` / `moveInto` / `removeNode`).
A fresh tree is empty; nodes are added by ref, not seeded, so `seed` yields empty.
-}
tree : Crdt ek a -> Crdt (TreeK ek a) (Tree.Forest a)
tree (Crdt elem) =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                case Node.asTree node of
                    Just t ->
                        -- decode every node's payload; a decode error anywhere
                        -- fails the whole read (via the sentinel below)
                        decodeForest elem.decode t

                    Nothing ->
                        Err (TypeMismatch "expected tree")
        , empty = \ctx -> ( Node.tree Tree.empty, ctx )
        , seed = \_ ctx -> ( Node.tree Tree.empty, ctx )
        }


{-| Decode a tree's payloads through `dec`, failing the whole forest if any node's
payload fails (so a corrupt node surfaces as a `Result` error, not a silent drop).
-}
decodeForest : (Node -> Result Error a) -> Tree.Tree Node -> Result Error (Tree.Forest a)
decodeForest dec t =
    let
        -- collect the first decode error, if any, while building the forest
        forest =
            Tree.toForest (\n -> dec n |> Result.toMaybe) t

        anyError =
            Tree.payloads t
                |> Dict.values
                |> List.filterMap (\n -> dec n |> resultError)
                |> List.head
    in
    case anyError of
        Just e ->
            Err e

        Nothing ->
            Ok forest


resultError : Result e a -> Maybe e
resultError r =
    case r of
        Err e ->
            Just e

        Ok _ ->
            Nothing



-- DICT -----------------------------------------------------------------------


{-| A dictionary of string keys to `a`. Key presence is LWW, so concurrent
set/remove resolves by stamp. Reads back as a standard `Dict`, omitting removed
(tombstoned) keys.
-}
dict : Crdt vk a -> Crdt (DictK vk a) (Dict String a)
dict (Crdt val) =
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                Node.presentEntries node
                    |> List.map (\( k, v ) -> val.decode v |> Result.map (Tuple.pair k))
                    |> combine
                    |> Result.map Dict.fromList
        , empty = \ctx -> ( Node.mapFromEntries Dict.empty, ctx )
        , seed =
            \values ctx ->
                let
                    ( entries, ctx1 ) =
                        Dict.foldl
                            (\k v ( acc, c ) ->
                                let
                                    ( childNode, c1 ) =
                                        val.seed v c

                                    ( stamp, c2 ) =
                                        Id.nextId c1
                                in
                                ( Dict.insert k (Node.entry stamp True childNode) acc, c2 )
                            )
                            ( Dict.empty, ctx )
                            values
                in
                ( Node.mapFromEntries entries, ctx1 )
        }



-- OP-SET (user-defined CRDT) -------------------------------------------------


{-| A **user-defined operation-based CRDT** (see `docs/14`): a grow-only/removable set of
**contributions**, each written under its own op-id key, read by folding the present
contributions into a value. Convergence is free — merge is `Dict.union` of the keys, so
concurrent contributions from any replicas all survive — and the semantics is entirely
your `fold`. This is the shape of the built-in counter (op-keyed deltas, summed) opened up.

    -- a grow-only maximum register:
    maxReg : Crdt (OpSetK Int) Int
    maxReg =
        opSet { contribution = int, fold = List.maximum >> Maybe.withDefault 0 }

    -- a multi-value register (keeps all concurrent values):
    mvReg : Crdt (OpSetK a) (List a)
    mvReg =
        opSet { contribution = mySchema, fold = identity }

Edited only through `Crdt.contribute` (add a contribution) and `retract` (tombstone
one); it is not `set`-able, since its value is derived, not stored.

**Law:** `fold` must be a pure function of the _set_ of contributions — order-independent
and idempotent-friendly — or the read won't converge. `List.maximum`, `Set.fromList`,
`List.sum` are fine; anything depending on contribution order (e.g. "take the first") is
not. The node is a `Map`; contributions are its present entries.

-}
opSet : { contribution : Crdt ck c, fold : List c -> a } -> Crdt (OpSetK c) a
opSet config =
    let
        (Crdt contrib) =
            config.contribution
    in
    Crdt
        { whenAbsent = Nothing
        , decode =
            \node ->
                Node.presentEntries node
                    |> List.map (\( _, v ) -> contrib.decode v)
                    |> combine
                    |> Result.map config.fold
        , empty = \ctx -> ( Node.mapFromEntries Dict.empty, ctx )

        -- No wholesale seed: the value is derived from contributions, which can't be
        -- inverted from a folded `a`. A fresh op-set is empty; you build it with
        -- `contribute`. (Matches how `counter` seeds empty.)
        , seed = \_ ctx -> ( Node.mapFromEntries Dict.empty, ctx )
        }



-- RECORD BUILDER -------------------------------------------------------------


{-| In-progress record schema. Accumulates field decoders and seeders along with
the constructor function being applied.
-}
type RecordBuilder full a
    = RecordBuilder
        { decode : Node -> Result Error a
        , empty : Ctx -> List ( String, Node ) -> ( List ( String, Node ), Ctx )
        , seed : full -> Ctx -> List ( String, Node ) -> ( List ( String, Node ), Ctx )
        }


{-| Begin a record schema from its constructor.

    record Todo
        |> field "text" .text text
        |> field "done" .done bool
        |> build

-}
record : (a -> b) -> RecordBuilder full (a -> b)
record ctor =
    RecordBuilder
        { decode = \_ -> Ok ctor
        , empty = \ctx acc -> ( acc, ctx )
        , seed = \_ ctx acc -> ( acc, ctx )
        }


{-| Add a field: its key, a getter from the full record (for seeding), and the
field's own schema.
-}
field : String -> (full -> a) -> Crdt fk a -> RecordBuilder full (a -> b) -> RecordBuilder full b
field name getter schema rb =
    aliasedField name [] getter schema rb


{-| Add a field that also reads from **older names** (`aliases`), in order, when the
current `name` is absent — a schema-evolution rename (see docs/13). Reads always prefer
`name`; writes/seeds always go to `name`, so once a value is written the old key becomes
inert (a tolerated extra key). A v1 peer (which only knows the old name) and a v2 peer
(new name + alias) converge on the same `Node` and each reads it through its own names.

    -- renamed "complete" -> "done":
    |> aliasedField "done" [ "complete" ] .done S.bool

-}
aliasedField : String -> List String -> (full -> a) -> Crdt fk a -> RecordBuilder full (a -> b) -> RecordBuilder full b
aliasedField name aliases getter (Crdt fieldSchema) (RecordBuilder rb) =
    let
        -- Read whichever of the names (canonical + aliases) carries the HIGHEST-stamped
        -- entry — LWW across the rename. This is what makes a rename converge safely
        -- even though every replica base-seeds its own canonical name: a base seed is
        -- minted at `init` (a low stamp), so a real write to *any* name — old or new —
        -- outranks the seed, and the latest write across names wins (see docs/13).
        -- Falls back to the schema's `whenAbsent` (a `withDefault`/`optional` rename is
        -- tolerant on all sides), else `MissingField`.
        readField entries =
            case highestStamped (name :: aliases) entries of
                Just e ->
                    fieldSchema.decode e.value

                Nothing ->
                    case fieldSchema.whenAbsent of
                        Just fallback ->
                            Ok fallback

                        Nothing ->
                            Err (MissingField name)
    in
    RecordBuilder
        { decode =
            \node ->
                case Node.asMap node of
                    Just entries ->
                        Result.map2 (\f a -> f a) (rb.decode node) (readField entries)

                    Nothing ->
                        Err (TypeMismatch ("expected record for field " ++ name))
        , empty =
            \ctx acc ->
                let
                    ( accValues, ctx1 ) =
                        rb.empty ctx acc
                in
                -- An ALIASED field (a rename) or an absence-tolerant field
                -- (`optional`/`withDefault`) is left OUT of the empty base. For a rename
                -- this is essential: if the base seeded the canonical name, its stamp
                -- could TIE a peer's write to the old name on counter and win the
                -- replica-id tiebreak, masking the real value — so only real writes may
                -- carry a stamp for these names. A tolerant field reads its fallback
                -- until written. Required, non-aliased fields are still seeded so a fresh
                -- document satisfies its own reader. See docs/13.
                if not (List.isEmpty aliases) || fieldSchema.whenAbsent /= Nothing then
                    ( accValues, ctx1 )

                else
                    let
                        ( fieldNode, ctx2 ) =
                            fieldSchema.empty ctx1
                    in
                    ( ( name, fieldNode ) :: accValues, ctx2 )
        , seed =
            \full ctx acc ->
                let
                    ( accValues, ctx1 ) =
                        rb.seed full ctx acc

                    ( fieldNode, ctx2 ) =
                        fieldSchema.seed (getter full) ctx1
                in
                ( ( name, fieldNode ) :: accValues, ctx2 )
        }


{-| Among `names`, the present entry with the highest stamp (LWW across a rename's old
and new names), or `Nothing` if none is present. A base seed's stamp is minted low at
`init`, so a real write to any name outranks it; the latest real write wins.
-}
highestStamped : List String -> Dict String Node.Entry -> Maybe Node.Entry
highestStamped names entries =
    names
        |> List.filterMap (\n -> Dict.get n entries |> Maybe.andThen keepPresent)
        |> List.foldl
            (\e acc ->
                case acc of
                    Just best ->
                        if Id.compareOpId e.stamp best.stamp == GT then
                            Just e

                        else
                            acc

                    Nothing ->
                        Just e
            )
            Nothing


{-| An entry if it is present (not tombstoned), else `Nothing`.
-}
keepPresent : Node.Entry -> Maybe Node.Entry
keepPresent e =
    if e.present then
        Just e

    else
        Nothing


{-| Finish a record schema.
-}
build : RecordBuilder a a -> Crdt Nested a
build (RecordBuilder rb) =
    Crdt
        { whenAbsent = Nothing
        , decode = rb.decode
        , empty = \ctx -> rb.empty ctx [] |> stampEntries
        , seed = \value ctx -> rb.seed value ctx [] |> stampEntries
        }


{-| Turn a list of `(key, valueNode)` pairs into a present `Map`, minting a
distinct presence stamp for each entry so the document never holds duplicate
OpIds.
-}
stampEntries : ( List ( String, Node ), Ctx ) -> ( Node, Ctx )
stampEntries ( pairs, ctx ) =
    let
        ( entries, ctx1 ) =
            List.foldl
                (\( k, node ) ( acc, c ) ->
                    let
                        ( stamp, c1 ) =
                            Id.nextId c
                    in
                    ( Dict.insert k (Node.entry stamp True node) acc, c1 )
                )
                ( Dict.empty, ctx )
                pairs
    in
    ( Node.mapFromEntries entries, ctx1 )



-- CUSTOM (SUM) TYPES ---------------------------------------------------------
-- A sum type materializes to a `Map` with a reserved LWW `$tag` register naming
-- the active variant, plus one key per non-nullary variant holding a positional
-- payload map ("0".."N-1"). The active variant is LWW; each payload merges as its
-- own sub-CRDT. See `docs/06-sum-types.md`. This is schema sugar over `Map`+`Reg`
-- — the `Node`/merge core is untouched.


{-| A seeder that produces one variant's full custom node (tag + payload) given a
clock. Both `seed` (real value) and `empty` (default variant) build one of these.

**Opaque** (it wraps a `Ctx -> (Node, Ctx)`, and `Node` is package-internal) so it
can be _named_ in the ref-emitting sum builders (`Crdt.variant0`/`variant1`/…) without
leaking `Node` into the public API. You never construct one directly.

-}
type VariantSeed
    = VariantSeed (Ctx -> ( Node, Ctx ))


{-| Run a variant seeder against a clock.
-}
runVariantSeed : VariantSeed -> Ctx -> ( Node, Ctx )
runVariantSeed (VariantSeed f) =
    f


{-| In-progress custom (sum) type schema.

`match` is the caller's dispatcher, with one variant-encoder argument peeled off by
each `variantN` (the elm-codec trick: each `variantN`'s signature writes `match` as
a concrete arrow so the partially-applied dispatcher can be called). `value` is the
Elm custom type. After all variants are declared, `match` has shrunk to
`value -> VariantSeed`, which `buildCustom` uses as the seeder.

-}
type CustomBuilder match value
    = CustomBuilder
        { match : match
        , decoders : Dict String (Node -> Result Error value)
        , default : Maybe VariantSeed

        -- Schema-evolution fallback (see docs/13): how to read a `$tag` that names no
        -- declared variant — the forward-compatibility case where a peer on a newer
        -- schema wrote a variant this (older) schema doesn't know. `Nothing` = unknown
        -- tags are a `BadValue` error (the default); `Just f` = read them as `f tag`
        -- (set by `catchAll`), so an added variant doesn't break old peers.
        , catchAll : Maybe (String -> value)
        }


{-| Begin a custom (sum) type schema from a **dispatcher**: a function that receives
one handler per variant (in declaration order) plus the value to match, and returns
the handler applied to that variant's payload. Elm can't reflect on a custom type,
so you supply the `case`:

    statusSchema : Crdt Status
    statusSchema =
        custom
            (\active snoozed done value ->
                case value of
                    Active ->
                        active

                    Snoozed t ->
                        snoozed t

                    Done note ->
                        done note
            )
            |> variant0 "active" Active
            |> variant1 "snoozed" Snoozed int
            |> variant1 "done" Done text
            |> buildCustom

The first variant declared is the **default** (used to build a fresh document).

-}
custom : match -> CustomBuilder match value
custom match =
    CustomBuilder { match = match, decoders = Dict.empty, default = Nothing, catchAll = Nothing }


{-| Declare a **catch-all variant** for schema evolution (see `docs/13`): a variant of
your Elm type, carrying the raw `$tag` string, that unknown tags decode to instead of
failing the read with `BadValue`. This makes adding a variant **forward-compatible** — a
peer on an older schema reads a newer peer's variant as `Unknown "the-tag"` rather than
erroring.

Like `variant1`, it consumes one dispatcher handler (`String -> VariantSeed`) — the
catch-all is a real variant of your type, so `match` must handle it. Declare it once:

    custom
        (\active done unknown v ->
            case v of
                Active ->
                    active

                Done note ->
                    done note

                Unknown tag ->
                    unknown tag
        )
        |> variant0 "active" Active
        |> variant1 "done" Done text
        |> catchAll Unknown
        |> buildCustom

`ctor` is your constructor (`Unknown : String -> value`). Seeding/switching to
`Unknown t` writes `$tag = t` with **no payload** — so the tag is _preserved_ on the
wire and a newer peer still reads its real variant. (Conservation caveat: an old peer
that switches away from an unknown value can't reproduce its payload — documented in
`docs/13`. Merely holding the value loses nothing, since it isn't rewritten.)

-}
catchAll : (String -> value) -> CustomBuilder ((String -> VariantSeed) -> b) value -> CustomBuilder b value
catchAll ctor (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (\tag -> seedCustomNode tag [])
        , decoders = cb.decoders

        -- The catch-all never supplies the fresh-document default: it contributes no
        -- tag of its own, and a real variant declared before it already set the default
        -- (via `keepFirst`). A custom type whose ONLY variant is a catch-all is
        -- degenerate — it then has no default, so a fresh document has no `$tag` and
        -- fails to read (`MissingField`); declare at least one real variant first.
        , default = cb.default
        , catchAll = Just ctor
        }


{-| A nullary variant: its tag and its Elm value (e.g. `Active`).
-}
variant0 : String -> value -> CustomBuilder (VariantSeed -> b) value -> CustomBuilder b value
variant0 name ctorValue (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (seedCustomNode name [])
        , decoders = Dict.insert name (\_ -> Ok ctorValue) cb.decoders
        , default = keepFirst cb.default (seedCustomNode name [])
        , catchAll = cb.catchAll
        }


{-| A one-argument variant: its tag, its Elm constructor, and the payload schema.
-}
variant1 :
    String
    -> (t1 -> value)
    -> Crdt k1 t1
    -> CustomBuilder ((t1 -> VariantSeed) -> b) value
    -> CustomBuilder b value
variant1 name ctor (Crdt s1) (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (\a1 -> seedCustomNode name [ s1.seed a1 ])
        , decoders = Dict.insert name (\p -> Result.map ctor (arg 0 s1 p)) cb.decoders
        , default = keepFirst cb.default (seedCustomNode name [ s1.empty ])
        , catchAll = cb.catchAll
        }


{-| A two-argument variant.
-}
variant2 :
    String
    -> (t1 -> t2 -> value)
    -> Crdt k1 t1
    -> Crdt k2 t2
    -> CustomBuilder ((t1 -> t2 -> VariantSeed) -> b) value
    -> CustomBuilder b value
variant2 name ctor (Crdt s1) (Crdt s2) (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (\a1 a2 -> seedCustomNode name [ s1.seed a1, s2.seed a2 ])
        , decoders = Dict.insert name (\p -> Result.map2 ctor (arg 0 s1 p) (arg 1 s2 p)) cb.decoders
        , default = keepFirst cb.default (seedCustomNode name [ s1.empty, s2.empty ])
        , catchAll = cb.catchAll
        }


{-| A three-argument variant.
-}
variant3 :
    String
    -> (t1 -> t2 -> t3 -> value)
    -> Crdt k1 t1
    -> Crdt k2 t2
    -> Crdt k3 t3
    -> CustomBuilder ((t1 -> t2 -> t3 -> VariantSeed) -> b) value
    -> CustomBuilder b value
variant3 name ctor (Crdt s1) (Crdt s2) (Crdt s3) (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (\a1 a2 a3 -> seedCustomNode name [ s1.seed a1, s2.seed a2, s3.seed a3 ])
        , decoders =
            Dict.insert name
                (\p -> Result.map3 ctor (arg 0 s1 p) (arg 1 s2 p) (arg 2 s3 p))
                cb.decoders
        , default = keepFirst cb.default (seedCustomNode name [ s1.empty, s2.empty, s3.empty ])
        , catchAll = cb.catchAll
        }


{-| Finish a custom (sum) type schema.
-}
buildCustom : CustomBuilder (value -> VariantSeed) value -> Crdt (Variants value) value
buildCustom (CustomBuilder cb) =
    Crdt
        { whenAbsent = Nothing
        , decode = decodeCustom cb.decoders cb.catchAll
        , empty =
            case cb.default of
                Just vs ->
                    runVariantSeed vs

                Nothing ->
                    \ctx -> ( Node.mapFromEntries Dict.empty, ctx )
        , seed = \value ctx -> runVariantSeed (cb.match value) ctx
        }


{-| Keep the earlier-declared default (the first variant); take the new one only if
none set yet.
-}
keepFirst : Maybe VariantSeed -> VariantSeed -> Maybe VariantSeed
keepFirst existing new =
    case existing of
        Just _ ->
            existing

        Nothing ->
            Just new


{-| The reserved key naming the active variant.
-}
tagKey : String
tagKey =
    "$tag"


{-| Escape a variant name for use as a payload key, so it can never collide with
`$tag`: any name already starting with `$` gets one more `$` prepended (reversible;
`$tag` itself would be stored as `$$tag`).
-}
escapeVariant : String -> String
escapeVariant name =
    if String.startsWith "$" name then
        "$" ++ name

    else
        name


{-| The payload-map key a variant's node is stored under (the escaped variant
name). Exposed so the `Crdt` module can build a path to a variant's payload without
re-deriving the escape rule.
-}
variantArgKey : String -> String
variantArgKey =
    escapeVariant


{-| Build one variant's node: an LWW `$tag` register plus, for a non-nullary
variant, a positional payload map ("0".."N-1"). Nullary variants omit the payload
key entirely (D4). Each map entry gets a fresh presence stamp; the tag register and
each payload value carry their own ids, so no OpId is ever duplicated.
-}
seedCustomNode : String -> List (Ctx -> ( Node, Ctx )) -> VariantSeed
seedCustomNode name argSeeders =
    VariantSeed (seedCustomNodeRaw name argSeeders)


seedCustomNodeRaw : String -> List (Ctx -> ( Node, Ctx )) -> Ctx -> ( Node, Ctx )
seedCustomNodeRaw name argSeeders ctx =
    let
        ( tagId, ctx1 ) =
            Id.nextId ctx

        ( tagStamp, ctx2 ) =
            Id.nextId ctx1

        tagEntry =
            Node.entry tagStamp True (Node.reg (PString name) tagId)
    in
    if List.isEmpty argSeeders then
        ( Node.mapFromEntries (Dict.singleton tagKey tagEntry), ctx2 )

    else
        let
            ( argEntries, ctx3 ) =
                List.foldl
                    (\seeder ( acc, c, i ) ->
                        let
                            ( argNode, cA ) =
                                seeder c

                            ( argStamp, cB ) =
                                Id.nextId cA
                        in
                        ( Dict.insert (String.fromInt i) (Node.entry argStamp True argNode) acc, cB, i + 1 )
                    )
                    ( Dict.empty, ctx2, 0 )
                    argSeeders
                    |> (\( a, c, _ ) -> ( a, c ))

            ( payloadStamp, ctx4 ) =
                Id.nextId ctx3

            payloadEntry =
                Node.entry payloadStamp True (Node.mapFromEntries argEntries)

            top =
                Dict.fromList [ ( tagKey, tagEntry ), ( escapeVariant name, payloadEntry ) ]
        in
        ( Node.mapFromEntries top, ctx4 )


{-| Decode a custom-type node: read `$tag`, dispatch to that variant's decoder,
handing it the variant's payload map (an empty map for a nullary variant). An unknown
tag routes to the `catchAll` fallback if the schema declared one, else it is a
`BadValue` error (see `docs/13`).
-}
decodeCustom : Dict String (Node -> Result Error value) -> Maybe (String -> value) -> Node -> Result Error value
decodeCustom decoders fallback node =
    case Node.asMap node of
        Nothing ->
            Err (TypeMismatch "expected custom-type map")

        Just entries ->
            case Dict.get tagKey entries of
                Nothing ->
                    Err (MissingField tagKey)

                Just tagEntry ->
                    case Node.asPrim tagEntry.value of
                        Just (PString name) ->
                            case Dict.get name decoders of
                                Just dec ->
                                    Dict.get (escapeVariant name) entries
                                        |> Maybe.map .value
                                        |> Maybe.withDefault (Node.mapFromEntries Dict.empty)
                                        |> dec

                                Nothing ->
                                    case fallback of
                                        Just toValue ->
                                            Ok (toValue name)

                                        Nothing ->
                                            Err (UnknownVariant name)

                        _ ->
                            Err (BadValue "custom $tag is not a string")


{-| Pull positional argument `i` out of a payload map and decode it through `s`.
-}
arg : Int -> { d | decode : Node -> Result Error t } -> Node -> Result Error t
arg i s payload =
    case Node.asMap payload of
        Just entries ->
            case Dict.get (String.fromInt i) entries of
                Just e ->
                    s.decode e.value

                Nothing ->
                    Err (MissingField ("variant argument " ++ String.fromInt i))

        Nothing ->
            Err (TypeMismatch "expected variant payload")



-- HELPERS --------------------------------------------------------------------


combine : List (Result e a) -> Result e (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])
