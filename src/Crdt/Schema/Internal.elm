module Crdt.Schema.Internal exposing
    ( Crdt, Error(..), Seed
    , Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK
    , int, float, string, bool, text, counter, lww
    , list, movableList, dict
    , record, field, build, RecordBuilder
    , CustomBuilder, VariantSeed, custom, variant0, variant1, variant2, variant3, buildCustom
    , variantArgKey
    , with, decodeNode, emptyNode, errorToString
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
state — that is the hard diff problem. All in-place mutation goes through
`Crdt.Edit`, which is decoupled from this layer.

@docs Crdt, Error, Seed
@docs Settable, Counter, Nested, Variants, ListK, Fixed, Movable, DictK
@docs int, float, string, bool, text, counter, lww
@docs list, movableList, dict
@docs record, field, build, RecordBuilder
@docs CustomBuilder, VariantSeed, custom, variant0, variant1, variant2, variant3, buildCustom
@docs variantArgKey
@docs with, decodeNode, emptyNode, errorToString

-}

import Crdt.Id as Id exposing (Ctx)
import Crdt.Internal as I
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Crdt.Text as Text
import Dict exposing (Dict)


{-| An opaque builder of a fresh subtree from a value, produced by `with` and
consumed by the edit APIs. Re-exported from `Crdt.Internal` so the edit APIs can
name it without leaking the internal `Node` type.
-}
type alias Seed =
    I.Seed


{-| A schema tying a typed value `a` to CRDT `Node` state, tagged with a phantom
`kind` describing how it may be _edited_ (used by `Crdt.Ref` to reject nonsensical
edits at compile time — e.g. `increment` on text). The `kind` never affects reads
or merge; it is erased at runtime.
-}
type Crdt kind a
    = Crdt
        { decode : Node -> Result Error a
        , empty : Ctx -> ( Node, Ctx )
        , seed : a -> Ctx -> ( Node, Ctx )
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


{-| What can go wrong reading a `Node` through a schema.
-}
type Error
    = TypeMismatch String
    | MissingField String
    | BadValue String


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



-- DRIVERS (used by Crdt.elm / Crdt.Edit) -------------------------------------


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


{-| Seed a node from a value, producing an opaque `Seed` that `Crdt.Edit` /
`Crdt.OpDoc` pass to `listAppend` / `setKey`.

    todoSchema |> S.with (Todo "pack" False)

-}
with : a -> Crdt kind a -> Seed
with value (Crdt c) =
    I.Seed (c.seed value)



-- PRIMITIVES -----------------------------------------------------------------


primReg : (Prim -> Result Error a) -> (a -> Prim) -> Prim -> Crdt Settable a
primReg fromPrim toPrim emptyPrim =
    Crdt
        { decode =
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


{-| An explicit LWW marker. Primitives are already last-write-wins, so this is
the identity — provided for readable schemas.
-}
lww : Crdt kind a -> Crdt kind a
lww =
    identity



-- COUNTER --------------------------------------------------------------------


{-| A PN-counter, read as its integer total. Unlike an `int` register (which is
last-write-wins, so concurrent `+1`/`+1` collapses to 1), concurrent increments
from different replicas **sum** — `+1` and `+1` give 2. Use `Crdt.Edit.increment`
/ `Crdt.OpDoc.increment` to change it.
-}
counter : Crdt Counter Int
counter =
    Crdt
        { decode =
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
        { decode =
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



-- LIST -----------------------------------------------------------------------


{-| An ordered list of `a`, backed by an RGA. Concurrent inserts from different
replicas all survive and converge to a deterministic order.
-}
list : Crdt ek a -> Crdt (ListK Fixed ek a) (List a)
list (Crdt elem) =
    Crdt
        { decode =
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
`Crdt.OpDoc.listMove` and keep their identity (nested edits and cursors follow a
moved item). Backed by `Crdt.MoveList`. Reads as a plain `List a` in order.
-}
movableList : Crdt ek a -> Crdt (ListK Movable ek a) (List a)
movableList (Crdt elem) =
    Crdt
        { decode =
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



-- DICT -----------------------------------------------------------------------


{-| A dictionary of string keys to `a`. Key presence is LWW, so concurrent
set/remove resolves by stamp. Reads back as a standard `Dict`, omitting removed
(tombstoned) keys.
-}
dict : Crdt vk a -> Crdt (DictK vk a) (Dict String a)
dict (Crdt val) =
    Crdt
        { decode =
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
field name getter (Crdt fieldSchema) (RecordBuilder rb) =
    RecordBuilder
        { decode =
            \node ->
                case Node.asMap node of
                    Just entries ->
                        let
                            fieldResult =
                                case Dict.get name entries of
                                    Just e ->
                                        fieldSchema.decode e.value

                                    Nothing ->
                                        Err (MissingField name)
                        in
                        Result.map2 (\f a -> f a) (rb.decode node) fieldResult

                    Nothing ->
                        Err (TypeMismatch ("expected record for field " ++ name))
        , empty =
            \ctx acc ->
                let
                    ( accValues, ctx1 ) =
                        rb.empty ctx acc

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


{-| Finish a record schema.
-}
build : RecordBuilder a a -> Crdt Nested a
build (RecordBuilder rb) =
    Crdt
        { decode = rb.decode
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
can be _named_ in the ref-emitting sum builders (`Crdt.Ref.variantNR`) without
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
    CustomBuilder { match = match, decoders = Dict.empty, default = Nothing }


{-| A nullary variant: its tag and its Elm value (e.g. `Active`).
-}
variant0 : String -> value -> CustomBuilder (VariantSeed -> b) value -> CustomBuilder b value
variant0 name ctorValue (CustomBuilder cb) =
    CustomBuilder
        { match = cb.match (seedCustomNode name [])
        , decoders = Dict.insert name (\_ -> Ok ctorValue) cb.decoders
        , default = keepFirst cb.default (seedCustomNode name [])
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
        }


{-| Finish a custom (sum) type schema.
-}
buildCustom : CustomBuilder (value -> VariantSeed) value -> Crdt (Variants value) value
buildCustom (CustomBuilder cb) =
    Crdt
        { decode = decodeCustom cb.decoders
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
name). Exposed so `Crdt.Ref` can build a path to a variant's payload without
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
handing it the variant's payload map (an empty map for a nullary variant).
-}
decodeCustom : Dict String (Node -> Result Error value) -> Node -> Result Error value
decodeCustom decoders node =
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
                                    Err (BadValue ("unknown variant: " ++ name))

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
