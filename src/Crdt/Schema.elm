module Crdt.Schema exposing
    ( Crdt, Error(..)
    , int, float, string, bool, text, lww
    , list, dict
    , record, field, build
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

@docs Crdt, Error
@docs int, float, string, bool, text, lww
@docs list, dict
@docs record, field, build
@docs with, decodeNode, emptyNode, errorToString

-}

import Crdt.Id as Id exposing (Ctx)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Crdt.Text as Text
import Dict exposing (Dict)


{-| A schema tying a typed value `a` to CRDT `Node` state.
-}
type Crdt a
    = Crdt
        { decode : Node -> Result Error a
        , empty : Ctx -> ( Node, Ctx )
        , seed : a -> Ctx -> ( Node, Ctx )
        }


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
decodeNode : Crdt a -> Node -> Result Error a
decodeNode (Crdt c) =
    c.decode


{-| The empty node for a schema.
-}
emptyNode : Crdt a -> Ctx -> ( Node, Ctx )
emptyNode (Crdt c) =
    c.empty


{-| Seed a node from a value, producing a `Seed` (the thunk `Crdt.Edit` and the
demo pass to `listAppend` / `setKey`). Minting fresh ids requires a context, so
the result is a function of `Ctx`.

    todoSchema |> S.with (Todo "pack" False)

-}
with : a -> Crdt a -> (Ctx -> ( Node, Ctx ))
with value (Crdt c) =
    c.seed value



-- PRIMITIVES -----------------------------------------------------------------


primReg : (Prim -> Result Error a) -> (a -> Prim) -> Prim -> Crdt a
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
int : Crdt Int
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
float : Crdt Float
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
string : Crdt String
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
bool : Crdt Bool
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
lww : Crdt a -> Crdt a
lww =
    identity



-- TEXT -----------------------------------------------------------------------


{-| Collaborative text, read as a `String`. Backed by an RGA of characters so
concurrent edits merge character-wise.
-}
text : Crdt String
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
list : Crdt a -> Crdt (List a)
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



-- DICT -----------------------------------------------------------------------


{-| A dictionary of string keys to `a`. Key presence is LWW, so concurrent
set/remove resolves by stamp. Reads back as a standard `Dict`, omitting removed
(tombstoned) keys.
-}
dict : Crdt a -> Crdt (Dict String a)
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
field : String -> (full -> a) -> Crdt a -> RecordBuilder full (a -> b) -> RecordBuilder full b
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
build : RecordBuilder a a -> Crdt a
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



-- HELPERS --------------------------------------------------------------------


combine : List (Result e a) -> Result e (List a)
combine =
    List.foldr (Result.map2 (::)) (Ok [])
