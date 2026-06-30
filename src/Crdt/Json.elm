module Crdt.Json exposing
    ( encodeNode
    , encodeOpId
    , encodePrim
    , nodeDecoder
    , opIdDecoder
    , primDecoder
    )

{-| Lossless JSON serialization of the `Node` tree, including every `OpId`,
presence cell and tombstone. This is what the demo ships over the WebSocket;
nothing about ordering or causality may be dropped, or convergence breaks.

The wire shape tags each node with a `t` field:

  - register: `{ "t": "reg", "v": <prim>, "s": <opid> }`
  - map: `{ "t": "map", "e": { key: {v, present, s}, ... } }`
  - seq / txt: `{ "t": "seq"|"txt", "el": [ {id, origin, content, deleted}, ... ] }`

`OpId`s serialize as `[counter, replica]`; primitives carry a type tag so ints
and floats survive the roundtrip distinctly.

-}

import Crdt.Id as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Element, Entry, Node, Prim(..))
import Crdt.Rga as Rga
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Set



-- ENCODE ---------------------------------------------------------------------


encodeNode : Node -> JE.Value
encodeNode node =
    case node of
        Node.Reg r ->
            JE.object
                [ ( "t", JE.string "reg" )
                , ( "v", encodePrim r.value )
                , ( "s", encodeOpId r.stamp )
                ]

        Node.Map entries ->
            JE.object
                [ ( "t", JE.string "map" )
                , ( "e", JE.dict identity encodeEntry entries )
                ]

        Node.Seq rga ->
            JE.object
                [ ( "t", JE.string "seq" )
                , ( "el", JE.list encodeElement (Rga.elements rga) )
                ]

        Node.Txt rga ->
            JE.object
                [ ( "t", JE.string "txt" )
                , ( "el", JE.list encodeElement (Rga.elements rga) )
                ]

        Node.Cnt contributions ->
            JE.object
                [ ( "t", JE.string "cnt" )
                , ( "c", JE.dict identity encodeIncrement contributions )
                ]

        Node.Mov ml ->
            JE.object
                [ ( "t", JE.string "mov" )
                , ( "cells", JE.list encodeCell (Rga.elements (MoveList.cells ml)) )
                , ( "vals", JE.dict identity encodeNode (MoveList.values ml) )
                , ( "del", JE.list JE.string (Set.toList (MoveList.deletedIds ml)) )
                ]


{-| A position cell of a movable list: its id, anchor, and the valueId it carries.
-}
encodeCell : Rga.Element OpId -> JE.Value
encodeCell cell =
    JE.object
        [ ( "id", encodeOpId cell.id )
        , ( "o"
          , case cell.origin of
                Just o ->
                    encodeOpId o

                Nothing ->
                    JE.null
          )
        , ( "v", encodeOpId cell.content )
        ]


encodeIncrement : Node.Increment -> JE.Value
encodeIncrement inc =
    JE.object
        [ ( "s", encodeOpId inc.stamp )
        , ( "d", JE.int inc.delta )
        ]


encodeEntry : Entry -> JE.Value
encodeEntry e =
    JE.object
        [ ( "v", encodeNode e.value )
        , ( "p", JE.bool e.present )
        , ( "s", encodeOpId e.stamp )
        ]


encodeElement : Element -> JE.Value
encodeElement el =
    JE.object
        [ ( "id", encodeOpId el.id )
        , ( "o"
          , case el.origin of
                Just o ->
                    encodeOpId o

                Nothing ->
                    JE.null
          )
        , ( "c", encodeNode el.content )
        , ( "d", JE.bool el.deleted )
        ]


encodeOpId : OpId -> JE.Value
encodeOpId id =
    JE.list identity
        [ JE.int (Id.opIdCounter id)
        , JE.string (Id.toString (Id.opIdReplica id))
        ]


encodePrim : Prim -> JE.Value
encodePrim prim =
    case prim of
        PNull ->
            JE.object [ ( "k", JE.string "null" ) ]

        PBool b ->
            JE.object [ ( "k", JE.string "bool" ), ( "x", JE.bool b ) ]

        PInt n ->
            JE.object [ ( "k", JE.string "int" ), ( "x", JE.int n ) ]

        PFloat n ->
            JE.object [ ( "k", JE.string "float" ), ( "x", JE.float n ) ]

        PString s ->
            JE.object [ ( "k", JE.string "string" ), ( "x", JE.string s ) ]



-- DECODE ---------------------------------------------------------------------


nodeDecoder : Decoder Node
nodeDecoder =
    JD.field "t" JD.string
        |> JD.andThen
            (\tag ->
                case tag of
                    "reg" ->
                        JD.map2 (\v s -> Node.reg v s)
                            (JD.field "v" primDecoder)
                            (JD.field "s" opIdDecoder)

                    "map" ->
                        JD.field "e" (JD.dict entryDecoder)
                            |> JD.map Node.mapFromEntries

                    "seq" ->
                        JD.field "el" (JD.list elementDecoder)
                            |> JD.map (Rga.fromElements >> Node.seq)

                    "txt" ->
                        JD.field "el" (JD.list elementDecoder)
                            |> JD.map (Rga.fromElements >> Node.txt)

                    "cnt" ->
                        JD.field "c" (JD.dict incrementDecoder)
                            |> JD.map Node.counter

                    "mov" ->
                        JD.map3
                            (\cs vs del ->
                                Node.mov (MoveList.fromParts (Rga.fromElements cs) vs (Set.fromList del))
                            )
                            (JD.field "cells" (JD.list cellDecoder))
                            (JD.field "vals" (JD.dict (JD.lazy (\_ -> nodeDecoder))))
                            (JD.field "del" (JD.list JD.string))

                    other ->
                        JD.fail ("unknown node tag: " ++ other)
            )


cellDecoder : Decoder (Rga.Element OpId)
cellDecoder =
    JD.map3 (\id origin valueId -> Rga.element id origin valueId False)
        (JD.field "id" opIdDecoder)
        (JD.field "o" (JD.nullable opIdDecoder))
        (JD.field "v" opIdDecoder)


entryDecoder : Decoder Entry
entryDecoder =
    JD.map3 (\v p s -> { value = v, present = p, stamp = s })
        (JD.field "v" (JD.lazy (\_ -> nodeDecoder)))
        (JD.field "p" JD.bool)
        (JD.field "s" opIdDecoder)


incrementDecoder : Decoder Node.Increment
incrementDecoder =
    JD.map2 Node.increment
        (JD.field "s" opIdDecoder)
        (JD.field "d" JD.int)


elementDecoder : Decoder Element
elementDecoder =
    JD.map4 Rga.element
        (JD.field "id" opIdDecoder)
        (JD.field "o" (JD.nullable opIdDecoder))
        (JD.field "c" (JD.lazy (\_ -> nodeDecoder)))
        (JD.field "d" JD.bool)


opIdDecoder : Decoder OpId
opIdDecoder =
    JD.map2 (\c r -> Id.opId c (Id.replica r))
        (JD.index 0 JD.int)
        (JD.index 1 JD.string)


primDecoder : Decoder Prim
primDecoder =
    JD.field "k" JD.string
        |> JD.andThen
            (\kind ->
                case kind of
                    "null" ->
                        JD.succeed PNull

                    "bool" ->
                        JD.map PBool (JD.field "x" JD.bool)

                    "int" ->
                        JD.map PInt (JD.field "x" JD.int)

                    "float" ->
                        JD.map PFloat (JD.field "x" JD.float)

                    "string" ->
                        JD.map PString (JD.field "x" JD.string)

                    other ->
                        JD.fail ("unknown prim kind: " ++ other)
            )
