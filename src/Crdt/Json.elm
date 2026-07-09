module Crdt.Json exposing
    ( encodeMarkAnchor
    , encodeNode
    , encodeOpId
    , encodePrim
    , encodeSide
    , markAnchorDecoder
    , nodeDecoder
    , opIdDecoder
    , primDecoder
    , sideDecoder
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

import Crdt.Frac as Frac
import Crdt.Id as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Element, Entry, Node, Prim(..))
import Crdt.Rga as Rga
import Crdt.Tree as Tree
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

        Node.Tree t ->
            JE.object
                [ ( "t", JE.string "tree" )
                , ( "moves", JE.dict identity encodeMove (Tree.moves t) )
                , ( "vals", JE.dict identity encodeNode (Tree.payloads t) )
                , ( "del", JE.list JE.string (Set.toList (Tree.deletedIds t)) )
                ]

        Node.Rich r ->
            JE.object
                [ ( "t", JE.string "rich" )
                , ( "el", JE.list encodeElement (Rga.elements r.text) )
                , ( "marks", JE.dict identity encodeMarkOp r.marks )
                ]


{-| A mark op: id, type, value (a prim), and its two range anchors.
-}
encodeMarkOp : Node.MarkOp -> JE.Value
encodeMarkOp m =
    JE.object
        [ ( "id", encodeOpId m.id )
        , ( "ty", JE.string m.type_ )
        , ( "v", encodePrim m.value )
        , ( "st", encodeMarkAnchor m.start )
        , ( "en", encodeMarkAnchor m.end )
        ]


encodeMarkAnchor : Node.MarkAnchor -> JE.Value
encodeMarkAnchor a =
    JE.object
        [ ( "r"
          , case a.ref of
                Just id ->
                    encodeOpId id

                Nothing ->
                    JE.null
          )
        , ( "sd", encodeAnchorSide a.side )
        ]


encodeAnchorSide : Node.AnchorSide -> JE.Value
encodeAnchorSide side =
    case side of
        Node.Before ->
            JE.string "b"

        Node.After ->
            JE.string "a"


{-| A tree move record: the move op, the child, its parent (null = root), and its
fractional sibling position (a digit list).
-}
encodeMove : Tree.Move -> JE.Value
encodeMove m =
    JE.object
        [ ( "op", encodeOpId m.moveOp )
        , ( "c", encodeOpId m.child )
        , ( "p"
          , case m.parent of
                Just p ->
                    encodeOpId p

                Nothing ->
                    JE.null
          )
        , ( "pos", JE.list JE.int (Frac.toList m.pos) )
        ]


{-| A position cell of a movable list: its id, anchor, and the valueId it carries.
-}
encodeCell : Rga.Element OpId -> JE.Value
encodeCell cell =
    -- A MoveList cell is always a right-child (structural ordering), so its `side`
    -- is not serialized; the decoder reconstructs it as `Right`.
    JE.object
        [ ( "id", encodeOpId cell.id )
        , ( "o"
          , case cell.parent of
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
        , ( "p"
          , case el.parent of
                Just p ->
                    encodeOpId p

                Nothing ->
                    JE.null
          )
        , ( "s", encodeSide el.side )
        , ( "c", encodeNode el.content )
        , ( "d", JE.bool el.deleted )
        ]


encodeSide : Rga.Side -> JE.Value
encodeSide side =
    case side of
        Rga.Left ->
            JE.string "L"

        Rga.Right ->
            JE.string "R"


sideDecoder : Decoder Rga.Side
sideDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "L" ->
                        JD.succeed Rga.Left

                    "R" ->
                        JD.succeed Rga.Right

                    _ ->
                        JD.fail ("unknown side: " ++ s)
            )


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

                    "tree" ->
                        JD.map3
                            (\ms vs del ->
                                Node.tree (Tree.fromParts ms vs (Set.fromList del))
                            )
                            (JD.field "moves" (JD.dict moveDecoder))
                            (JD.field "vals" (JD.dict (JD.lazy (\_ -> nodeDecoder))))
                            (JD.field "del" (JD.list JD.string))

                    "rich" ->
                        JD.map2
                            (\els marks -> Node.rich { text = Rga.fromElements els, marks = marks })
                            (JD.field "el" (JD.list elementDecoder))
                            (JD.field "marks" (JD.dict markOpDecoder))

                    other ->
                        JD.fail ("unknown node tag: " ++ other)
            )


cellDecoder : Decoder (Rga.Element OpId)
cellDecoder =
    -- cells are always right-children (structural), so reconstruct `side = Right`.
    JD.map3 (\id parent valueId -> Rga.element id parent Rga.Right valueId False)
        (JD.field "id" opIdDecoder)
        (JD.field "o" (JD.nullable opIdDecoder))
        (JD.field "v" opIdDecoder)


moveDecoder : Decoder Tree.Move
moveDecoder =
    JD.map4 (\op c p pos -> { moveOp = op, child = c, parent = p, pos = Frac.fromList pos })
        (JD.field "op" opIdDecoder)
        (JD.field "c" opIdDecoder)
        (JD.field "p" (JD.nullable opIdDecoder))
        (JD.field "pos" (JD.list JD.int))


markOpDecoder : Decoder Node.MarkOp
markOpDecoder =
    JD.map5 (\id ty v st en -> { id = id, type_ = ty, value = v, start = st, end = en })
        (JD.field "id" opIdDecoder)
        (JD.field "ty" JD.string)
        (JD.field "v" primDecoder)
        (JD.field "st" markAnchorDecoder)
        (JD.field "en" markAnchorDecoder)


markAnchorDecoder : Decoder Node.MarkAnchor
markAnchorDecoder =
    JD.map2 (\r sd -> { ref = r, side = sd })
        (JD.field "r" (JD.nullable opIdDecoder))
        (JD.field "sd" anchorSideDecoder)


anchorSideDecoder : Decoder Node.AnchorSide
anchorSideDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "b" ->
                        JD.succeed Node.Before

                    "a" ->
                        JD.succeed Node.After

                    _ ->
                        JD.fail ("unknown anchor side: " ++ s)
            )


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
    JD.map5 Rga.element
        (JD.field "id" opIdDecoder)
        (JD.field "p" (JD.nullable opIdDecoder))
        (JD.field "s" sideDecoder)
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
