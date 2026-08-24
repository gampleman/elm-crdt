module Crdt.Json exposing
    ( blockTokenDecoder
    , encodeBlockToken
    , encodeMarkAnchor
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
presence cell and tombstone. It serializes the `Node` values that travel inside the
op wire format (`Crdt.OpJson`) — insert/presence/tree seeds — plus the compacted
`base` node a snapshot payload carries (see `Crdt.Doc.Internal`). Nothing about
ordering or causality may be dropped, or convergence breaks.

The wire shape tags each node with a `t` field:

  - register: `{ "t": "reg", "v": <prim>, "s": <opid> }`
  - map: `{ "t": "map", "e": { key: {v, p, s}, ... } }` — `p` = present, `s` = stamp
  - seq / txt: `{ "t": "seq"|"txt", "el": [ {id, p, s, c, d}, ... ] }` — per element
    `p` = parent anchor, `s` = side, `c` = content, `d` = deleted (see `Crdt.Rga`)
  - counter: `{ "t": "cnt", "c": { opid: {s, d}, ... } }`
  - movable list: `{ "t": "mov", "cells": [...], "vals": {...}, "del": [...] }`
  - tree: `{ "t": "tree", "moves": {...}, "vals": {...}, "del": [...] }`
  - rich text: `{ "t": "rich", "el": [...], "marks": {...} }`

`OpId`s serialize as `[counter, replica]`; primitives carry a type tag so ints
and floats survive the roundtrip distinctly.

-}

import Crdt.Frac as Frac
import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Entry, Node, Prim(..))
import Crdt.Rga as Rga
import Crdt.Tree.Internal as Tree
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
                , ( "el", JE.list (encodeElement encodeNode) (Rga.elements rga) )
                ]

        Node.Txt rga ->
            JE.object
                [ ( "t", JE.string "txt" )
                , ( "el", JE.list (encodeElement JE.string) (Rga.elements rga) )
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
                , ( "el", JE.list (encodeElement encodeRichElem) (Rga.elements r.text) )
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

`side` and `deleted` are **not serialized** — a cell is always a live right-child, so
both are constants (see the `cellRga` note on `Crdt.MoveList.MoveList`) and `cellsDecoder`
reconstructs them through the same `Rga.putRight` that wrote them.

-}
encodeCell : Rga.Element OpId -> JE.Value
encodeCell cell =
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


{-| An RGA element, with its **content encoded by the caller**. Each sequence container
holds a different content type (`design-docs/16-typed-sequence-content.md`): a `Seq` element
is a whole `Node`, a `Txt` element one character, a `Rich` element a `RichElem`. Only the
ordering fields — id, Fugue anchor, side, tombstone — are shared, and they are what this
writes.
-}
encodeElement : (c -> JE.Value) -> Rga.Element c -> JE.Value
encodeElement encodeContent el =
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
        , ( "c", encodeContent el.content )
        , ( "d", JE.bool el.deleted )
        ]


{-| A rich-text element's content. A character is `{"c":"a"}`; the two structural tokens are
`{"tk":"m"}` (block marker) and `{"tk":"n"}` (nest/indent token). Tagged rather than
positional so the three are distinguishable without knowing the vocabulary — these used to
be a whole register node carrying a magic `PInt`.
-}
encodeRichElem : Node.RichElem -> JE.Value
encodeRichElem elem =
    case elem of
        Node.TextChar ch ->
            JE.object [ ( "c", JE.string ch ) ]

        Node.Token token ->
            JE.object [ ( "tk", encodeBlockToken token ) ]


{-| A block token's wire tag: `"m"` for a boundary marker, `"n"` for one nest/indent unit.
Shared by the node codec (an element's content) and `Crdt.OpJson` (the `tok` action that
inserts one).
-}
encodeBlockToken : Node.BlockToken -> JE.Value
encodeBlockToken =
    blockTokenTag >> JE.string


blockTokenTag : Node.BlockToken -> String
blockTokenTag token =
    case token of
        Node.Marker ->
            "m"

        Node.Nest ->
            "n"


blockTokenDecoder : Decoder Node.BlockToken
blockTokenDecoder =
    JD.string
        |> JD.andThen
            (\tag ->
                case tag of
                    "m" ->
                        JD.succeed Node.Marker

                    "n" ->
                        JD.succeed Node.Nest

                    other ->
                        JD.fail ("unknown block token: " ++ other)
            )


richElemDecoder : Decoder Node.RichElem
richElemDecoder =
    JD.oneOf
        [ JD.field "c" charDecoder |> JD.map Node.TextChar
        , JD.field "tk" blockTokenDecoder |> JD.map Node.Token
        ]


{-| A text element's content: exactly **one** `Char`.

The invariant is enforced here, at the edge, rather than on every read. One element per
character is what every writer produces and what the index-based paths assume —
`Doc.Internal.applyCharDiff` keeps its id array aligned one-to-one with the text, so a
two-character element would silently shift every cursor and diff position after it. Measured
in `Char`s (code points), not `String.length` (UTF-16 units), so an astral character such as
an emoji is one, matching how the writers split.

-}
charDecoder : Decoder String
charDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case String.toList s of
                    [ _ ] ->
                        JD.succeed s

                    chars ->
                        JD.fail
                            ("a text element must hold exactly one character, not "
                                ++ String.fromInt (List.length chars)
                            )
            )


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
                        JD.field "el" (JD.list (elementDecoder (JD.lazy (\_ -> nodeDecoder))))
                            |> JD.map (Rga.fromElements >> Node.seq)

                    "txt" ->
                        JD.field "el" (JD.list (elementDecoder charDecoder))
                            |> JD.map (Rga.fromElements >> Node.txt)

                    "cnt" ->
                        JD.field "c" (JD.dict incrementDecoder)
                            |> JD.map Node.counter

                    "mov" ->
                        JD.map3
                            (\cs vs del ->
                                Node.mov (MoveList.fromParts cs vs (Set.fromList del))
                            )
                            (JD.field "cells" cellsDecoder)
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
                            (JD.field "el" (JD.list (elementDecoder richElemDecoder)))
                            (JD.field "marks" (JD.dict markOpDecoder))

                    other ->
                        JD.fail ("unknown node tag: " ++ other)
            )


{-| The whole cell RGA of a movable list, rebuilt through `Rga.putRight` — the same
constructor `Crdt.MoveList` writes cells with, which is what lets the wire omit `side`
and `deleted`. Decoding the RGA as a unit (rather than a `List Element` handed to
`Rga.fromElements`) is what keeps that constructor the only way a cell is ever made.
-}
cellsDecoder : Decoder (Rga.Rga OpId)
cellsDecoder =
    JD.map3 (\id parent valueId -> Rga.putRight id parent valueId)
        (JD.field "id" opIdDecoder)
        (JD.field "o" (JD.nullable opIdDecoder))
        (JD.field "v" opIdDecoder)
        |> JD.list
        |> JD.map (List.foldl (<|) Rga.empty)


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


elementDecoder : Decoder c -> Decoder (Rga.Element c)
elementDecoder contentDecoder =
    JD.map5 Rga.element
        (JD.field "id" opIdDecoder)
        (JD.field "p" (JD.nullable opIdDecoder))
        (JD.field "s" sideDecoder)
        (JD.field "c" contentDecoder)
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
