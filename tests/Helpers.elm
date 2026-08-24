module Helpers exposing (fuzzElements, fuzzNode, fuzzOp, fuzzTreeValue)

{-| Shared fuzzers over the internal replicated-state types, for the property tests
that need **arbitrary — including malformed — input** rather than hand-built fixtures.

Nothing constrains what these generate: element ids and `parent` anchors are drawn
from the same small pool, so cycles, self-parents and dangling anchors all occur, and
map entries can be absent with any stamp. That is deliberate. A CRDT decodes state
from the network, so every walk over a `Node` must terminate and stay total on input
no attacker had to make well-formed. See `tests/NodeFuzzTests.elm`,
`tests/TreePropertyTests.elm` and `tests/OpJsonTests.elm`.

-}

import Crdt.Frac as Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId, ReplicaId)
import Crdt.MoveList as MoveList
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.OpLog exposing (Action(..), Op, TargetStep(..))
import Crdt.Rga as Rga
import Crdt.Tree.Internal as Tree
import Dict
import Fuzz exposing (Fuzzer)
import Set



-- DOMAIN SCHEMA (same shape as the demo) -------------------------------------


replicas : List ReplicaId
replicas =
    List.map Id.replica [ "alice", "bob", "carol" ]



-- FUZZERS --------------------------------------------------------------------


{-| A depth-bounded fuzzer over the internal `Node` type. Bounded so recursive
generation terminates (and so runs stay fast — an unbounded version stalls).
-}
fuzzNode : Fuzzer Node
fuzzNode =
    fuzzNodeDepth 2


{-| Covers **every** `Node` constructor. Any that is missing here is a constructor
whose codec branch and read path go unfuzzed, which is how a decoder bug survives.
-}
fuzzNodeDepth : Int -> Fuzzer Node
fuzzNodeDepth depth =
    if depth <= 0 then
        fuzzReg

    else
        Fuzz.oneOf
            [ fuzzReg
            , fuzzMap (depth - 1)
            , fuzzSeq (depth - 1)
            , fuzzTxt
            , fuzzCnt
            , fuzzMov (depth - 1)
            , fuzzTree (depth - 1)
            , fuzzRich
            ]


fuzzCnt : Fuzzer Node
fuzzCnt =
    Fuzz.listOfLengthBetween 0 4 (Fuzz.pair fuzzOpId (Fuzz.intRange -50 50))
        |> Fuzz.map
            (List.map (\( stamp, delta ) -> ( Id.opIdToString stamp, Node.increment stamp delta ))
                >> Dict.fromList
                >> Node.counter
            )


fuzzReg : Fuzzer Node
fuzzReg =
    Fuzz.map2
        (\prim stamp -> Node.reg prim stamp)
        fuzzPrim
        fuzzOpId


fuzzPrim : Fuzzer Prim
fuzzPrim =
    Fuzz.oneOf
        [ Fuzz.map PInt (Fuzz.intRange -100 100)
        , Fuzz.map PString (Fuzz.oneOfValues [ "a", "b", "c", "" ])
        , Fuzz.map PBool Fuzz.bool
        , Fuzz.constant PNull
        ]


fuzzOpId : Fuzzer OpId
fuzzOpId =
    Fuzz.map2 Id.opId
        (Fuzz.intRange 0 20)
        (Fuzz.oneOfValues replicas)


fuzzMap : Int -> Fuzzer Node
fuzzMap depth =
    Fuzz.listOfLengthBetween 0 3 (Fuzz.pair fuzzKey (fuzzEntry depth))
        |> Fuzz.map (Dict.fromList >> Node.mapFromEntries)


fuzzEntry : Int -> Fuzzer Node.Entry
fuzzEntry depth =
    Fuzz.map3 (\stamp present value -> Node.entry stamp present value)
        fuzzOpId
        Fuzz.bool
        (fuzzNodeDepth depth)


fuzzKey : Fuzzer String
fuzzKey =
    Fuzz.oneOfValues [ "x", "y", "z" ]


fuzzSeq : Int -> Fuzzer Node
fuzzSeq depth =
    Fuzz.listOfLengthBetween 0 4 (fuzzElement depth)
        |> Fuzz.map (Rga.fromElements >> Node.seq)


{-| Plain text: an RGA of **single characters**. Nothing else is representable — that is the
point of the content being typed (`design-docs/16-typed-sequence-content.md`), and this
fuzzer used to generate arbitrary `Node` content here, i.e. states the reader had to cope
with and no writer could produce. The element _ids_ and anchors are still adversarial.
-}
fuzzTxt : Fuzzer Node
fuzzTxt =
    Fuzz.listOfLengthBetween 0 4 (fuzzElementOf fuzzChar)
        |> Fuzz.map (Rga.fromElements >> Node.txt)


{-| One character, including a multi-code-unit one — an astral emoji is a single `Char`, so
it must survive every walk that counts or slices characters.
-}
fuzzChar : Fuzzer String
fuzzChar =
    Fuzz.oneOfValues [ "a", "b", " ", "\n", "ü", "→", "🙈" ]


{-| A movable list: a cell RGA whose contents are valueIds, a value table, and a
tombstone set. The three are generated **independently**, so cells routinely name
values that don't exist and values are routinely orphaned — the read has to cope.
-}
fuzzMov : Int -> Fuzzer Node
fuzzMov depth =
    Fuzz.map3
        (\cells vals dead ->
            Node.mov (MoveList.fromParts (Rga.fromElements cells) (Dict.fromList vals) (Set.fromList dead))
        )
        (Fuzz.listOfLengthBetween 0 4 fuzzCell)
        (Fuzz.listOfLengthBetween 0 4 (Fuzz.pair fuzzIdKey (fuzzNodeDepth depth)))
        (Fuzz.listOfLengthBetween 0 2 fuzzIdKey)


{-| A cell is structural: it always anchors as a **right**-child and is never
tombstoned (a movable list records deletions as valueIds in its own tombstone set).
`Crdt.Json` relies on that and omits both fields from the wire, reconstructing them
on decode — so generating other combinations would fuzz unreachable states and fail
the round-trip for no good reason. Everything else about a cell is free, including a
`content` valueId naming no value and a `parent` that cycles.
-}
fuzzCell : Fuzzer (Rga.Element Id.OpId)
fuzzCell =
    Fuzz.map5 Rga.element
        fuzzOpId
        (Fuzz.maybe fuzzOpId)
        (Fuzz.constant Rga.Right)
        fuzzOpId
        (Fuzz.constant False)


fuzzTree : Int -> Fuzzer Node
fuzzTree depth =
    Fuzz.map Node.tree (fuzzTreeDepth depth)


{-| A movable tree, generated component-wise: a move set, a payload table and tombstones
that need not agree with each other. Moves drawn from a small id pool mean parent cycles
are common — the resolver must skip them, not loop — and a move whose child has no payload,
or a payload with no move, is routine. Exposed on its own because the tree's read path has
invariants that outlive any one `Node` (`tests/TreePropertyTests.elm`).
-}
fuzzTreeValue : Fuzzer (Tree.Tree Node)
fuzzTreeValue =
    fuzzTreeDepth 1


fuzzTreeDepth : Int -> Fuzzer (Tree.Tree Node)
fuzzTreeDepth depth =
    Fuzz.map3
        (\moves vals dead ->
            Tree.fromParts (Dict.fromList moves) (Dict.fromList vals) (Set.fromList dead)
        )
        (Fuzz.listOfLengthBetween 0 4 (Fuzz.map (\m -> ( Id.opIdToString m.moveOp, m )) fuzzMove))
        (Fuzz.listOfLengthBetween 0 4 (Fuzz.pair fuzzIdKey (fuzzNodeDepth depth)))
        (Fuzz.listOfLengthBetween 0 2 fuzzIdKey)


fuzzMove : Fuzzer Tree.Move
fuzzMove =
    Fuzz.map4 (\moveOp child parent pos -> { moveOp = moveOp, child = child, parent = parent, pos = pos })
        fuzzOpId
        fuzzOpId
        (Fuzz.maybe fuzzOpId)
        fuzzFrac


fuzzFrac : Fuzzer Frac
fuzzFrac =
    Fuzz.listOfLengthBetween 1 3 (Fuzz.intRange 0 255)
        |> Fuzz.map Frac.fromList


{-| Rich text: a sequence of characters and block-structure tokens, plus a mark set whose
anchors are generated **independently** of the characters — so marks routinely point at ids
no element carries, and ranges routinely run backwards. Markers and nest tokens appear at any
position, including the malformed arrangements block reads have to tolerate (a nest token
before any marker, two markers in a row, tokens with no block).
-}
fuzzRich : Fuzzer Node
fuzzRich =
    Fuzz.map2
        (\text marks -> Node.rich { text = Rga.fromElements text, marks = Dict.fromList marks })
        (Fuzz.listOfLengthBetween 0 6 (fuzzElementOf fuzzRichElem))
        (Fuzz.listOfLengthBetween 0 3 (Fuzz.map (\m -> ( Id.opIdToString m.id, m )) fuzzMarkOp))


fuzzRichElem : Fuzzer Node.RichElem
fuzzRichElem =
    Fuzz.frequency
        [ ( 5, Fuzz.map Node.TextChar fuzzChar )
        , ( 1, Fuzz.constant (Node.Token Node.Marker) )
        , ( 1, Fuzz.constant (Node.Token Node.Nest) )
        ]


fuzzMarkOp : Fuzzer Node.MarkOp
fuzzMarkOp =
    Fuzz.map5 (\id type_ value start end -> { id = id, type_ = type_, value = value, start = start, end = end })
        fuzzOpId
        (Fuzz.oneOfValues [ "bold", "italic", "link" ])
        fuzzPrim
        fuzzAnchor
        fuzzAnchor


fuzzAnchor : Fuzzer Node.MarkAnchor
fuzzAnchor =
    Fuzz.map2 (\ref side -> { ref = ref, side = side })
        (Fuzz.maybe fuzzOpId)
        (Fuzz.oneOfValues [ Node.Before, Node.After ])


{-| An id in the string form the value/payload tables are keyed by.
-}
fuzzIdKey : Fuzzer String
fuzzIdKey =
    Fuzz.map Id.opIdToString fuzzOpId


{-| A batch of sequence elements, as the op-log fold would insert them. Ids come from
the same small pool as `parent` anchors, so a batch routinely contains cycles and
self-parents.
-}
fuzzElements : Fuzzer (List (Rga.Element Node))
fuzzElements =
    Fuzz.listOfLengthBetween 0 8 (fuzzElement 1)


fuzzElement : Int -> Fuzzer (Rga.Element Node)
fuzzElement depth =
    fuzzElementOf (fuzzNodeDepth depth)


{-| An RGA element over any content type. The ordering fields are what make an element
adversarial — ids from a small pool so anchors cycle, self-parent and dangle — and they are
the same whatever the sequence holds, which is exactly why `Crdt.Rga` is polymorphic.
-}
fuzzElementOf : Fuzzer c -> Fuzzer (Rga.Element c)
fuzzElementOf content =
    Fuzz.map5 Rga.element
        fuzzOpId
        (Fuzz.maybe fuzzOpId)
        fuzzSide
        content
        Fuzz.bool


fuzzSide : Fuzzer Rga.Side
fuzzSide =
    Fuzz.oneOfValues [ Rga.Left, Rga.Right ]



-- OPERATIONS -----------------------------------------------------------------


{-| An arbitrary operation: any action, any target, any seed. Nothing here has to make
sense against a document — the wire codec has to be lossless for whatever it is handed,
and a target that addresses nothing is exactly what a torn delta delivers.
-}
fuzzOp : Fuzzer Op
fuzzOp =
    Fuzz.map3 (\id deps action -> { id = id, deps = deps, action = action })
        fuzzOpId
        (Fuzz.listOfLengthBetween 0 3 fuzzOpId)
        fuzzAction


{-| Covers **every** `Action` constructor. Any that is missing here is a wire-format branch
that goes unfuzzed, which is how an encode/decode asymmetry ships.

Nothing here notices a _new_ constructor — a `Fuzz.oneOf` over a list compiles fine when a
union grows a sibling. `tests/OpJsonTests.elm`'s `kindOf` is the compile-time tripwire that
does; when it sends you here, add the branch.

-}
fuzzAction : Fuzzer Action
fuzzAction =
    Fuzz.oneOf
        [ Fuzz.map2 SetReg fuzzTarget fuzzPrim
        , Fuzz.map3 (\t p s -> SetKeyPresence { target = t, present = p, seed = s })
            fuzzTarget
            Fuzz.bool
            fuzzNode
        , Fuzz.map5
            (\t e p sd s -> InsertElem { container = t, elemId = e, parent = p, side = sd, seed = s })
            fuzzTarget
            fuzzOpId
            (Fuzz.maybe fuzzOpId)
            fuzzSide
            fuzzNode
        , Fuzz.map5
            (\t e x p sd -> InsertText { container = t, start = e, text = x, parent = p, side = sd })
            fuzzTarget
            fuzzOpId
            (Fuzz.oneOfValues [ "", "a", "hello", "→ ünïcøde" ])
            (Fuzz.maybe fuzzOpId)
            fuzzSide
        , Fuzz.map4
            (\t e p ( sd, token ) -> InsertToken { container = t, elemId = e, parent = p, side = sd, token = token })
            fuzzTarget
            fuzzOpId
            (Fuzz.maybe fuzzOpId)
            (Fuzz.pair fuzzSide (Fuzz.oneOfValues [ Node.Marker, Node.Nest ]))
        , Fuzz.map2 (\t e -> DeleteElem { container = t, elem = e }) fuzzTarget fuzzOpId
        , Fuzz.map3 (\t e o -> MoveElem { container = t, elem = e, after = o })
            fuzzTarget
            fuzzOpId
            (Fuzz.maybe fuzzOpId)
        , Fuzz.map2 (\t d -> Increment { target = t, delta = d }) fuzzTarget (Fuzz.intRange -50 50)
        , Fuzz.map5 (\t c p pos s -> TreeMove { container = t, child = c, parent = p, pos = pos, seed = s })
            fuzzTarget
            fuzzOpId
            (Fuzz.maybe fuzzOpId)
            fuzzFrac
            (Fuzz.maybe fuzzNode)
        , Fuzz.map4
            (\t m ( ty, v ) ( st, en ) ->
                AddMark { container = t, markId = m, type_ = ty, value = v, start = st, end = en }
            )
            fuzzTarget
            fuzzOpId
            (Fuzz.pair (Fuzz.oneOfValues [ "bold", "link" ]) fuzzPrim)
            (Fuzz.pair fuzzAnchor fuzzAnchor)
        ]


fuzzTarget : Fuzzer (List TargetStep)
fuzzTarget =
    Fuzz.listOfLengthBetween 0
        3
        (Fuzz.oneOf
            [ Fuzz.map IntoKey fuzzKey
            , Fuzz.map IntoElem fuzzOpId
            ]
        )
