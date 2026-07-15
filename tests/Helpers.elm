module Helpers exposing (fuzzNode)

{-| Shared fixtures + fuzzers for the test suite. Mirrors the schema used by the
demo so tests and demo stay in lockstep.
-}

import Crdt.Id.Internal as Id exposing (OpId, ReplicaId)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Dict
import Fuzz exposing (Fuzzer)



-- DOMAIN SCHEMA (same shape as the demo) -------------------------------------


replicas : List ReplicaId
replicas =
    List.map Id.replica [ "alice", "bob", "carol" ]



-- FUZZERS --------------------------------------------------------------------


{-| A depth-bounded fuzzer over the internal `Node` type, used for the CRDT
algebraic laws. Bounded so recursive generation terminates.
-}
fuzzNode : Fuzzer Node
fuzzNode =
    fuzzNodeDepth 2


fuzzNodeDepth : Int -> Fuzzer Node
fuzzNodeDepth depth =
    if depth <= 0 then
        fuzzReg

    else
        Fuzz.oneOf
            [ fuzzReg
            , fuzzMap (depth - 1)
            , fuzzSeq (depth - 1)
            , fuzzCnt
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


fuzzElement : Int -> Fuzzer (Rga.Element Node)
fuzzElement depth =
    Fuzz.map5 Rga.element
        fuzzOpId
        (Fuzz.maybe fuzzOpId)
        fuzzSide
        (fuzzNodeDepth depth)
        Fuzz.bool


fuzzSide : Fuzzer Rga.Side
fuzzSide =
    Fuzz.oneOfValues [ Rga.Left, Rga.Right ]
