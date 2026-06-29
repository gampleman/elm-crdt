module NodeMergeTests exposing (suite)

{-| The CRDT algebraic laws, fuzzed over the internal `Node` type.

A state-based CRDT's merge must form a join-semilattice: it must be
**commutative**, **associative**, and **idempotent**. If any of these fail, two
replicas can fail to converge. These three properties are the bedrock the entire
library rests on.

-}

import Crdt.Node as Node
import Expect
import Helpers exposing (fuzzNode)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


suite : Test
suite =
    describe "Node.merge — join-semilattice laws"
        [ fuzz2 fuzzNode fuzzNode "commutativity: merge a b == merge b a" <|
            \a b ->
                Node.merge a b
                    |> Expect.equal (Node.merge b a)
        , fuzz3 fuzzNode fuzzNode fuzzNode "associativity: merge a (merge b c) == merge (merge a b) c" <|
            \a b c ->
                Node.merge a (Node.merge b c)
                    |> Expect.equal (Node.merge (Node.merge a b) c)
        , fuzz fuzzNode "idempotence: merge a a == a" <|
            \a ->
                Node.merge a a
                    |> Expect.equal a
        , fuzz3 fuzzNode fuzzNode fuzzNode "idempotence under re-merge: merging the same input twice changes nothing" <|
            \a b c ->
                let
                    once =
                        Node.merge (Node.merge a b) c
                in
                Node.merge once b
                    |> Expect.equal once
        ]
