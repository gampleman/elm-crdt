module OpLogTests exposing (suite)

{-| Phase 1 op-log tests (see `docs/02-oplog.md`): real DAG + frontiers, causal
materialize, op-union merge, and checkout.

The central properties:

  - **merge is a join-semilattice** over op stores (commutative, associative,
    idempotent) — trivially, since it's set-union by id;
  - **materialize is independent of the causal linearization** chosen — folding
    any valid permutation of the ops yields the same `Node`;
  - **checkout** to a frontier reproduces the state as of that point;
  - **SetPresence** expresses dict set/remove on the log.

-}

import Crdt.Id as Id exposing (OpId)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.OpLog as OpLog exposing (Action(..), Op, OpStore, TargetStep(..))
import Crdt.Rga as Rga
import Crdt.Schema.Internal as S exposing (Crdt)
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)



-- FIXTURE SCHEMA -------------------------------------------------------------


type alias Doc =
    { title : String
    , items : List Item
    , notes : Dict.Dict String String
    }


type alias Item =
    { label : String
    , done : Bool
    }


schema : Crdt S.Nested Doc
schema =
    S.record Doc
        |> S.field "title" .title S.text
        |> S.field "items" .items (S.list itemSchema)
        |> S.field "notes" .notes (S.dict S.text)
        |> S.build


itemSchema : Crdt S.Nested Item
itemSchema =
    S.record Item
        |> S.field "label" .label S.text
        |> S.field "done" .done S.bool
        |> S.build


emptyTree : Node
emptyTree =
    S.emptyNode schema (Id.ctx (Id.replica "base")) |> Tuple.first


emptyItem : Node
emptyItem =
    S.emptyNode itemSchema (Id.ctx (Id.replica "base")) |> Tuple.first


read : OpStore -> Result S.Error Doc
read store =
    S.decodeNode schema (OpLog.materialize emptyTree store)


storeOf : List Op -> OpStore
storeOf =
    List.foldl OpLog.insert OpLog.empty



-- ID / OP HELPERS ------------------------------------------------------------


alice : Int -> OpId
alice n =
    Id.opId n (Id.replica "alice")


bob : Int -> OpId
bob n =
    Id.opId n (Id.replica "bob")


items : OpLog.Target
items =
    [ IntoKey "items" ]


notes : OpLog.Target
notes =
    [ IntoKey "notes" ]


{-| A small but causally-structured op set: add an item, label it, add a second
item, delete the first. The delete causally depends on the first insert, which is
why a _causal_ order (not just any order) is needed to materialize correctly.
-}
sampleOps : List Op
sampleOps =
    [ { id = alice 1, deps = [], action = InsertElem { container = items, elemId = alice 1, parent = Nothing, side = Rga.Right, seed = emptyItem } }
    , { id = alice 2, deps = [ alice 1 ], action = InsertElem { container = [ IntoKey "items", IntoElem (alice 1), IntoKey "label" ], elemId = alice 2, parent = Nothing, side = Rga.Right, seed = charNode 'A' (alice 2) } }
    , { id = alice 3, deps = [ alice 1 ], action = InsertElem { container = items, elemId = alice 3, parent = Just (alice 1), side = Rga.Right, seed = emptyItem } }
    , { id = alice 4, deps = [ alice 1 ], action = DeleteElem { container = items, elem = alice 1 } }
    ]


charNode : Char -> OpId -> Node
charNode c stamp =
    Node.reg (PString (String.fromChar c)) stamp



-- FUZZER: random valid permutations of an op list ----------------------------


{-| Shuffle a list deterministically from a fuzzed seed list of indices. Produces
arbitrary permutations so we can assert materialize is order-independent.
-}
shuffle : List Int -> List a -> List a
shuffle keys xs =
    List.map2 Tuple.pair (keys ++ List.range 0 (List.length xs)) xs
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


permutationFuzzer : Int -> Fuzzer (List Int)
permutationFuzzer n =
    Fuzz.listOfLength n (Fuzz.intRange 0 1000)



-- TESTS ----------------------------------------------------------------------


suite : Test
suite =
    describe "op-log (Phase 1)"
        [ describe "merge laws (op-union)"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            storeOf (List.take 2 sampleOps)

                        b =
                            storeOf (List.drop 2 sampleOps)
                    in
                    Expect.equal
                        (read (OpLog.merge a b))
                        (read (OpLog.merge b a))
            , test "idempotent: merging a store with itself is identity on the read" <|
                \_ ->
                    let
                        s =
                            storeOf sampleOps
                    in
                    Expect.equal (read s) (read (OpLog.merge s s))
            , test "associative" <|
                \_ ->
                    let
                        a =
                            storeOf (List.take 1 sampleOps)

                        b =
                            storeOf (List.take 2 sampleOps |> List.drop 1)

                        c =
                            storeOf (List.drop 2 sampleOps)
                    in
                    Expect.equal
                        (read (OpLog.merge a (OpLog.merge b c)))
                        (read (OpLog.merge (OpLog.merge a b) c))
            ]
        , describe "causal materialize"
            [ fuzz (permutationFuzzer (List.length sampleOps)) "is independent of insertion order into the store" <|
                \keys ->
                    let
                        shuffled =
                            storeOf (shuffle keys sampleOps)
                    in
                    Expect.equal (read (storeOf sampleOps)) (read shuffled)
            , test "delete-after-insert resolves correctly (item 1 deleted, item 3 remains)" <|
                \_ ->
                    read (storeOf sampleOps)
                        |> Result.map (.items >> List.length)
                        |> Expect.equal (Ok 1)
            , test "the surviving item keeps its data" <|
                \_ ->
                    -- only item 3 (empty label) should remain; item 1 (label "A") is gone
                    read (storeOf sampleOps)
                        |> Result.map .items
                        |> Expect.equal (Ok [ { label = "", done = False } ])
            ]
        , describe "checkout (time travel)"
            [ test "checkout to the frontier before the delete still shows item 1" <|
                \_ ->
                    let
                        store =
                            storeOf sampleOps

                        -- frontier just after inserting+labelling item 1 (before alice 3 / alice 4)
                        beforeDelete =
                            [ alice 2 ]

                        atVersion =
                            OpLog.checkout beforeDelete emptyTree store
                    in
                    S.decodeNode schema atVersion
                        |> Result.map (.items >> List.length)
                        |> Expect.equal (Ok 1)
            , test "checkout to that frontier shows item 1 with its label 'A'" <|
                \_ ->
                    let
                        store =
                            storeOf sampleOps

                        atVersion =
                            OpLog.checkout [ alice 2 ] emptyTree store
                    in
                    S.decodeNode schema atVersion
                        |> Result.map (.items >> List.map .label)
                        |> Expect.equal (Ok [ "A" ])
            ]
        , describe "SetPresence (dict on the log)"
            [ test "setting a key present (seeded as text) then typing into it reads back" <|
                \_ ->
                    let
                        emptyNote =
                            S.emptyNode S.text (Id.ctx (Id.replica "base")) |> Tuple.first

                        ops =
                            [ { id = alice 1, deps = [], action = SetPresence { target = [ IntoKey "notes", IntoKey "k" ], present = True, seed = emptyNote } }
                            , { id = alice 2, deps = [ alice 1 ], action = InsertElem { container = [ IntoKey "notes", IntoKey "k" ], elemId = alice 2, parent = Nothing, side = Rga.Right, seed = charNode 'v' (alice 2) } }
                            ]
                    in
                    read (storeOf ops)
                        |> Result.map (.notes >> Dict.get "k")
                        |> Expect.equal (Ok (Just "v"))
            , test "a later remove (present=False, higher stamp) wins by LWW" <|
                \_ ->
                    let
                        emptyNote =
                            S.emptyNode S.text (Id.ctx (Id.replica "base")) |> Tuple.first

                        ops =
                            [ { id = alice 1, deps = [], action = SetPresence { target = [ IntoKey "notes", IntoKey "k" ], present = True, seed = emptyNote } }
                            , { id = alice 5, deps = [ alice 1 ], action = SetPresence { target = [ IntoKey "notes", IntoKey "k" ], present = False, seed = emptyNote } }
                            ]
                    in
                    read (storeOf ops)
                        |> Result.map (.notes >> Dict.member "k")
                        |> Expect.equal (Ok False)
            ]
        , describe "SetReg (LWW primitive)"
            [ test "later write wins; reading reflects the winning value" <|
                \_ ->
                    let
                        donePath =
                            [ IntoKey "items", IntoElem (alice 1), IntoKey "done" ]

                        ops =
                            [ { id = alice 1, deps = [], action = InsertElem { container = items, elemId = alice 1, parent = Nothing, side = Rga.Right, seed = emptyItem } }
                            , { id = alice 2, deps = [ alice 1 ], action = SetReg donePath (PBool True) }
                            , { id = alice 5, deps = [ alice 2 ], action = SetReg donePath (PBool False) }
                            ]
                    in
                    read (storeOf ops)
                        |> Result.map (.items >> List.map .done)
                        |> Expect.equal (Ok [ False ])
            , test "LWW is by stamp, not fold order: lower-stamp write loses even if applied last" <|
                \_ ->
                    let
                        donePath =
                            [ IntoKey "items", IntoElem (alice 1), IntoKey "done" ]

                        -- bob 9 (higher counter) sets True; alice 3 sets False.
                        -- Whichever fold order, the higher stamp (bob 9 = True) wins.
                        ops =
                            [ { id = alice 1, deps = [], action = InsertElem { container = items, elemId = alice 1, parent = Nothing, side = Rga.Right, seed = emptyItem } }
                            , { id = alice 3, deps = [ alice 1 ], action = SetReg donePath (PBool False) }
                            , { id = bob 9, deps = [ alice 1 ], action = SetReg donePath (PBool True) }
                            ]
                    in
                    read (storeOf ops)
                        |> Result.map (.items >> List.map .done)
                        |> Expect.equal (Ok [ True ])
            ]
        , describe "convergence across replicas"
            [ test "alice and bob edit concurrently; merged store reads the same either way" <|
                \_ ->
                    let
                        -- alice appends an item; bob concurrently sets a note. No
                        -- shared deps — genuinely concurrent edits from two peers.
                        emptyNote =
                            S.emptyNode S.text (Id.ctx (Id.replica "base")) |> Tuple.first

                        aliceStore =
                            storeOf
                                [ { id = alice 1, deps = [], action = InsertElem { container = items, elemId = alice 1, parent = Nothing, side = Rga.Right, seed = emptyItem } } ]

                        bobStore =
                            storeOf
                                [ { id = bob 1, deps = [], action = SetPresence { target = notes ++ [ IntoKey "n" ], present = True, seed = emptyNote } } ]
                    in
                    Expect.equal
                        (read (OpLog.merge aliceStore bobStore))
                        (read (OpLog.merge bobStore aliceStore))
            ]
        , describe "store introspection API"
            [ test "member reports presence by id; ops lists everything" <|
                \_ ->
                    let
                        store =
                            storeOf sampleOps
                    in
                    Expect.all
                        [ \_ -> Expect.equal True (OpLog.member (alice 1) store)
                        , \_ -> Expect.equal False (OpLog.member (bob 99) store)
                        , \_ -> Expect.equal (List.length sampleOps) (List.length (OpLog.ops store))
                        ]
                        ()
            , test "frontier is the set of ops nothing depends on" <|
                \_ ->
                    -- in sampleOps, alice 2/3/4 all depend on alice 1; nothing
                    -- depends on 2, 3, or 4 — so they are the tips.
                    storeOf sampleOps
                        |> OpLog.frontier
                        |> List.sortWith Id.compareOpId
                        |> Expect.equal [ alice 2, alice 3, alice 4 ]
            , test "applyOps over an explicit causal order matches materialize" <|
                \_ ->
                    let
                        store =
                            storeOf sampleOps
                    in
                    Expect.equal
                        (OpLog.materialize emptyTree store)
                        (OpLog.applyOps emptyTree (OpLog.causalOrder store))
            ]
        ]
