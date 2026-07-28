module ExtensibilityTests exposing (suite)

{-| User-defined CRDT types via `Crdt.opSet` + `Edit.contribute`/`retract`
(see docs/14). An op-set is a grow-only/removable set of op-id-keyed contributions folded
into a value at read; convergence is free (merge unions the contributions), the semantics
is the user's `fold`. These tests build three user types — a max-register, a multi-value
register, and an add-wins set — entirely in test code (no library change), and assert the
property that makes it a real CRDT: **concurrent contributions from two replicas converge
to the same value in both merge orders**, plus that the fold and removal read correctly.
-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Set
import Test exposing (Test, describe, test)



-- A DOCUMENT WITH THREE USER-DEFINED CRDT FIELDS -----------------------------


type alias Sample =
    { high : Int -- max-register: the highest score anyone contributed
    , seen : List Int -- multi-value register: every concurrently-set value
    , tags : List String -- add-wins set of strings
    }


type alias DocRefs =
    { high : Ref Sample (C.OpSetK Int) Int
    , seen : Ref Sample (C.OpSetK Int) (List Int)
    , tags : Ref Sample (C.OpSetK String) (List String)
    , schema : C.Schema C.Nested Sample
    }


maxRegister : C.Leaf (C.OpSetK Int) Int
maxRegister =
    C.opSet { contribution = C.int, fold = List.maximum >> Maybe.withDefault 0 }


mvRegister : C.Leaf (C.OpSetK Int) (List Int)
mvRegister =
    -- keep every distinct concurrently-contributed value, sorted for a stable read
    C.opSet { contribution = C.int, fold = \xs -> Set.toList (Set.fromList xs) }


stringSet : C.Leaf (C.OpSetK String) (List String)
stringSet =
    C.opSet { contribution = C.string, fold = \xs -> Set.toList (Set.fromList xs) }


docDoc : DocRefs
docDoc =
    C.record Sample DocRefs
        |> C.field "high" .high maxRegister
        |> C.field "seen" .seen mvRegister
        |> C.field "tags" .tags stringSet
        |> C.build


init : String -> Doc Sample
init name =
    C.init (Id.replica name) docDoc.schema


okc : Doc Sample -> Result Edit.EditError ( String, Doc Sample ) -> Doc Sample
okc fb r =
    case r of
        Ok ( _, d ) ->
            d

        Err _ ->
            fb


ok : Doc Sample -> Result Edit.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


read : Doc Sample -> Result Doc.ReadError Sample
read =
    Doc.read


peerOf : String -> Doc Sample -> Doc Sample
peerOf name from =
    Doc.decodeInto (Doc.encode from) (init name) |> Result.withDefault (init name)


mergeIn : Doc Sample -> Doc Sample -> Doc Sample
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to


suite : Test
suite =
    describe "extensibility: user-defined op-set CRDTs (docs/14)"
        [ test "fresh op-set reads its fold of the empty set" <|
            \_ ->
                read (init "a")
                    |> Expect.equal (Ok { high = 0, seen = [], tags = [] })
        , test "max-register folds contributions to the maximum" <|
            \_ ->
                let
                    d =
                        init "a"
                            |> (\x -> Edit.contribute docDoc.high C.int 5 x |> okc x)
                            |> (\x -> Edit.contribute docDoc.high C.int 12 x |> okc x)
                            |> (\x -> Edit.contribute docDoc.high C.int 3 x |> okc x)
                in
                read d |> Result.map .high |> Expect.equal (Ok 12)
        , test "CONCURRENT contributions converge (max), both merge orders" <|
            \_ ->
                -- the CRDT law: two replicas each contribute a different score with no
                -- coordination; after exchange, both read the max, order-independent.
                let
                    base =
                        init "seed"

                    a =
                        peerOf "alice" base |> (\x -> Edit.contribute docDoc.high C.int 7 x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Edit.contribute docDoc.high C.int 20 x |> okc x)

                    ab =
                        mergeIn b a

                    ba =
                        mergeIn a b
                in
                Expect.all
                    [ \_ -> Expect.equal (read ab) (read ba)
                    , \_ -> Expect.equal (Ok 20) (read ab |> Result.map .high)
                    ]
                    ()
        , test "multi-value register keeps BOTH concurrent values (no clobber)" <|
            \_ ->
                -- unlike an LWW register, concurrent sets both survive — the point of an
                -- MV-register. alice contributes 1, bob contributes 2, both remain.
                let
                    base =
                        init "seed"

                    a =
                        peerOf "alice" base |> (\x -> Edit.contribute docDoc.seen C.int 1 x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Edit.contribute docDoc.seen C.int 2 x |> okc x)

                    ab =
                        mergeIn b a

                    ba =
                        mergeIn a b
                in
                Expect.all
                    [ \_ -> Expect.equal (read ab) (read ba)
                    , \_ -> Expect.equal (Ok [ 1, 2 ]) (read ab |> Result.map .seen)
                    ]
                    ()
        , test "add-wins set: concurrent adds union" <|
            \_ ->
                let
                    base =
                        init "seed"

                    a =
                        peerOf "alice" base |> (\x -> Edit.contribute docDoc.tags C.string "red" x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Edit.contribute docDoc.tags C.string "blue" x |> okc x)

                    merged =
                        mergeIn b a
                in
                read merged |> Result.map .tags |> Expect.equal (Ok [ "blue", "red" ])
        , test "retract removes a specific contribution by its key" <|
            \_ ->
                let
                    ( _, d1 ) =
                        Edit.contribute docDoc.tags C.string "keep" (init "a")
                            |> Result.withDefault ( "", init "a" )

                    ( _, d2 ) =
                        Edit.contribute docDoc.tags C.string "drop" d1
                            |> Result.withDefault ( "", d1 )

                    ( dropKey, d3 ) =
                        Edit.contribute docDoc.tags C.string "dropme" d2
                            |> Result.withDefault ( "", d2 )

                    d4 =
                        Edit.retract docDoc.tags dropKey d3 |> ok d3
                in
                Expect.all
                    [ \_ -> Expect.equal (Ok [ "drop", "keep" ]) (read d2 |> Result.map .tags)

                    -- "dropme" was retracted by its key; the other two remain (retract
                    -- hits exactly the intended contribution, keyed by its op-id)
                    , \_ -> Expect.equal (Ok [ "drop", "keep" ]) (read d4 |> Result.map .tags)
                    ]
                    ()
        , test "retract converges with a concurrent add of the same value" <|
            \_ ->
                -- alice retracts her contribution; bob concurrently adds the same value
                -- under a DIFFERENT key → add-wins (bob's survives). Both orders agree.
                let
                    ( aliceKey, base1 ) =
                        Edit.contribute docDoc.tags C.string "x" (init "seed")
                            |> Result.withDefault ( "", init "seed" )

                    a =
                        peerOf "alice" base1 |> (\x -> Edit.retract docDoc.tags aliceKey x |> ok x)

                    b =
                        peerOf "bob" base1 |> (\x -> Edit.contribute docDoc.tags C.string "x" x |> okc x)

                    ab =
                        mergeIn b a

                    ba =
                        mergeIn a b
                in
                Expect.all
                    [ \_ -> Expect.equal (read ab) (read ba)
                    , \_ -> Expect.equal (Ok [ "x" ]) (read ab |> Result.map .tags)
                    ]
                    ()
        ]
