module ExtensibilityTests exposing (suite)

{-| User-defined CRDT types via `Crdt.Schema.opSet` + `Crdt.Ref.contribute`/`retract`
(see docs/14). An op-set is a grow-only/removable set of op-id-keyed contributions folded
into a value at read; convergence is free (merge unions the contributions), the semantics
is the user's `fold`. These tests build three user types — a max-register, a multi-value
register, and an add-wins set — entirely in test code (no library change), and assert the
property that makes it a real CRDT: **concurrent contributions from two replicas converge
to the same value in both merge orders**, plus that the fold and removal read correctly.
-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Schema as S exposing (Crdt)
import Expect
import Set
import Test exposing (Test, describe, test)



-- A DOCUMENT WITH THREE USER-DEFINED CRDT FIELDS -----------------------------


type alias Doc =
    { high : Int -- max-register: the highest score anyone contributed
    , seen : List Int -- multi-value register: every concurrently-set value
    , tags : List String -- add-wins set of strings
    }


type alias DocRefs =
    { high : Ref Doc (S.OpSetK Int) Int
    , seen : Ref Doc (S.OpSetK Int) (List Int)
    , tags : Ref Doc (S.OpSetK String) (List String)
    }


maxRegister : Crdt (S.OpSetK Int) Int
maxRegister =
    S.opSet { contribution = S.int, fold = List.maximum >> Maybe.withDefault 0 }


mvRegister : Crdt (S.OpSetK Int) (List Int)
mvRegister =
    -- keep every distinct concurrently-contributed value, sorted for a stable read
    S.opSet { contribution = S.int, fold = \xs -> Set.toList (Set.fromList xs) }


stringSet : Crdt (S.OpSetK String) (List String)
stringSet =
    S.opSet { contribution = S.string, fold = \xs -> Set.toList (Set.fromList xs) }


docDoc : Ref.RecordRefs Doc DocRefs
docDoc =
    Ref.record Doc DocRefs
        |> Ref.field "high" .high maxRegister
        |> Ref.field "seen" .seen mvRegister
        |> Ref.field "tags" .tags stringSet
        |> Ref.build


init : String -> OpDoc Doc
init name =
    OpDoc.init (Id.replica name) docDoc.schema


okc : OpDoc Doc -> Result OpDoc.Error ( String, OpDoc Doc ) -> OpDoc Doc
okc fb r =
    case r of
        Ok ( _, d ) ->
            d

        Err _ ->
            fb


ok : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
ok fb =
    Result.withDefault fb


read : OpDoc Doc -> Result S.Error Doc
read =
    OpDoc.read


peerOf : String -> OpDoc Doc -> OpDoc Doc
peerOf name from =
    OpDoc.decodeInto (OpDoc.encode from) (init name) |> Result.withDefault (init name)


mergeIn : OpDoc Doc -> OpDoc Doc -> OpDoc Doc
mergeIn from to =
    OpDoc.decodeInto (OpDoc.encode from) to |> Result.withDefault to


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
                            |> (\x -> Ref.contribute S.int 5 docDoc.refs.high x |> okc x)
                            |> (\x -> Ref.contribute S.int 12 docDoc.refs.high x |> okc x)
                            |> (\x -> Ref.contribute S.int 3 docDoc.refs.high x |> okc x)
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
                        peerOf "alice" base |> (\x -> Ref.contribute S.int 7 docDoc.refs.high x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Ref.contribute S.int 20 docDoc.refs.high x |> okc x)

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
                        peerOf "alice" base |> (\x -> Ref.contribute S.int 1 docDoc.refs.seen x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Ref.contribute S.int 2 docDoc.refs.seen x |> okc x)

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
                        peerOf "alice" base |> (\x -> Ref.contribute S.string "red" docDoc.refs.tags x |> okc x)

                    b =
                        peerOf "bob" base |> (\x -> Ref.contribute S.string "blue" docDoc.refs.tags x |> okc x)

                    merged =
                        mergeIn b a
                in
                read merged |> Result.map .tags |> Expect.equal (Ok [ "blue", "red" ])
        , test "retract removes a specific contribution by its key" <|
            \_ ->
                let
                    ( _, d1 ) =
                        Ref.contribute S.string "keep" docDoc.refs.tags (init "a")
                            |> Result.withDefault ( "", init "a" )

                    ( _, d2 ) =
                        Ref.contribute S.string "drop" docDoc.refs.tags d1
                            |> Result.withDefault ( "", d1 )

                    ( dropKey, d3 ) =
                        Ref.contribute S.string "dropme" docDoc.refs.tags d2
                            |> Result.withDefault ( "", d2 )

                    d4 =
                        Ref.retract dropKey docDoc.refs.tags d3 |> ok d3
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
                        Ref.contribute S.string "x" docDoc.refs.tags (init "seed")
                            |> Result.withDefault ( "", init "seed" )

                    a =
                        peerOf "alice" base1 |> (\x -> Ref.retract aliceKey docDoc.refs.tags x |> ok x)

                    b =
                        peerOf "bob" base1 |> (\x -> Ref.contribute S.string "x" docDoc.refs.tags x |> okc x)

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
