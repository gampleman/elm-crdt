module OpJsonTests exposing (suite)

{-| **The op wire format: lossless, stable, and hostile-input-safe.**

`Crdt.OpJson` had no test module of its own. It was exercised only _transitively_, through
`Doc.encode`/`decodeInto` in the sync tests — which always encode and decode with the **same
build**, so the one thing that matters most about a wire format is exactly the thing those
tests cannot see: whether it is the format the other peer speaks. Rename a field key, swap
two positional args, drop a component from one branch, and every round-trip test still
passes while every real peer breaks.

Three claims, one group each:

1.  **Lossless.** `decode ∘ encode == identity` for an arbitrary op, over every `Action`
    constructor. An op is the unit of convergence: anything dropped or altered on the wire is
    two replicas that no longer agree, and since structural equality is the convergence oracle
    the assertion is plain `==`.
2.  **Stable.** The literal bytes of one op per kind are pinned. This is the only test in the
    suite that fails when the format changes compatibly-with-itself, since encoder and decoder
    always move together. The format is **not frozen until `1.0.0` ships**, so before then a
    failure here usually means "re-pin it" — the point is that the change shows up in the diff
    as a decision rather than slipping through green. Once 1.0.0 is out this becomes a
    compatibility guard and the answer changes to "revert, or version the format on purpose".
3.  **Total on hostile input.** Ops arrive from the network. Every malformed payload must
    produce `Err`, and a payload that fails must leave the receiving document **untouched** —
    a partially-applied delta is silent corruption, and unlike a rejected one it cannot be
    retried.

-}

import Crdt.Doc.Internal as Doc
import Crdt.Frac as Frac
import Crdt.Id.Internal as Id
import Crdt.Node as Node
import Crdt.OpJson as OpJson
import Crdt.OpLog exposing (Action(..), Op, TargetStep(..))
import Crdt.Rga as Rga
import Expect
import Fuzz
import Helpers exposing (fuzzOp)
import Helpers.Edits as E
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "the op wire format"
        [ describe "lossless"
            [ fuzz fuzzOp "an arbitrary op round-trips" <|
                \op ->
                    roundTrip [ op ] |> Expect.equal (Ok [ op ])
            , fuzz (Fuzz.listOfLengthBetween 0 5 fuzzOp) "so does a batch of them, in order" <|
                \ops ->
                    -- Order is not semantically load-bearing (the receiver folds a causal
                    -- order of its own store), but a codec that reversed or deduplicated a
                    -- batch would be hiding something.
                    roundTrip ops |> Expect.equal (Ok ops)
            , test "every action kind is represented above, in order" <|
                \_ ->
                    -- Neither `fuzzAction` nor `sampleOps` can notice a NEW `Action`
                    -- constructor on its own: both are lists of constructor applications, and
                    -- adding a sibling to a union does not make an existing list fail to
                    -- compile. (An earlier version of this test claimed it did, and a new
                    -- action duly slipped in unfuzzed and unpinned.)
                    --
                    -- `kindOf` is the tripwire that actually works — an exhaustive `case`, so
                    -- a new constructor is a compile error there. Naming its wire tag then
                    -- lands you here, where the fixture list has to grow to match.
                    List.map (.action >> kindOf) sampleOps |> Expect.equalLists allKinds
            , test "each fixture round-trips too" <|
                \_ ->
                    roundTrip sampleOps |> Expect.equal (Ok sampleOps)
            ]
        , describe "stable"
            [ test "the bytes of one op per kind are exactly this" <|
                \_ ->
                    -- Changing this string is changing the protocol. Pre-1.0 that is allowed
                    -- and often right: if you are here because it failed and the change was
                    -- intended, re-pin it and note it in `design-docs/02-oplog.md`. What must
                    -- not happen is the format moving without anyone deciding to move it —
                    -- which is exactly what a same-build round-trip test cannot notice.
                    OpJson.encodeOps sampleOps
                        |> JE.encode 0
                        |> Expect.equal expectedWire
            ]
        , describe "total on hostile input"
            [ test "every malformed payload is rejected" <|
                \_ ->
                    -- Rejection, not tolerance: an op the receiver cannot read in full is one
                    -- it must not guess at. Silently defaulting a missing field would apply an
                    -- op nobody sent, to a document that then disagrees with its author's.
                    malformed
                        |> List.map (\( name, payload ) -> ( name, decodesToErr payload ))
                        |> List.filter (Tuple.second >> not)
                        |> List.map Tuple.first
                        |> Expect.equalLists []
            , test "an unknown action kind is rejected, not skipped" <|
                \_ ->
                    -- Forward compatibility is deliberately *not* "ignore what you don't
                    -- understand": dropping an op keeps the document readable while making it
                    -- silently wrong, and the wrongness is permanent (the op is gone from this
                    -- replica's store, so it is never retried and never re-sent). Failing the
                    -- whole delta keeps the peer's `version` un-advanced, so it re-offers it.
                    """[{"id":[1,"a"],"deps":[],"a":{"k":"teleport","t":[]}}]"""
                        |> JD.decodeString OpJson.opsDecoder
                        |> Result.mapError (always "rejected")
                        |> Expect.equal (Err "rejected")
            , test "a corrupt op in a delta leaves the document exactly as it was" <|
                \_ ->
                    -- All-or-nothing, which is what makes a failed sync retryable. `decodeInto`
                    -- decodes the whole batch before it applies any of it, so there is no
                    -- half-merged state to reconcile.
                    let
                        before =
                            E.init "alice" |> E.run [ E.SetTitle "hello", E.AddTag "t" ]

                        after =
                            Doc.decodeInto corruptDelta before
                    in
                    Expect.all
                        [ \_ -> after |> Result.map (always ()) |> Result.mapError (always ()) |> Expect.equal (Err ())
                        , \_ -> E.render (Result.withDefault before after) |> Expect.equal (E.render before)
                        , \_ -> Doc.opCount (Result.withDefault before after) |> Expect.equal (Doc.opCount before)
                        , \_ -> Doc.pendingCount (Result.withDefault before after) |> Expect.equal 0
                        ]
                        ()
            , test "a fractional position that isn't one decodes to the midpoint" <|
                \_ ->
                    -- The one place the format is deliberately *tolerant* rather than strict,
                    -- because `Frac` has no invalid value: `fromList []` is the midpoint key.
                    -- Pinned so the tolerance is a decision, not a surprise — a rejected tree
                    -- op would strand the node instead.
                    """[{"id":[1,"a"],"deps":[],"a":{"k":"tree","t":[],"c":[1,"a"],"p":null,"pos":[],"s":null}}]"""
                        |> JD.decodeString OpJson.opsDecoder
                        |> Result.map (List.map (.action >> treePos))
                        |> Expect.equal (Ok [ Just (Frac.toList (Frac.fromList [])) ])
            ]
        ]



-- HELPERS ---------------------------------------------------------------------


roundTrip : List Op -> Result JD.Error (List Op)
roundTrip ops =
    OpJson.encodeOps ops |> JD.decodeValue OpJson.opsDecoder


decodesToErr : String -> Bool
decodesToErr payload =
    case JD.decodeString OpJson.opsDecoder payload of
        Err _ ->
            True

        Ok _ ->
            False


treePos : Action -> Maybe (List Int)
treePos action =
    case action of
        TreeMove m ->
            Just (Frac.toList m.pos)

        _ ->
            Nothing


{-| Every `Action`'s wire tag, as an exhaustive `case`. Adding a constructor to `Action`
breaks this, which is the point: it is the only place in the suite the compiler can force a
new action to be acknowledged.
-}
kindOf : Action -> String
kindOf action =
    case action of
        SetReg _ _ ->
            "reg"

        SetKeyPresence _ ->
            "pres"

        InsertElem _ ->
            "ins"

        InsertText _ ->
            "itxt"

        InsertToken _ ->
            "tok"

        DeleteElem _ ->
            "del"

        MoveElem _ ->
            "mov"

        Increment _ ->
            "inc"

        TreeMove _ ->
            "tree"

        AddMark _ ->
            "mark"


{-| The tags `sampleOps` must cover, in the order the fixtures are written. Kept beside
`kindOf` so the two are updated together.
-}
allKinds : List String
allKinds =
    [ "reg", "pres", "ins", "itxt", "tok", "del", "mov", "inc", "tree", "mark" ]



-- FIXTURES --------------------------------------------------------------------


id1 : Id.OpId
id1 =
    Id.opId 7 (Id.replica "alice")


id2 : Id.OpId
id2 =
    Id.opId 9 (Id.replica "bob")


target : List TargetStep
target =
    [ IntoKey "notes", IntoElem id1 ]


anchor : Node.MarkAnchor
anchor =
    { ref = Just id2, side = Node.After }


{-| One op per `Action` constructor, with every optional component **present** (a `Just`
parent, a non-empty target, a seed) — the shape that exercises the most of each branch. The
count is asserted above, so adding a constructor without adding a fixture fails.
-}
sampleOps : List Op
sampleOps =
    [ opWith (SetReg target (Node.PString "x"))
    , opWith (SetKeyPresence { target = target, present = True, seed = Node.reg (Node.PInt 3) id1 })
    , opWith (InsertElem { container = target, elemId = id1, parent = Just id2, side = Rga.Left, seed = Node.reg Node.PNull id2 })
    , opWith (InsertText { container = target, start = id1, text = "hi", parent = Just id2, side = Rga.Right })
    , opWith (InsertToken { container = target, elemId = id1, parent = Just id2, side = Rga.Left, token = Node.Marker })
    , opWith (DeleteElem { container = target, elem = id1 })
    , opWith (MoveElem { container = target, elem = id1, after = Just id2 })
    , opWith (Increment { target = target, delta = -2 })
    , opWith (TreeMove { container = target, child = id1, parent = Just id2, pos = Frac.fromList [ 5, 1 ], seed = Just (Node.reg (Node.PBool True) id1) })
    , opWith (AddMark { container = target, markId = id1, type_ = "bold", value = Node.PNull, start = anchor, end = anchor })
    ]


opWith : Action -> Op
opWith action =
    { id = id1, deps = [ id2 ], action = action }


{-| The literal wire form of `sampleOps`. Written out rather than generated, so the test is a
statement about the protocol instead of a restatement of the encoder.
-}
expectedWire : String
expectedWire =
    let
        t =
            -- the shared `target`, as `{"key":…}` / `{"elem":…}` steps
            """"t":[{"key":"notes"},{"elem":[7,"alice"]}]"""

        a =
            -- the shared mark `anchor`: `sd` is "b"|"a", not the RGA's "L"|"R"
            """{"r":[9,"bob"],"sd":"a"}"""
    in
    "["
        ++ ([ """{"k":"reg",""" ++ t ++ ""","v":{"k":"string","x":"x"}}"""
            , """{"k":"pres",""" ++ t ++ ""","p":true,"s":{"t":"reg","v":{"k":"int","x":3},"s":[7,"alice"]}}"""
            , """{"k":"ins",""" ++ t ++ ""","e":[7,"alice"],"p":[9,"bob"],"sd":"L","s":{"t":"reg","v":{"k":"null"},"s":[9,"bob"]}}"""
            , """{"k":"itxt",""" ++ t ++ ""","e":[7,"alice"],"x":"hi","p":[9,"bob"],"sd":"R"}"""
            , """{"k":"tok",""" ++ t ++ ""","e":[7,"alice"],"p":[9,"bob"],"sd":"L","tk":"m"}"""
            , """{"k":"del",""" ++ t ++ ""","e":[7,"alice"]}"""
            , """{"k":"mov",""" ++ t ++ ""","e":[7,"alice"],"o":[9,"bob"]}"""
            , """{"k":"inc",""" ++ t ++ ""","d":-2}"""
            , """{"k":"tree",""" ++ t ++ ""","c":[7,"alice"],"p":[9,"bob"],"pos":[5,1],"s":{"t":"reg","v":{"k":"bool","x":true},"s":[7,"alice"]}}"""
            , """{"k":"mark",""" ++ t ++ ""","m":[7,"alice"],"ty":"bold","v":{"k":"null"},"st":""" ++ a ++ ""","en":""" ++ a ++ "}"
            ]
                |> List.map (\action -> """{"id":[7,"alice"],"deps":[[9,"bob"]],"a":""" ++ action ++ "}")
                |> String.join ","
           )
        ++ "]"


{-| Payloads a peer, a relay or an attacker can produce, each of which must decode to `Err`.
Named so a failure says which shape got through.
-}
malformed : List ( String, String )
malformed =
    [ ( "not JSON", "{{{" )
    , ( "not a list", """{"id":[1,"a"],"deps":[],"a":{"k":"del","t":[],"e":[1,"a"]}}""" )
    , ( "null", "null" )
    , ( "an op that is a string", """["nope"]""" )
    , ( "no id", """[{"deps":[],"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "no deps", """[{"id":[1,"a"],"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "no action", """[{"id":[1,"a"],"deps":[]}]""" )
    , ( "no action kind", """[{"id":[1,"a"],"deps":[],"a":{"t":[],"e":[1,"a"]}}]""" )
    , ( "unknown action kind", """[{"id":[1,"a"],"deps":[],"a":{"k":"nope","t":[]}}]""" )
    , ( "an opid that is a string", """[{"id":"1@a","deps":[],"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "an opid missing its replica", """[{"id":[1],"deps":[],"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "an opid counter that is a string", """[{"id":["1","a"],"deps":[],"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "deps that is not a list", """[{"id":[1,"a"],"deps":7,"a":{"k":"del","t":[],"e":[1,"a"]}}]""" )
    , ( "a delete with no element", """[{"id":[1,"a"],"deps":[],"a":{"k":"del","t":[]}}]""" )
    , ( "a target step that is neither key nor elem", """[{"id":[1,"a"],"deps":[],"a":{"k":"del","t":[{"nope":1}],"e":[1,"a"]}}]""" )
    , ( "a target that is not a list", """[{"id":[1,"a"],"deps":[],"a":{"k":"del","t":"notes","e":[1,"a"]}}]""" )
    , ( "an insert with no seed", """[{"id":[1,"a"],"deps":[],"a":{"k":"ins","t":[],"e":[1,"a"],"p":null,"sd":"L"}}]""" )
    , ( "an insert with an unknown side", """[{"id":[1,"a"],"deps":[],"a":{"k":"ins","t":[],"e":[1,"a"],"p":null,"sd":"sideways","s":{"t":"reg","v":{"k":"null"},"s":[1,"a"]}}}]""" )
    , ( "a text insert whose run is a number", """[{"id":[1,"a"],"deps":[],"a":{"k":"itxt","t":[],"e":[1,"a"],"x":5,"p":null,"sd":"R"}}]""" )
    , ( "an increment with no delta", """[{"id":[1,"a"],"deps":[],"a":{"k":"inc","t":[]}}]""" )
    , ( "an increment by a string", """[{"id":[1,"a"],"deps":[],"a":{"k":"inc","t":[],"d":"lots"}}]""" )
    , ( "a presence op with no seed", """[{"id":[1,"a"],"deps":[],"a":{"k":"pres","t":[],"p":true}}]""" )
    , ( "a presence op whose seed is not a node", """[{"id":[1,"a"],"deps":[],"a":{"k":"pres","t":[],"p":true,"s":{"t":"quantum"}}}]""" )
    , ( "a mark with no anchors", """[{"id":[1,"a"],"deps":[],"a":{"k":"mark","t":[],"m":[1,"a"],"ty":"bold","v":{"k":"null"}}}]""" )
    , ( "a tree move with no position", """[{"id":[1,"a"],"deps":[],"a":{"k":"tree","t":[],"c":[1,"a"],"p":null,"s":null}}]""" )
    ]


{-| A well-formed envelope carrying one good op and one that names an action kind this build
does not know — the realistic shape of a newer peer talking to an older one.
-}
corruptDelta : JE.Value
corruptDelta =
    JE.object
        [ ( "kind", JE.string "ops" )
        , ( "ops"
          , JE.list identity
                [ JD.decodeString JD.value """{"id":[99,"z"],"deps":[],"a":{"k":"del","t":[{"key":"tags"}],"e":[1,"z"]}}"""
                    |> Result.withDefault JE.null
                , JD.decodeString JD.value """{"id":[100,"z"],"deps":[],"a":{"k":"from-the-future","t":[]}}"""
                    |> Result.withDefault JE.null
                ]
          )
        ]
