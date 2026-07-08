module SumTypeTests exposing (suite)

{-| Custom (sum) type schema: `S.custom` / `variantN` / `buildCustom`.

A sum type materializes to a `Map` with an LWW `$tag` register + per-variant
positional payload subtrees. This module tests everything reachable **before** the
`Ref` edit layer exists: the default variant, seeding each variant (all arities)
and reading it back, JSON round-trip, composition, and decode errors. Concurrent
_variant switching_ is tested with the edit layer (see the Ref tests), since
switching needs an editable variant op.

Values are seeded through the real variant seeder by appending them to a `list` of
the sum type and reading them back — no internal `Node` access needed.

-}

import Crdt
import Crdt.Id as Id
import Crdt.OpDoc as OpDoc
import Crdt.Path as Path
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Test exposing (Test, describe, test)



-- FIXTURES --------------------------------------------------------------------


type Status
    = Active
    | Snoozed Int
    | Done String


statusSchema : Crdt (S.Variants Status) Status
statusSchema =
    S.custom
        (\active snoozed done value ->
            case value of
                Active ->
                    active

                Snoozed n ->
                    snoozed n

                Done note ->
                    done note
        )
        |> S.variant0 "active" Active
        |> S.variant1 "snoozed" Snoozed S.int
        |> S.variant1 "done" Done S.text
        |> S.buildCustom


{-| A two-argument variant, to exercise positional payloads.
-}
type Shape
    = Circle Int
    | Rect Int Int


shapeSchema : Crdt (S.Variants Shape) Shape
shapeSchema =
    S.custom
        (\circle rect value ->
            case value of
                Circle r ->
                    circle r

                Rect w h ->
                    rect w h
        )
        |> S.variant1 "circle" Circle S.int
        |> S.variant2 "rect" Rect S.int S.int
        |> S.buildCustom



-- SEEDING HELPERS -------------------------------------------------------------


{-| Seed a concrete value through its variant seeder (via a one-element list) and
read it back, using the op-log doc.
-}
seedRead : Crdt kind a -> a -> Result Crdt.Error (Maybe a)
seedRead schema value =
    let
        listSchema =
            S.list schema

        doc0 =
            OpDoc.init (Id.replica "seed") listSchema
    in
    case OpDoc.listAppend Path.root (schema |> S.with value) doc0 of
        Ok doc1 ->
            OpDoc.read doc1 |> Result.map List.head

        Err _ ->
            Ok Nothing


{-| Seed, JSON-encode, decode on a fresh replica, and read back — proving the
custom node survives the wire (it's a `Map`, so this should be free).
-}
seedEncodeDecodeRead : Crdt kind a -> a -> Result String (Maybe a)
seedEncodeDecodeRead schema value =
    let
        listSchema =
            S.list schema

        doc0 =
            OpDoc.init (Id.replica "seed") listSchema
    in
    case OpDoc.listAppend Path.root (schema |> S.with value) doc0 of
        Ok doc1 ->
            OpDoc.encode doc1
                |> (\v -> OpDoc.decodeInto v (OpDoc.init (Id.replica "reader") listSchema))
                |> Result.andThen
                    (\doc2 ->
                        OpDoc.read doc2
                            |> Result.map List.head
                            |> Result.mapError Crdt.errorToString
                    )

        Err _ ->
            Err "seed failed"



-- SUITE -----------------------------------------------------------------------


type alias DocR =
    { status : Status }


docSchema : Crdt S.Nested DocR
docSchema =
    S.record DocR
        |> S.field "status" .status statusSchema
        |> S.build


suite : Test
suite =
    describe "Custom (sum) types"
        [ describe "default variant"
            [ test "fresh op-log doc defaults to the first-declared variant" <|
                \_ ->
                    OpDoc.read (OpDoc.init (Id.replica "a") docSchema)
                        |> Expect.equal (Ok { status = Active })
            , test "fresh state-based doc also defaults to the first variant" <|
                \_ ->
                    Crdt.read docSchema (Crdt.init (Id.replica "a") docSchema)
                        |> Expect.equal (Ok { status = Active })
            ]
        , describe "seed + read round-trip (all arities)"
            [ test "nullary variant" <|
                \_ -> seedRead statusSchema Active |> Expect.equal (Ok (Just Active))
            , test "one-arg Int variant" <|
                \_ -> seedRead statusSchema (Snoozed 42) |> Expect.equal (Ok (Just (Snoozed 42)))
            , test "one-arg text variant" <|
                \_ -> seedRead statusSchema (Done "ship it") |> Expect.equal (Ok (Just (Done "ship it")))
            , test "two-arg variant round-trips both positional args" <|
                \_ -> seedRead shapeSchema (Rect 3 4) |> Expect.equal (Ok (Just (Rect 3 4)))
            , test "one-arg variant in a two-arg schema" <|
                \_ -> seedRead shapeSchema (Circle 7) |> Expect.equal (Ok (Just (Circle 7)))
            ]
        , describe "JSON wire round-trip"
            [ test "nullary variant survives encode/decode" <|
                \_ -> seedEncodeDecodeRead statusSchema Active |> Expect.equal (Ok (Just Active))
            , test "text-payload variant survives encode/decode" <|
                \_ -> seedEncodeDecodeRead statusSchema (Done "hi") |> Expect.equal (Ok (Just (Done "hi")))
            , test "two-arg variant survives encode/decode" <|
                \_ -> seedEncodeDecodeRead shapeSchema (Rect 5 6) |> Expect.equal (Ok (Just (Rect 5 6)))
            ]
        , describe "composition"
            [ test "a record field holding a sum type round-trips" <|
                \_ -> seedRead docSchema { status = Done "review" } |> Expect.equal (Ok (Just { status = Done "review" }))
            , test "a list of sum types keeps each element's variant" <|
                \_ ->
                    let
                        listSchema =
                            S.list statusSchema

                        doc0 =
                            OpDoc.init (Id.replica "s") listSchema

                        appended =
                            OpDoc.listAppend Path.root (statusSchema |> S.with Active) doc0
                                |> Result.andThen (OpDoc.listAppend Path.root (statusSchema |> S.with (Snoozed 1)))
                                |> Result.andThen (OpDoc.listAppend Path.root (statusSchema |> S.with (Done "z")))
                    in
                    case appended of
                        Ok doc1 ->
                            OpDoc.read doc1 |> Expect.equal (Ok [ Active, Snoozed 1, Done "z" ])

                        Err _ ->
                            Expect.fail "append failed"
            ]
        , describe "decode errors (no crash)"
            [ test "reading a non-map node through a custom schema → Err" <|
                \_ ->
                    -- a plain int register is not a custom-type map
                    Crdt.read statusSchema (Crdt.init (Id.replica "a") S.int)
                        |> Expect.err
            , test "reading a map with no $tag through a custom schema → Err" <|
                \_ ->
                    -- a record (Map) that lacks the reserved $tag key
                    Crdt.read statusSchema (Crdt.init (Id.replica "a") docSchema)
                        |> Expect.err
            ]
        ]
