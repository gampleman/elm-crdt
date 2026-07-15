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

import Crdt.Doc.Internal as Doc
import Crdt.Id.Internal as Id
import Crdt.Node exposing (Node)
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



-- HELPERS ---------------------------------------------------------------------


{-| The empty `Node` a schema constructs — used to feed a decoder a node of the
"wrong" shape and assert it errors rather than crashes.
-}
emptyNodeFor : Crdt kind a -> Node
emptyNodeFor schema =
    S.emptyNode schema (Id.ctx (Id.replica "x")) |> Tuple.first



-- SEEDING HELPERS -------------------------------------------------------------


{-| Seed a concrete value through its variant seeder (via a one-element list) and
read it back, using the op-log doc.
-}
seedRead : Crdt kind a -> a -> Result S.Error (Maybe a)
seedRead schema value =
    let
        listSchema =
            S.list schema

        doc0 =
            Doc.init (Id.replica "seed") listSchema
    in
    case Doc.listAppend Path.root (schema |> S.with value) doc0 of
        Ok doc1 ->
            Doc.read doc1 |> Result.map List.head

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
            Doc.init (Id.replica "seed") listSchema
    in
    case Doc.listAppend Path.root (schema |> S.with value) doc0 of
        Ok doc1 ->
            Doc.encode doc1
                |> (\v -> Doc.decodeInto v (Doc.init (Id.replica "reader") listSchema))
                |> Result.andThen
                    (\doc2 ->
                        Doc.read doc2
                            |> Result.map List.head
                            |> Result.mapError S.errorToString
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
            [ test "fresh doc defaults to the first-declared variant" <|
                \_ ->
                    Doc.read (Doc.init (Id.replica "a") docSchema)
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
                            Doc.init (Id.replica "s") listSchema

                        appended =
                            Doc.listAppend Path.root (statusSchema |> S.with Active) doc0
                                |> Result.andThen (Doc.listAppend Path.root (statusSchema |> S.with (Snoozed 1)))
                                |> Result.andThen (Doc.listAppend Path.root (statusSchema |> S.with (Done "z")))
                    in
                    case appended of
                        Ok doc1 ->
                            Doc.read doc1 |> Expect.equal (Ok [ Active, Snoozed 1, Done "z" ])

                        Err _ ->
                            Expect.fail "append failed"
            ]
        , describe "decode errors (no crash)"
            [ test "reading a non-map node through a custom schema → Err" <|
                \_ ->
                    -- a plain int register is not a custom-type map
                    S.decodeNode statusSchema (emptyNodeFor S.int)
                        |> Expect.err
            , test "reading a map with no $tag through a custom schema → Err" <|
                \_ ->
                    -- a record (Map) that lacks the reserved $tag key
                    S.decodeNode statusSchema (emptyNodeFor docSchema)
                        |> Expect.err
            ]
        ]
