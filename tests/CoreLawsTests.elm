module CoreLawsTests exposing (suite)

{-| **The CRDT laws, over a document that uses every container at once.**

`merge` is claimed to be a join: commutative, associative, idempotent — and the cache is
claimed to always equal a full re-materialize. Those claims were previously fuzzed only
per-feature (blocks in `BlockConvergenceTests`, movable lists in `MoveTests`, and so on),
each over a schema holding one container. That leaves the interesting failure unreachable:
a bug where merging a _tree_ leaves the cache stale for a _rich text_ field, or where the
clock is advanced from one subtree and not another, only shows up when both live in the
same document.

So every property here runs over `Helpers.Edits`' one-of-everything schema, and compares
whole documents through `render` (the entire read model as a string), not one field.

Three replicas, not two: associativity and delivery-order independence are vacuous at two,
and a two-replica test cannot produce the case where `a`'s view of `b`'s history is a
strict subset of `c`'s.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Expect
import Helpers.Edits as E exposing (Op, Sample)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


suite : Test
suite =
    describe "core CRDT laws over the whole schema"
        [ describe "merge is a join"
            [ fuzz3 E.fuzzScript E.fuzzScript E.fuzzScript "commutative" <|
                \sa sb sc ->
                    let
                        ( a, b, c ) =
                            replicas sa sb sc
                    in
                    Expect.all
                        [ \_ -> Expect.equal (E.render (Doc.merge a b)) (E.render (Doc.merge b a))
                        , \_ -> Expect.equal (E.render (Doc.merge b c)) (E.render (Doc.merge c b))
                        , \_ -> Expect.equal (E.render (Doc.merge a c)) (E.render (Doc.merge c a))
                        ]
                        ()
            , fuzz3 E.fuzzScript E.fuzzScript E.fuzzScript "associative" <|
                \sa sb sc ->
                    let
                        ( a, b, c ) =
                            replicas sa sb sc
                    in
                    Expect.equal
                        (E.render (Doc.merge (Doc.merge a b) c))
                        (E.render (Doc.merge a (Doc.merge b c)))
            , fuzz2 E.fuzzScript E.fuzzScript "idempotent: merging the same peer twice adds nothing" <|
                \sa sb ->
                    let
                        ( a, b, _ ) =
                            replicas sa sb []

                        once =
                            Doc.merge a b
                    in
                    Expect.all
                        [ \_ -> Expect.equal (E.render (Doc.merge once b)) (E.render once)
                        , \_ -> Expect.equal (E.render (Doc.merge once a)) (E.render once)
                        , \_ -> Expect.equal (E.render (Doc.merge once once)) (E.render once)
                        ]
                        ()
            , fuzz E.fuzzScript "merging a replica with itself is the identity" <|
                \sa ->
                    let
                        a =
                            E.runFrom "alice" sa
                    in
                    Expect.equal (E.render (Doc.merge a a)) (E.render a)
            ]
        , describe "the cache never drifts from a full re-materialize"
            [ fuzz E.fuzzScript "after any script of local edits" <|
                \script ->
                    E.runFrom "alice" script
                        |> Doc.cacheConsistent
                        |> Expect.equal True
            , fuzz3 E.fuzzScript E.fuzzScript E.fuzzScript "after merging in every order" <|
                \sa sb sc ->
                    let
                        ( a, b, c ) =
                            replicas sa sb sc
                    in
                    [ Doc.merge (Doc.merge a b) c
                    , Doc.merge (Doc.merge a c) b
                    , Doc.merge a (Doc.merge b c)
                    , Doc.merge (Doc.merge c b) a
                    ]
                        |> List.map Doc.cacheConsistent
                        |> Expect.equal [ True, True, True, True ]
            , fuzz2 E.fuzzScript E.fuzzScript "after editing on top of a merge" <|
                \sa sb ->
                    let
                        ( a, b, _ ) =
                            replicas sa sb []
                    in
                    Doc.merge a b
                        |> E.run sb
                        |> Doc.cacheConsistent
                        |> Expect.equal True
            , fuzz (E.fuzzSchedule 3) "after an arbitrary schedule of edits and partial syncs" <|
                \steps ->
                    E.runSchedule 3 steps
                        |> List.map Doc.cacheConsistent
                        |> List.all identity
                        |> Expect.equal True
            ]
        , describe "convergence"
            [ fuzz3 E.fuzzScript E.fuzzScript E.fuzzScript "three replicas that all merge everything agree" <|
                \sa sb sc ->
                    let
                        ( a, b, c ) =
                            replicas sa sb sc

                        results =
                            [ Doc.merge (Doc.merge a b) c
                            , Doc.merge (Doc.merge b c) a
                            , Doc.merge (Doc.merge c a) b
                            ]
                                |> List.map E.render
                    in
                    case results of
                        first :: rest ->
                            Expect.equal rest (List.repeat (List.length rest) first)

                        [] ->
                            Expect.pass
            , fuzz (E.fuzzSchedule 3) "any schedule of edits and partial syncs converges once everyone syncs" <|
                \steps ->
                    case E.runSchedule 3 steps |> E.syncAll |> List.map E.render of
                        first :: rest ->
                            Expect.equal rest (List.repeat (List.length rest) first)

                        [] ->
                            Expect.pass
            , fuzz (E.fuzzSchedule 3) "the wire path and the in-memory path agree" <|
                \steps ->
                    -- `deliver` (encodeSince/decodeInto) and `merge` are two
                    -- implementations of the same join; a schedule run to convergence
                    -- through the wire must read the same as merging the same replicas
                    -- directly.
                    let
                        docs =
                            E.runSchedule 3 steps
                    in
                    Expect.equal
                        (E.syncAll docs |> List.map E.render |> List.head)
                        (List.foldl Doc.merge (E.init "empty") docs |> E.render |> Just)
            ]
        ]


{-| Three replicas that each ran their own script from a fresh document, so their histories
are fully concurrent — the hardest starting point for a merge.
-}
replicas : List Op -> List Op -> List Op -> ( Doc Sample, Doc Sample, Doc Sample )
replicas sa sb sc =
    ( E.runFrom "alice" sa
    , E.runFrom "bob" sb
    , E.runFrom "carol" sc
    )
