module DeliveryOrderTests exposing (suite)

{-| **Delivery order must not matter — including orders no well-behaved peer would produce.**

Merging is fuzzed all over this suite, but merging is the _easy_ direction: `Doc.merge` and a
delta from `encodeSince` both hand over a causally closed set of ops. The wire does not
promise that. A relay answering delta queries out of an op table, a persisted log truncated
mid-write, a payload split across messages that arrive out of order, a peer filtered by
author — each delivers a **partition of the op set in an arbitrary order**, and an op can
land before the insert it names (`PendingOpsTests` documents the four mutators that used to
silently evaporate in that case).

`PendingOpsTests` pins the specific regressions with hand-built orders over a list-and-text
schema. This module fuzzes the general claim over the whole schema:

> For **any** partition of a document's ops into deltas and **any** order of delivery, the
> receiver ends up reading exactly what the sender reads, with a consistent cache and
> nothing left pending.

"Nothing left pending" is the part that only a property can check: an op held back for a
missing dependency is invisible in the read (the document just looks like the edit didn't
happen), so a leak shows up as `pendingCount` never returning to zero once every delta has
arrived — not as a wrong read.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Expect
import Fuzz exposing (Fuzzer)
import Helpers.Edits as E exposing (Op, Sample)
import Json.Encode as JE
import Test exposing (Test, describe, fuzz2, fuzz3)


suite : Test
suite =
    describe "delivery order independence"
        [ fuzz2 E.fuzzScript order "any order of one replica's deltas reads as that replica" <|
            \script keys ->
                let
                    ( source, deltas ) =
                        deltasOf "alice" script

                    receiver =
                        deliverAll (permute keys deltas) (E.init "bob")
                in
                Expect.all
                    [ \d -> E.render d |> Expect.equal (E.render source)
                    , \d -> Doc.cacheConsistent d |> Expect.equal True
                    , \d -> Doc.pendingCount d |> Expect.equal 0
                    ]
                    receiver
        , fuzz2 E.fuzzScript order "the cache stays consistent at every intermediate step" <|
            \script keys ->
                -- Not just at the end: while ops are held back, the incrementally-maintained
                -- cache must still equal a full re-materialize of what has actually applied,
                -- or live state and history-scrubbing disagree about the same document.
                let
                    ( _, deltas ) =
                        deltasOf "alice" script
                in
                permute keys deltas
                    |> List.foldl
                        (\payload ( doc, oks ) ->
                            let
                                next =
                                    apply payload doc
                            in
                            ( next, Doc.cacheConsistent next :: oks )
                        )
                        ( E.init "bob", [] )
                    |> Tuple.second
                    |> List.all identity
                    |> Expect.equal True
        , fuzz2 E.fuzzScript order "delivering everything twice, out of order both times, changes nothing" <|
            \script keys ->
                -- Idempotence on the wire: a retried message, or a relay that replays its
                -- table, must not double-apply an op.
                let
                    ( source, deltas ) =
                        deltasOf "alice" script

                    shuffled =
                        permute keys deltas

                    once =
                        deliverAll shuffled (E.init "bob")
                in
                Expect.all
                    [ \d -> E.render d |> Expect.equal (E.render source)
                    , \d -> Doc.pendingCount d |> Expect.equal 0
                    ]
                    (deliverAll (List.reverse shuffled) once)
        , fuzz2 E.fuzzScript E.fuzzScript "STRICTLY REVERSED delivery, the worst case for a causal fold" <|
            \sa sb ->
                -- Deterministic rather than fuzzed: every op arrives before everything it
                -- depends on, so every deferrable op is deferred at least once and the
                -- pending set has to drain from the last delta backwards.
                let
                    ( a, deltasA ) =
                        deltasOf "alice" sa

                    ( b, deltasB ) =
                        deltasOf "bob" sb

                    receiver =
                        deliverAll (List.reverse (deltasA ++ deltasB)) (E.init "carol")
                in
                Expect.all
                    [ \d -> E.render d |> Expect.equal (E.render (Doc.merge a b))
                    , \d -> Doc.cacheConsistent d |> Expect.equal True
                    , \d -> Doc.pendingCount d |> Expect.equal 0
                    ]
                    receiver
        , fuzz3 E.fuzzScript E.fuzzScript order "two replicas' deltas interleaved arbitrarily equal their merge" <|
            \sa sb keys ->
                -- The realistic shape: a relay forwarding two peers' messages, with the two
                -- streams interleaved and each stream itself out of order.
                let
                    ( a, deltasA ) =
                        deltasOf "alice" sa

                    ( b, deltasB ) =
                        deltasOf "bob" sb

                    receiver =
                        deliverAll (permute keys (deltasA ++ deltasB)) (E.init "carol")
                in
                Expect.all
                    [ \d -> E.render d |> Expect.equal (E.render (Doc.merge a b))
                    , \d -> Doc.cacheConsistent d |> Expect.equal True
                    , \d -> Doc.pendingCount d |> Expect.equal 0
                    ]
                    receiver
        , fuzz3 E.fuzzScript E.fuzzScript order "a partition that SPLITS a batch still drains" <|
            \sa sb keys ->
                -- The deltas above are per-edit, so each one is internally causally closed.
                -- Here the source's history is re-cut into single-op deltas at arbitrary
                -- points (`encodeSince` over each prefix of the causal order), which is what
                -- a log replayed op-by-op or a relay filtering by author actually produces.
                let
                    source =
                        E.run sb (E.runFrom "alice" sa)

                    receiver =
                        deliverAll (permute keys (perOpDeltas source)) (E.init "carol")
                in
                Expect.all
                    [ \d -> E.render d |> Expect.equal (E.render source)
                    , \d -> Doc.cacheConsistent d |> Expect.equal True
                    , \d -> Doc.pendingCount d |> Expect.equal 0
                    ]
                    receiver
        ]



-- DELTAS ----------------------------------------------------------------------


{-| Run a script on a fresh replica, capturing the wire delta each edit produced. The result
is a partition of the document's ops into causally closed, per-edit payloads — what a peer
broadcasting after every keystroke sends.
-}
deltasOf : String -> List Op -> ( Doc Sample, List JE.Value )
deltasOf name script =
    List.foldl
        (\op ( doc, acc ) ->
            let
                next =
                    E.apply op doc
            in
            ( next, acc ++ [ Doc.encodeSince (Doc.version doc) next ] )
        )
        ( E.init name, [] )
        script


{-| Re-cut a finished document's history into **one delta per op**, by asking for the ops
after each prefix of its own causal order. Unlike `deltasOf` these can split an edit that
emitted several ops (a text run plus its block marker, a move plus its home cell), so a
delivery order can separate ops that were minted together.
-}
perOpDeltas : Doc Sample -> List JE.Value
perOpDeltas doc =
    List.range 0 (Doc.historyLength doc - 1)
        |> List.map
            (\i ->
                -- ops after the `i`th, as seen by a copy that stops at the `i+1`th: exactly
                -- one op. (`forkAt` keeps only the ancestors of the version it is given and
                -- leaves op ids alone, so this really is a re-cut of the same history.)
                Doc.forkAt (Id.replica "cut") (Doc.versionAt (i + 1) doc) doc
                    |> Doc.encodeSince (Doc.versionAt i doc)
            )


{-| Ingest a payload the way an application's incoming-message handler does, keeping the
document if it fails to decode (a decode error would otherwise hide behind a read mismatch).
-}
apply : JE.Value -> Doc Sample -> Doc Sample
apply payload doc =
    Doc.decodeInto payload doc |> Result.withDefault doc


deliverAll : List JE.Value -> Doc Sample -> Doc Sample
deliverAll payloads doc =
    List.foldl apply doc payloads



-- PERMUTATIONS ----------------------------------------------------------------


{-| Sort keys, used to permute a list whose length is only known at apply time (there is no
shuffle fuzzer). Ties keep their original relative order, so a list of equal keys is the
identity permutation and a list of distinct ones can express any order.
-}
order : Fuzzer (List Int)
order =
    Fuzz.listOfLengthBetween 0 40 (Fuzz.intRange 0 12)


permute : List Int -> List a -> List a
permute keys xs =
    let
        padded =
            keys ++ List.repeat (List.length xs) 0
    in
    List.map2 Tuple.pair padded xs
        |> List.indexedMap (\i ( k, x ) -> ( ( k, i ), x ))
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
