module CompactionPropertyTests exposing (suite)

{-| **Compaction as a cross-cutting axis, not a feature.**

`GcTests` covers compaction thoroughly — but almost always over a document holding one list
or one text field, with a hand-built history. That leaves the failure mode compaction is
most likely to have: it walks _every_ node kind (`OpLog.compact` folds ops into `base`, and
a full compact then physically rebuilds each RGA), so the container it gets wrong is the one
nobody compacted in a test. A movable list's move cells, a tree's tombstoned node ids, a
rich-text mark anchored at a character that the tombstone pass drops, an op-set's
contribution keys — those live in the same `base` as the text everyone tests.

So every property here compacts a document that holds **all of them at once**
(`Helpers.Edits`), and compares the whole read model through `render`.

The claim under test is one sentence: **compaction is invisible.** Any cut, any container,
before or after a merge, mid-schedule or at the end — the read is unchanged, the cache still
equals a full re-materialize, and everyone still converges. Only the _history_ below the cut
is gone (that trade is example-tested in `GcTests`).

Every cut taken here is one the library documents as safe:

  - a lone replica with no peers may compact **anywhere**, including its own frontier
    (`full` below) — that is the only cut that triggers the physical tombstone rebuild;
  - a replica with peers compacts at the **stable frontier**, the cut everyone has delivered
    past (`E.Compact`, and the last group here).

Compacting at one's own frontier while a peer holds concurrent work is outside that envelope
(`design-docs/04-gc.md`), and a property that did it would only re-derive a documented
limitation rather than test compaction.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Expect
import Fuzz
import Helpers.Edits as E exposing (Sample)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)


suite : Test
suite =
    describe "compaction is invisible, over every container at once"
        [ describe "one document"
            [ fuzz E.fuzzScript "a full compact preserves the read and the cache" <|
                \script ->
                    let
                        doc =
                            E.runFrom "alice" script
                    in
                    Expect.all
                        [ \d -> E.render d |> Expect.equal (E.render doc)
                        , \d -> Doc.cacheConsistent d |> Expect.equal True
                        ]
                        (full doc)
            , fuzz2 E.fuzzScript cutPoint "a cut ANYWHERE in history preserves the read and the cache" <|
                \script k ->
                    -- A mid-history cut is the harder case: ops remain above the cut whose
                    -- `deps` now point into `base`, and tombstones must be kept because a
                    -- surviving op may still anchor after one.
                    let
                        doc =
                            E.runFrom "alice" script

                        cut =
                            Doc.versionAt (k * Doc.historyLength doc // 4) doc

                        compacted =
                            Doc.compact cut doc
                    in
                    Expect.all
                        [ \d -> E.render d |> Expect.equal (E.render doc)
                        , \d -> Doc.cacheConsistent d |> Expect.equal True
                        ]
                        compacted
            , fuzz E.fuzzScript "compacting an already-compacted document changes nothing" <|
                \script ->
                    let
                        once =
                            full (E.runFrom "alice" script)
                    in
                    Expect.all
                        [ \d -> E.render d |> Expect.equal (E.render once)
                        , \d -> Doc.opCount d |> Expect.equal (Doc.opCount once)
                        ]
                        (full once)
            , fuzz E.fuzzScript "a full compact never grows the op count" <|
                \script ->
                    let
                        doc =
                            E.runFrom "alice" script
                    in
                    Doc.opCount (full doc) <= Doc.opCount doc |> Expect.equal True
            , fuzz E.fuzzScript "editing on top of a compacted document still works" <|
                \script ->
                    -- The clock has to survive the fold: stamps that moved into `base` still
                    -- count towards `maxCounter`, or a later edit collides with a folded op.
                    let
                        doc =
                            full (E.runFrom "alice" script)

                        after =
                            E.run script doc
                    in
                    Expect.all
                        [ \d -> Doc.cacheConsistent d |> Expect.equal True
                        , \d ->
                            -- the same script applied to the compacted and uncompacted doc
                            -- reads the same, so the fold lost nothing the edits depend on
                            E.render d |> Expect.equal (E.render (E.run script (E.runFrom "alice" script)))
                        ]
                        after
            ]
        , describe "in a concurrent schedule"
            [ fuzz2 (E.fuzzSchedule 3) cutPoint "compacting one replica mid-schedule changes nobody's read" <|
                \steps k ->
                    -- The same random schedule, run twice: once as generated, once with a
                    -- `Compact 0` spliced into it. Everything after the splice — deliveries
                    -- to and from the compacted replica, edits on top of its rebuilt base,
                    -- snapshot catch-up for peers now behind its `baseFrontier` — has to
                    -- land on the same document.
                    let
                        cutAt =
                            k * List.length steps // 4

                        withCompaction =
                            List.take cutAt steps ++ (E.Compact 0 :: List.drop cutAt steps)
                    in
                    converged withCompaction |> Expect.equal (converged steps)
            , fuzz2 (E.fuzzSchedule 3) cutPoint "and leaves every replica cache-consistent with nothing pending" <|
                \steps k ->
                    let
                        cutAt =
                            k * List.length steps // 4

                        docs =
                            List.take cutAt steps
                                ++ (E.Compact 0 :: List.drop cutAt steps)
                                |> E.runSchedule 3
                                |> E.syncAll
                    in
                    Expect.all
                        [ \ds -> List.map Doc.cacheConsistent ds |> Expect.equal (List.repeat 3 True)
                        , \ds -> List.map Doc.pendingCount ds |> Expect.equal (List.repeat 3 0)
                        ]
                        docs
            , fuzz3 (E.fuzzSchedule 3) E.fuzzScript E.fuzzScript "work done AFTER the cut, on top of ops now in `base`, still merges" <|
                \steps lagging more ->
                    -- r0 garbage-collects at the cut everyone has delivered past, and only
                    -- then does r1 edit — without shipping it. r1's new ops are not below
                    -- anyone's cut, but their `deps` name ops that are now inside r0's
                    -- `base`, so reconciling them later works against a store that no
                    -- longer holds what they point at.
                    let
                        synced =
                            E.runSchedule 3 steps |> E.syncAll

                        versions =
                            List.map Doc.version synced

                        compacted =
                            synced
                                |> updateAt 0 (\d -> Doc.compact (Doc.stableFrontier versions d) d)
                                |> updateAt 1 (E.run lagging)
                                |> updateAt 0 (E.run more)

                        baseline =
                            synced
                                |> updateAt 1 (E.run lagging)
                                |> updateAt 0 (E.run more)
                    in
                    Expect.all
                        [ \ds -> renders (E.syncAll ds) |> Expect.equal (renders (E.syncAll baseline))
                        , \ds -> E.syncAll ds |> List.map Doc.cacheConsistent |> Expect.equal (List.repeat 3 True)
                        ]
                        compacted
            ]
        , describe "an op already inside `base` must not be re-applied from the wire"
            [ test "a peer re-sending what the receiver already folded keeps the receiver's later edits" <|
                \_ ->
                    -- Deterministic, and squarely inside the safety envelope: the cut r0
                    -- takes is the one op r1 authored, so r1 has delivered past it. All that
                    -- happens afterwards is a redundant re-delivery — a reconnect, a relay
                    -- replaying its table, a peer that broadcasts its whole state on a timer.
                    let
                        r1 =
                            E.runFrom "r1" [ E.AddTodo "a" ]

                        -- r0 receives the insert, folds it into `base`, then edits the item
                        -- that now exists only inside `base`
                        r0 =
                            E.deliver r1 (E.init "r0") |> full |> E.apply (E.ToggleTodo 0)

                        -- r0's version names r0's toggle, an op r1 has never seen, so r1
                        -- cannot resolve ancestry and hands over its whole store again
                        viaWire =
                            E.deliver r1 r0
                    in
                    Expect.all
                        [ \d -> E.render d |> Expect.equal (E.render r0)
                        , \d -> E.render d |> Expect.equal (E.render (Doc.merge r0 r1))
                        , \d -> Doc.cacheConsistent d |> Expect.equal True
                        ]
                        viaWire
            ]
        , describe "the stable frontier: the one cut every replica may compact below"
            [ fuzz2 (E.fuzzSchedule 3) E.fuzzScript "everyone compacting below it keeps everyone converged" <|
                \steps after ->
                    let
                        synced =
                            E.runSchedule 3 steps |> E.syncAll

                        versions =
                            List.map Doc.version synced

                        compacted =
                            synced |> List.map (\d -> Doc.compact (Doc.stableFrontier versions d) d)

                        -- keep editing afterwards: r1 on top of its compacted base, then
                        -- everyone reconciles again
                        continued =
                            compacted |> updateAt 1 (E.run after) |> E.syncAll
                    in
                    Expect.all
                        [ \ds -> renders ds |> allEqual |> Expect.equal True
                        , \ds -> List.map Doc.cacheConsistent ds |> Expect.equal (List.repeat 3 True)
                        , \ds ->
                            renders ds
                                |> Expect.equal
                                    (renders (synced |> updateAt 1 (E.run after) |> E.syncAll))
                        ]
                        continued
            ]
        ]



-- HELPERS ---------------------------------------------------------------------


{-| Compact a document's **whole** history into its base — the always-safe single-replica
cut, and the only one that triggers the physical tombstone rebuild.
-}
full : Doc Sample -> Doc Sample
full doc =
    Doc.compact (Doc.version doc) doc


{-| A cut expressed in quarters of whatever length it is applied to, so one fuzzer works for
both a history and a schedule (0 = the very start, 4 = the end).
-}
cutPoint : Fuzz.Fuzzer Int
cutPoint =
    Fuzz.intRange 0 4


{-| Run a schedule, sync everyone, and return the one read they agree on (or the list of
disagreeing reads, which is what makes a failure legible).
-}
converged : List E.Step -> List String
converged steps =
    E.runSchedule 3 steps |> E.syncAll |> renders


renders : List (Doc Sample) -> List String
renders =
    List.map E.render


allEqual : List String -> Bool
allEqual xs =
    case xs of
        [] ->
            True

        first :: rest ->
            List.all ((==) first) rest


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap
        (\j x ->
            if i == j then
                f x

            else
                x
        )
