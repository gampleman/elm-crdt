module GcTests exposing (suite)

{-| Garbage collection / shallow snapshots (see `design-docs/04-gc.md`).

The load-bearing guarantee is **read-equivalence**: `gc` folds the ops at-or-below
a causal cut into `base` and drops them, and the document reads **identically**
before and after. Plus the things that must keep working across a compaction:
merge with a lagging peer still converges, the clock stays safe, and stable
cursors anchored below the cut still resolve.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Fuzz
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, test)



-- FIXTURE --------------------------------------------------------------------


type alias Sample =
    { title : String
    , items : List Item
    }


type alias Item =
    { label : String }


schema : Crdt S.Nested Sample
schema =
    S.record Sample
        |> S.field "title" .title S.text
        |> S.field "items" .items (S.list itemSchema)
        |> S.build


itemSchema : Crdt S.Nested Item
itemSchema =
    S.record Item |> S.field "label" .label S.text |> S.build


titlePath : Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path
todosPath =
    Path.root |> Path.field "items"


initDoc : String -> Doc Sample
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Sample -> Result Doc.Error (Doc Sample) -> Doc Sample
ok fallback =
    Result.withDefault fallback


setTitle : String -> Doc Sample -> Doc Sample
setTitle s doc =
    Doc.setText titlePath s doc |> ok doc


addItem : String -> Doc Sample -> Doc Sample
addItem label doc =
    Doc.listAppend todosPath (itemSchema |> S.with (Item label)) doc |> ok doc


removeItem : Int -> Doc Sample -> Doc Sample
removeItem i doc =
    Doc.listRemove todosPath i doc |> ok doc


{-| Edit a field _inside_ a list element — the shape that a re-applied insert op destroys,
since `Rga.insertElement` rebuilds the element from the op's own seed.
-}
setItemLabel : Int -> String -> Doc Sample -> Doc Sample
setItemLabel i label doc =
    Doc.setText (todosPath |> Path.index i |> Path.field "label") label doc |> ok doc


bytes : Doc Sample -> Int
bytes doc =
    Doc.encode doc |> JE.encode 0 |> String.length


title : Doc Sample -> String
title doc =
    read doc |> Result.map .title |> Result.withDefault "READ-FAILED"


{-| One user-level edit, bracketed for undo the way an app does it.
-}
edit : (Doc Sample -> Doc Sample) -> Doc Sample -> Doc Sample
edit f doc =
    Doc.recordEdit (Doc.version doc) (f doc)


{-| Two recorded edits, so there is an undo stack to compact across.
-}
recorded : Doc Sample
recorded =
    initDoc "alice"
        |> edit (setTitle "one")
        |> edit (setTitle "one two")


read : Doc Sample -> Result S.Error Sample
read =
    Doc.read


{-| A doc with a bit of varied history (text + list + a later text edit).
-}
sample : Doc Sample
sample =
    initDoc "alice"
        |> setTitle "Trip"
        |> addItem "pack"
        |> addItem "tickets"
        |> setTitle "Trip plan"


{-| Apply the i-th of a small menu of edits, so a fuzzer can build random
well-formed histories.
-}
applyEdit : Int -> Doc Sample -> Doc Sample
applyEdit n doc =
    case modBy 4 n of
        0 ->
            setTitle ("t" ++ String.fromInt n) doc

        1 ->
            addItem ("i" ++ String.fromInt n) doc

        _ ->
            -- toggle the title between two values / append, to vary op kinds
            setTitle (String.fromInt n) doc


buildFrom : List Int -> Doc Sample
buildFrom edits =
    List.foldl applyEdit (initDoc "alice") edits


labels : Doc Sample -> List String
labels doc =
    read doc |> Result.map (.items >> List.map .label) |> Result.withDefault [ "READ-FAILED" ]


{-| A replica that starts from `doc`'s state, over the wire (so it shares the lineage).
-}
forkOf : String -> Doc Sample -> Doc Sample
forkOf name doc =
    Doc.decodeInto (Doc.encode doc) (initDoc name) |> Result.withDefault (initDoc name)


{-| The shared starting point for the in-memory-merge-across-compaction tests.
-}
sharedStart : Doc Sample
sharedStart =
    initDoc "alice" |> setTitle "shared"


{-| B: forked from `sharedStart`, did work offline, then **compacted it all away** into
its base (`compact (version doc) doc` — the documented local-save policy). Its op store
is empty, so the only carrier of its work is `base`.
-}
offlinePeer : Doc Sample
offlinePeer =
    let
        worked =
            forkOf "bob" sharedStart |> addItem "B-offline"
    in
    Doc.compact (Doc.version worked) worked


{-| A: stayed on the shared lineage and made its own concurrent edit.
-}
localAhead : Doc Sample
localAhead =
    setTitle "shared+A" sharedStart



-- TESTS ----------------------------------------------------------------------


suite : Test
suite =
    describe "GC / shallow snapshot"
        [ test "read is unchanged after gc at the current version" <|
            \_ ->
                let
                    before =
                        read sample

                    after =
                        read (Doc.compact (Doc.version sample) sample)
                in
                Expect.equal before after
        , test "gc drops ops (op count shrinks) while the read stays equal" <|
            \_ ->
                let
                    compacted =
                        Doc.compact (Doc.version sample) sample
                in
                Expect.all
                    [ \_ -> Expect.equal (read sample) (read compacted)
                    , \_ -> Expect.lessThan (Doc.opCount sample) (Doc.opCount compacted)
                    , \_ -> Doc.opCount compacted |> Expect.equal 0
                    ]
                    ()
        , test "gc at a MID-history cut still reads identically (partial compaction)" <|
            \_ ->
                let
                    -- capture a version partway, add more, then gc at that version
                    midDoc =
                        initDoc "alice" |> setTitle "half"

                    midVersion =
                        Doc.version midDoc

                    full =
                        midDoc |> addItem "later" |> setTitle "half done"

                    compacted =
                        Doc.compact midVersion full
                in
                Expect.all
                    [ \_ -> Expect.equal (read full) (read compacted)
                    , -- some ops folded into base, some remain above the cut
                      \_ -> Expect.lessThan (Doc.opCount full) (Doc.opCount compacted)
                    ]
                    ()
        , fuzz (Fuzz.listOfLengthBetween 1 8 (Fuzz.intRange 0 20)) "fuzz: gc at the current version preserves the read" <|
            \edits ->
                let
                    doc =
                        buildFrom edits
                in
                Expect.equal (read doc) (read (Doc.compact (Doc.version doc) doc))
        , test "a lagging peer's older ops merge cleanly into a compacted doc (no resurrection)" <|
            \_ ->
                let
                    -- shared starting point
                    base =
                        initDoc "alice" |> setTitle "shared" |> addItem "one"

                    -- bob forks from the shared state
                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encode base)
                            |> Result.withDefault (initDoc "bob")

                    -- alice advances AND compacts everything she has
                    aliceAhead =
                        base |> addItem "two" |> setTitle "shared+"

                    aliceGc =
                        Doc.compact (Doc.version aliceAhead) aliceAhead

                    -- bob makes a concurrent edit (entirely below alice's cut in
                    -- the sense that bob hasn't seen alice's new ops) then they sync
                    bobEdit =
                        bob |> addItem "bob-item"

                    -- alice receives bob's ops; bob receives alice's (full) ops
                    aliceFinal =
                        aliceGc |> Doc.decodeInto (Doc.encode bobEdit) |> Result.withDefault aliceGc

                    bobFinal =
                        bobEdit |> Doc.decodeInto (Doc.encode aliceAhead) |> Result.withDefault bobEdit
                in
                -- both converge to the same read despite alice having compacted
                Expect.equal (read aliceFinal) (read bobFinal)
        , test "clock stays safe after gc: a later edit doesn't collide / loses nothing" <|
            \_ ->
                let
                    compacted =
                        Doc.compact (Doc.version sample) sample

                    -- edit after compaction; must apply on top, not vanish
                    edited =
                        setTitle "after gc" compacted
                in
                read edited |> Result.map .title |> Expect.equal (Ok "after gc")
        , test "a cursor anchored below the cut still resolves after gc" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "hello"

                    -- caret at offset 3, captured before gc
                    cur =
                        Doc.cursorAt titlePath 3 doc |> Result.toMaybe

                    compacted =
                        Doc.compact (Doc.version doc) doc
                in
                case cur of
                    Just c ->
                        -- the insert ops are now in `base`, but the element lives
                        -- on, so the cursor still resolves to offset 3
                        Doc.cursorOffset c compacted |> Expect.equal (Just 3)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "a cursor whose anchor was DELETED, then compacted away, unresolves" <|
            \_ ->
                let
                    doc =
                        initDoc "alice" |> setTitle "hello"

                    -- caret between "hel" and "lo", anchored to the second 'l'
                    cur =
                        Doc.cursorAt titlePath 3 doc |> Result.toMaybe

                    -- delete the anchored characters: "hello" -> "heo". The tombstones
                    -- still mark the spot, so the caret lands at the nearest live one.
                    deleted =
                        setTitle "heo" doc

                    -- ...but a full compaction physically drops those tombstones, and
                    -- then nothing in the sequence can place the anchor. Reporting an
                    -- offset anyway means reporting the END of the text (the live count
                    -- of a walk that never meets its anchor) — a caret that has silently
                    -- teleported, and is indistinguishable from a real end-of-text
                    -- caret. `Nothing` lets the caller draw no caret instead.
                    compacted =
                        Doc.compact (Doc.version deleted) deleted
                in
                case cur of
                    Just c ->
                        Expect.equal
                            { text = Ok "heo", beforeGc = Just 2, afterGc = Nothing }
                            { text = read compacted |> Result.map .title
                            , beforeGc = Doc.cursorOffset c deleted
                            , afterGc = Doc.cursorOffset c compacted
                            }

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "an anchor from an insert that has not ARRIVED yet also unresolves" <|
            \_ ->
                let
                    -- alice types, and captures a caret in the middle of her text
                    alice =
                        initDoc "alice" |> setTitle "hello"

                    cur =
                        Doc.cursorAt titlePath 3 alice |> Result.toMaybe

                    -- bob shares the field but has never received alice's characters, so
                    -- her anchor is unknown to him — same unplaceable case as a compacted
                    -- tombstone, and equally wrong to report as "end of text".
                    bob =
                        initDoc "bob" |> setTitle ""
                in
                case cur of
                    Just c ->
                        Doc.cursorOffset c bob |> Expect.equal Nothing

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "snapshot transfer catches up a peer behind the compaction boundary" <|
            \_ ->
                let
                    -- alice builds history then GCs ALL of it (base holds everything)
                    alice =
                        sample |> (\d -> Doc.compact (Doc.version d) d)

                    -- a brand-new peer who has none of alice's (now-gone) ops
                    fresh =
                        initDoc "bob"

                    -- a plain op-delta can't catch them up (ops are gone); encode
                    -- must send a snapshot. decodeInto adopts it.
                    caught =
                        fresh
                            |> Doc.decodeInto (Doc.encode alice)
                            |> Result.withDefault fresh
                in
                Expect.equal (read alice) (read caught)
        , test "encodeSince sends a snapshot when the peer is behind baseFrontier" <|
            \_ ->
                let
                    -- bob's version is empty (knows nothing)
                    bobVersion =
                        Doc.version (initDoc "bob")

                    alice =
                        sample |> (\d -> Doc.compact (Doc.version d) d)

                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encodeSince bobVersion alice)
                            |> Result.withDefault (initDoc "bob")
                in
                Expect.equal (read alice) (read bob)
        , test "a peer keeps converging after receiving a snapshot, then more edits" <|
            \_ ->
                let
                    alice =
                        sample |> (\d -> Doc.compact (Doc.version d) d)

                    bob =
                        initDoc "bob"
                            |> Doc.decodeInto (Doc.encode alice)
                            |> Result.withDefault (initDoc "bob")

                    -- alice edits post-GC; bob should catch up via a plain op delta
                    aliceMore =
                        setTitle "Trip plan v2" alice

                    bobCaught =
                        bob
                            |> Doc.decodeInto (Doc.encodeSince (Doc.version bob) aliceMore)
                            |> Result.withDefault bob
                in
                Expect.equal (read aliceMore) (read bobCaught)
        , describe "a peer must not hand back ops we already folded into `base`"
            [ test "our version names the base boundary, so the delta a peer offers is empty" <|
                \_ ->
                    -- The sender's side of it. `version` carries `baseFrontier` alongside
                    -- the store's tips; without that, our post-compaction edit is the only
                    -- tip we advertise, the peer cannot resolve it (it has never seen it),
                    -- and `opsAfter` concludes we know nothing and re-sends everything.
                    let
                        -- bob holds every op alice has
                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode sample)
                                |> Result.withDefault (initDoc "bob")

                        -- alice folds her whole history away, then edits on top of `base`
                        alice =
                            Doc.compact (Doc.version sample) sample |> setTitle "Trip plan v2"

                        -- what bob would send her: she is missing nothing of his
                        delta =
                            Doc.encodeSince (Doc.version alice) bob

                        -- count the ops in that delta by handing it to an empty peer
                        probe =
                            initDoc "carol"
                                |> Doc.decodeInto delta
                                |> Result.withDefault (initDoc "carol")
                    in
                    Doc.opCount probe |> Expect.equal 0
            , test "and a peer who re-sends anyway cannot clobber our post-compaction edits" <|
                \_ ->
                    -- The receiver's side, which must not depend on the sender: `encode`
                    -- ignores our version entirely, and a re-applied insert op would rebuild
                    -- the element from its original seed — reverting the label alice set
                    -- after the insert had already been folded into her base.
                    let
                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode sample)
                                |> Result.withDefault (initDoc "bob")

                        alice =
                            Doc.compact (Doc.version sample) sample
                                |> setItemLabel 0 "packed"

                        back =
                            alice
                                |> Doc.decodeInto (Doc.encode bob)
                                |> Result.withDefault alice
                    in
                    Expect.all
                        [ \d -> read d |> Expect.equal (read alice)
                        , -- nothing was re-imported into the store
                          \d -> Doc.opCount d |> Expect.equal (Doc.opCount alice)
                        , \d -> Doc.cacheConsistent d |> Expect.equal True
                        ]
                        back
            ]
        , test "gc is idempotent: gc-ing an already-compacted doc is a no-op on the read" <|
            \_ ->
                let
                    once =
                        Doc.compact (Doc.version sample) sample

                    twice =
                        Doc.compact (Doc.version once) once
                in
                Expect.equal (read once) (read twice)
        , describe "local undo/redo across a compaction boundary"
            [ test "a full compact drops the undo stack instead of leaving dead entries" <|
                \_ ->
                    let
                        doc =
                            recorded

                        -- everything the undo entries would invert is now folded into
                        -- `base`. `inverseBetween` collects ops from the STORE, so it
                        -- would find none: undo would silently do nothing while popping
                        -- the entry, and the UI's undo button would lie.
                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Expect.equal
                        { liveCanUndo = True, gcCanUndo = False, gcUndoIsNoOp = True }
                        { liveCanUndo = Doc.canUndo doc
                        , gcCanUndo = Doc.canUndo compacted
                        , gcUndoIsNoOp = title (Doc.undo compacted) == title compacted
                        }
            , test "undo still works up to the cut: entries ABOVE it survive" <|
                \_ ->
                    let
                        -- cut after the first recorded edit, then make a second one
                        upToFirst =
                            initDoc "alice" |> edit (setTitle "one")

                        cut =
                            Doc.version upToFirst

                        doc =
                            upToFirst
                                |> edit (setTitle "one two")
                                |> Doc.compact cut
                    in
                    -- the second edit's ops are above the cut and still in the store, so
                    -- its entry is honest and undo really unwinds it; the first edit's
                    -- entry was folded away and is gone.
                    Expect.equal
                        { canUndo = True, undone = "one", thenCanUndo = False }
                        { canUndo = Doc.canUndo doc
                        , undone = title (Doc.undo doc)
                        , thenCanUndo = Doc.canUndo (Doc.undo doc)
                        }
            , test "a surviving entry's undo emits real ops and syncs to a peer" <|
                \_ ->
                    let
                        upToFirst =
                            initDoc "alice" |> edit (setTitle "one")

                        alice =
                            upToFirst
                                |> edit (setTitle "one two")
                                |> Doc.compact (Doc.version upToFirst)
                                |> Doc.undo

                        bob =
                            forkOf "bob" alice
                    in
                    Expect.equal
                        { alice = "one", bob = Ok "one", consistent = True }
                        { alice = title alice
                        , bob = read bob |> Result.map .title
                        , consistent = Doc.cacheConsistent alice
                        }
            , test "the redo stack is pruned the same way (no dead redo either)" <|
                \_ ->
                    let
                        doc =
                            recorded |> Doc.undo

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Expect.equal
                        { liveCanRedo = True, gcCanRedo = False }
                        { liveCanRedo = Doc.canRedo doc
                        , gcCanRedo = Doc.canRedo compacted
                        }
            ]
        , describe "tombstone compaction (phase 4): a full compact physically drops dead tombstones"
            [ test "read is unchanged after compacting a heavily-deleted list" <|
                \_ ->
                    let
                        doc =
                            List.range 1 30
                                |> List.foldl (\i d -> addItem ("x" ++ String.fromInt i) d) (initDoc "alice")
                                -- delete the first 25 (index 0 repeatedly), keep 5
                                |> (\d -> List.range 1 25 |> List.foldl (\_ dd -> removeItem 0 dd) d)

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Expect.equal (read doc) (read compacted)
            , test "byte size collapses to roughly a fresh doc with only the survivors" <|
                \_ ->
                    let
                        deletedHeavy =
                            List.range 1 30
                                |> List.foldl (\i d -> addItem ("x" ++ String.fromInt i) d) (initDoc "alice")
                                |> (\d -> List.range 1 25 |> List.foldl (\_ dd -> removeItem 0 dd) d)

                        compacted =
                            Doc.compact (Doc.version deletedHeavy) deletedHeavy

                        -- a doc that only ever held the 5 survivors (x26..x30)
                        freshFive =
                            List.range 26 30
                                |> List.foldl (\i d -> addItem ("x" ++ String.fromInt i) d) (initDoc "alice")
                                |> (\d -> Doc.compact (Doc.version d) d)
                    in
                    Expect.all
                        [ -- the compacted doc must be far smaller than before (tombstones gone)
                          \_ -> Expect.lessThan (bytes deletedHeavy) (bytes compacted)
                        , -- and within a small factor of the tombstone-free equivalent
                          \_ -> Expect.lessThan (bytes freshFive * 2) (bytes compacted)
                        ]
                        ()
            , test "text: deleting most characters then compacting drops the char tombstones" <|
                \_ ->
                    let
                        -- type a long title, then shorten it drastically
                        doc =
                            initDoc "alice"
                                |> setTitle "the quick brown fox jumps over the lazy dog"
                                |> setTitle "hi"

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok "hi") (read compacted |> Result.map .title)
                        , \_ -> Expect.lessThan (bytes doc) (bytes compacted)
                        ]
                        ()
            , test "element identity survives: a cursor on a survivor still resolves after tombstone compaction" <|
                \_ ->
                    let
                        doc =
                            initDoc "alice"
                                |> addItem "keep-me"
                                |> addItem "delete-me"
                                |> removeItem 1

                        -- cursor into the surviving item's label text
                        cur =
                            Doc.cursorAt (Path.root |> Path.field "items" |> Path.index 0 |> Path.field "label") 4 doc
                                |> Result.toMaybe

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    case cur of
                        Just c ->
                            Doc.cursorOffset c compacted |> Expect.equal (Just 4)

                        Nothing ->
                            Expect.fail "cursorAt failed"
            , test "a lagging peer still converges after the other compacted away tombstones" <|
                \_ ->
                    let
                        base =
                            initDoc "alice" |> addItem "a" |> addItem "b" |> addItem "c"

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode base)
                                |> Result.withDefault (initDoc "bob")

                        -- alice deletes most, then fully compacts (dropping tombstones)
                        aliceGc =
                            base
                                |> removeItem 0
                                |> removeItem 0
                                |> (\d -> Doc.compact (Doc.version d) d)

                        -- bob edits concurrently, then they exchange full state
                        bobEdit =
                            bob |> addItem "bob-item"

                        aliceFinal =
                            aliceGc |> Doc.decodeInto (Doc.encode bobEdit) |> Result.withDefault aliceGc

                        bobFinal =
                            bobEdit |> Doc.decodeInto (Doc.encode aliceGc) |> Result.withDefault bobEdit
                    in
                    Expect.equal (read aliceFinal) (read bobFinal)
            , test "the read cache stays consistent (== full re-materialize) after tombstone compaction" <|
                \_ ->
                    let
                        doc =
                            initDoc "alice"
                                |> addItem "a"
                                |> addItem "b"
                                |> addItem "c"
                                |> removeItem 0
                                |> setTitle "hi"

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Doc.cacheConsistent compacted |> Expect.equal True
            , fuzz (Fuzz.listOfLengthBetween 1 10 (Fuzz.intRange 0 20)) "fuzz: full compact preserves the read even with deletions mixed in" <|
                \edits ->
                    let
                        doc =
                            List.foldl applyEdit (initDoc "alice") edits
                                -- delete whatever is at index 0 a few times (no-op if empty)
                                |> removeItem 0
                                |> removeItem 0

                        compacted =
                            Doc.compact (Doc.version doc) doc
                    in
                    Expect.all
                        [ \_ -> Expect.equal (read doc) (read compacted)
                        , \_ -> Doc.cacheConsistent compacted |> Expect.equal True
                        ]
                        ()
            ]
        , describe "stable-frontier GC (regime 2): the multi-replica-safe cut"
            [ test "two fully-synced peers: the stable frontier is their shared version" <|
                \_ ->
                    let
                        -- alice builds; bob receives everything → both at the same version
                        alice =
                            sample

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode alice)
                                |> Result.withDefault (initDoc "bob")

                        cut =
                            Doc.stableFrontier [ Doc.version alice, Doc.version bob ] alice

                        -- compacting alice below it drops ALL her ops (everyone has them)
                        compacted =
                            Doc.compact cut alice
                    in
                    Expect.all
                        [ \_ -> Expect.equal (read alice) (read compacted)
                        , \_ -> Doc.opCount compacted |> Expect.equal 0
                        ]
                        ()
            , test "a peer's UNSYNCED concurrent work is NOT below the stable cut (so it survives a merge)" <|
                \_ ->
                    let
                        -- shared base both peers have
                        base =
                            initDoc "alice" |> setTitle "shared" |> addItem "one"

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode base)
                                |> Result.withDefault (initDoc "bob")

                        -- both advance CONCURRENTLY without exchanging the new ops
                        aliceAhead =
                            base |> addItem "alice-item"

                        bobAhead =
                            bob |> addItem "bob-item"

                        -- alice computes the stable cut from the versions she knows
                        -- (her own + bob's LAST-KNOWN, which is the shared base version)
                        cut =
                            Doc.stableFrontier [ Doc.version aliceAhead, Doc.version base ] aliceAhead

                        -- she compacts below it, then bob's concurrent op arrives
                        aliceGc =
                            Doc.compact cut aliceAhead

                        aliceFinal =
                            aliceGc |> Doc.decodeInto (Doc.encode bobAhead) |> Result.withDefault aliceGc

                        bobFinal =
                            bobAhead |> Doc.decodeInto (Doc.encode aliceAhead) |> Result.withDefault bobAhead
                    in
                    Expect.all
                        [ -- alice kept her own new op (it was above the cut) and got bob's
                          \_ -> Expect.equal (read aliceFinal |> Result.map (.items >> List.length)) (Ok 3)
                        , -- both converge despite alice compacting on a stale peer version
                          \_ -> Expect.equal (read aliceFinal) (read bobFinal)
                        ]
                        ()
            , test "compacting below the stable frontier keeps both peers reading identically and converging" <|
                \_ ->
                    let
                        alice =
                            sample

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode alice)
                                |> Result.withDefault (initDoc "bob")

                        cut =
                            Doc.stableFrontier [ Doc.version alice, Doc.version bob ] alice

                        aliceGc =
                            Doc.compact cut alice

                        -- alice edits post-GC; bob merges the delta
                        aliceMore =
                            setTitle "after stable gc" aliceGc

                        bobFinal =
                            bob |> Doc.decodeInto (Doc.encode aliceMore) |> Result.withDefault bob
                    in
                    Expect.equal (read aliceMore) (read bobFinal)
            , test "a lagging peer (behind the cut) is caught up by a snapshot, not lost" <|
                \_ ->
                    let
                        alice =
                            sample

                        -- lagging peer knows NOTHING (not in the stable-frontier list)
                        lagging =
                            initDoc "carol"

                        -- alice + a synced bob agree; stable cut = their shared version.
                        -- carol is deliberately omitted (she's offline).
                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode alice)
                                |> Result.withDefault (initDoc "bob")

                        cut =
                            Doc.stableFrontier [ Doc.version alice, Doc.version bob ] alice

                        aliceGc =
                            Doc.compact cut alice

                        -- carol reconnects: a plain delta can't catch her up (ops are
                        -- gone), so `encode` sends a snapshot; `decodeInto` adopts it.
                        caroCaught =
                            lagging
                                |> Doc.decodeInto (Doc.encode aliceGc)
                                |> Result.withDefault lagging
                    in
                    Expect.equal (read aliceGc) (read caroCaught)
            , test "an empty peer list compacts nothing (no cut without knowing anyone)" <|
                \_ ->
                    let
                        cut =
                            Doc.stableFrontier [] sample

                        compacted =
                            Doc.compact cut sample
                    in
                    Expect.all
                        [ \_ -> Expect.equal (read sample) (read compacted)
                        , -- nothing dropped
                          \_ -> Doc.opCount compacted |> Expect.equal (Doc.opCount sample)
                        ]
                        ()
            , test "single-peer stable frontier equals that peer's own version (regime 1 as a special case)" <|
                \_ ->
                    let
                        cut =
                            Doc.stableFrontier [ Doc.version sample ] sample

                        viaStable =
                            Doc.compact cut sample

                        viaOwn =
                            Doc.compact (Doc.version sample) sample
                    in
                    Expect.equal (Doc.opCount viaStable) (Doc.opCount viaOwn)
            ]
        , describe "encodeFrom: shallow export at a cut (compaction you send, not self-mutation)"
            [ test "the SOURCE doc is untouched — it keeps all its ops and history" <|
                \_ ->
                    let
                        before =
                            Doc.opCount sample

                        -- the shallow export is smaller (proof it compacted)...
                        peer =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encodeFrom (Doc.version sample) sample)
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.all
                        [ -- ...yet the SOURCE still has every op — exporting doesn't shrink self
                          \_ -> Doc.opCount sample |> Expect.equal before
                        , \_ -> Expect.lessThan before (Doc.opCount peer)
                        ]
                        ()
            , test "a peer decoding the shallow export reads identically to the source" <|
                \_ ->
                    let
                        shallow =
                            Doc.encodeFrom (Doc.version sample) sample

                        peer =
                            initDoc "bob"
                                |> Doc.decodeInto shallow
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.equal (read sample) (read peer)
            , test "the shallow export carries fewer ops than the full encode (history below the cut is gone)" <|
                \_ ->
                    let
                        -- cut at the current version → the whole log folds into the base
                        shallow =
                            Doc.encodeFrom (Doc.version sample) sample

                        peerShallow =
                            initDoc "bob"
                                |> Doc.decodeInto shallow
                                |> Result.withDefault (initDoc "bob")

                        peerFull =
                            initDoc "carol"
                                |> Doc.decodeInto (Doc.encode sample)
                                |> Result.withDefault (initDoc "carol")
                    in
                    Expect.all
                        [ -- same value both ways
                          \_ -> Expect.equal (read peerShallow) (read peerFull)
                        , -- but the shallow peer holds no pre-cut ops (history redacted)
                          \_ -> Doc.opCount peerShallow |> Expect.equal 0
                        , \_ -> Expect.greaterThan 0 (Doc.opCount peerFull)
                        ]
                        ()
            , test "redaction: history below the cut can't be time-travelled by the recipient" <|
                \_ ->
                    let
                        -- a doc whose title said something sensitive, then was replaced
                        doc =
                            initDoc "alice"
                                |> setTitle "SECRET DRAFT"
                                |> setTitle "Public title"

                        -- export as-of now: the "SECRET DRAFT" edits are below the cut
                        shallow =
                            Doc.encodeFrom (Doc.version doc) doc

                        peer =
                            initDoc "bob"
                                |> Doc.decodeInto shallow
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.all
                        [ -- the live value is intact
                          \_ -> Expect.equal (Ok "Public title") (read peer |> Result.map .title)
                        , -- and there is NO earlier version to scrub back to
                          \_ -> Doc.historyLength peer |> Expect.equal 0
                        ]
                        ()
            , test "the recipient of a shallow export keeps converging on later edits" <|
                \_ ->
                    let
                        shallow =
                            Doc.encodeFrom (Doc.version sample) sample

                        peer =
                            initDoc "bob"
                                |> Doc.decodeInto shallow
                                |> Result.withDefault (initDoc "bob")

                        -- source edits on, sends a delta; peer merges it
                        sourceMore =
                            setTitle "Trip plan v2" sample

                        peerCaught =
                            peer
                                |> Doc.decodeInto (Doc.encodeSince (Doc.version peer) sourceMore)
                                |> Result.withDefault peer
                    in
                    Expect.equal (read sourceMore) (read peerCaught)
            , test "encodeFrom at a MID-history cut keeps the tail above it" <|
                \_ ->
                    let
                        mid =
                            initDoc "alice" |> setTitle "half"

                        midV =
                            Doc.version mid

                        full =
                            mid |> addItem "later" |> setTitle "half done"

                        shallow =
                            Doc.encodeFrom midV full

                        peer =
                            initDoc "bob"
                                |> Doc.decodeInto shallow
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.all
                        [ -- value identical to the source
                          \_ -> Expect.equal (read full) (read peer)
                        , -- some ops remain (the tail above the cut), but fewer than full
                          \_ -> Expect.greaterThan 0 (Doc.opCount peer)
                        , \_ -> Expect.lessThan (Doc.opCount full) (Doc.opCount peer)
                        ]
                        ()
            ]
        , describe "in-memory merge across a compaction boundary"
            -- REGRESSION: `merge` unions op stores and used to keep `local.base`
            -- unconditionally, ignoring `incoming.base`. History the incoming replica had
            -- folded into its base is in NO store, so it vanished — silently (the result
            -- still passed `cacheConsistent`), and asymmetrically (`merge a b` disagreed
            -- with `merge b a`). Only the wire path (`decodeInto`, snapshot adoption)
            -- handled it, and only that path was covered. Scenario throughout: B works
            -- offline, compacts (the documented single-replica save policy), reconnects.
            [ test "the incoming doc really does hold its work in `base` alone" <|
                \_ ->
                    Expect.all
                        [ \_ -> Doc.opCount offlinePeer |> Expect.equal 0
                        , \_ -> labels offlinePeer |> List.member "B-offline" |> Expect.equal True
                        ]
                        ()
            , test "merging it in adopts that base instead of dropping its work" <|
                \_ ->
                    let
                        merged =
                            Doc.merge localAhead offlinePeer
                    in
                    Expect.all
                        [ \_ -> labels merged |> List.member "B-offline" |> Expect.equal True
                        , -- and our own concurrent edit survives the adoption
                          \_ -> Result.map .title (read merged) |> Expect.equal (Ok "shared+A")
                        ]
                        ()
            , test "merge is commutative across the boundary" <|
                \_ ->
                    Expect.equal
                        (read (Doc.merge localAhead offlinePeer))
                        (read (Doc.merge offlinePeer localAhead))
            , test "and idempotent: merging twice changes nothing" <|
                \_ ->
                    let
                        once =
                            Doc.merge localAhead offlinePeer
                    in
                    Expect.equal (read once) (read (Doc.merge once offlinePeer))
            , test "the adopted result is cache-consistent and keeps editing safely" <|
                \_ ->
                    let
                        merged =
                            Doc.merge localAhead offlinePeer

                        edited =
                            merged |> addItem "after-adopt" |> setTitle "final"
                    in
                    Expect.all
                        [ \_ -> Doc.cacheConsistent merged |> Expect.equal True
                        , \_ -> Doc.cacheConsistent edited |> Expect.equal True
                        , -- a fresh id must not collide with anything the adopted base
                          -- carried, or one of these two items would be lost
                          \_ -> labels edited |> List.member "after-adopt" |> Expect.equal True
                        , \_ -> labels edited |> List.member "B-offline" |> Expect.equal True
                        , \_ -> Result.map .title (read edited) |> Expect.equal (Ok "final")
                        ]
                        ()
            , test "in-memory merge now agrees with the wire path" <|
                \_ ->
                    let
                        viaMerge =
                            Doc.merge localAhead offlinePeer

                        viaWire =
                            Doc.decodeInto (Doc.encode offlinePeer) localAhead
                                |> Result.withDefault localAhead
                    in
                    Expect.equal (read viaMerge) (read viaWire)
            , test "an UNcompacted incoming doc still takes the incremental path" <|
                \_ ->
                    -- the fix must not change the common case: nothing is adopted, so
                    -- containers the merge didn't touch keep their identity.
                    let
                        peer =
                            forkOf "bob" sample |> addItem "peer-item"
                    in
                    Expect.all
                        [ \_ -> labels (Doc.merge sample peer) |> List.member "peer-item" |> Expect.equal True
                        , \_ ->
                            Expect.equal
                                (read (Doc.merge sample peer))
                                (read (Doc.merge peer sample))
                        ]
                        ()
            ]
        ]
