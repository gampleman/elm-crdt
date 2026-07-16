module GcTests exposing (suite)

{-| Garbage collection / shallow snapshots (see `docs/04-gc.md`).

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


bytes : Doc Sample -> Int
bytes doc =
    Doc.encode doc |> JE.encode 0 |> String.length


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
        , test "gc is idempotent: gc-ing an already-compacted doc is a no-op on the read" <|
            \_ ->
                let
                    once =
                        Doc.compact (Doc.version sample) sample

                    twice =
                        Doc.compact (Doc.version once) once
                in
                Expect.equal (read once) (read twice)
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
        ]
