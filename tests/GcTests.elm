module GcTests exposing (suite)

{-| Garbage collection / shallow snapshots (see `docs/04-gc.md`).

The load-bearing guarantee is **read-equivalence**: `gc` folds the ops at-or-below
a causal cut into `base` and drops them, and the document reads **identically**
before and after. Plus the things that must keep working across a compaction:
merge with a lagging peer still converges, the clock stays safe, and stable
cursors anchored below the cut still resolve.

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema as S exposing (Crdt)
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)



-- FIXTURE --------------------------------------------------------------------


type alias Doc =
    { title : String
    , items : List Item
    }


type alias Item =
    { label : String }


schema : Crdt Doc
schema =
    S.record Doc
        |> S.field "title" .title S.text
        |> S.field "items" .items (S.list itemSchema)
        |> S.build


itemSchema : Crdt Item
itemSchema =
    S.record Item |> S.field "label" .label S.text |> S.build


titlePath : Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path
todosPath =
    Path.root |> Path.field "items"


initDoc : String -> OpDoc Doc
initDoc name =
    OpDoc.init (Id.replica name) schema


ok : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
ok fallback =
    Result.withDefault fallback


setTitle : String -> OpDoc Doc -> OpDoc Doc
setTitle s doc =
    OpDoc.setText titlePath s doc |> ok doc


addItem : String -> OpDoc Doc -> OpDoc Doc
addItem label doc =
    OpDoc.listAppend todosPath (itemSchema |> S.with (Item label)) doc |> ok doc


read : OpDoc Doc -> Result S.Error Doc
read =
    OpDoc.read


{-| A doc with a bit of varied history (text + list + a later text edit).
-}
sample : OpDoc Doc
sample =
    initDoc "alice"
        |> setTitle "Trip"
        |> addItem "pack"
        |> addItem "tickets"
        |> setTitle "Trip plan"


{-| Apply the i-th of a small menu of edits, so a fuzzer can build random
well-formed histories.
-}
applyEdit : Int -> OpDoc Doc -> OpDoc Doc
applyEdit n doc =
    case modBy 4 n of
        0 ->
            setTitle ("t" ++ String.fromInt n) doc

        1 ->
            addItem ("i" ++ String.fromInt n) doc

        _ ->
            -- toggle the title between two values / append, to vary op kinds
            setTitle (String.fromInt n) doc


buildFrom : List Int -> OpDoc Doc
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
                        read (OpDoc.gc (OpDoc.version sample) sample)
                in
                Expect.equal before after
        , test "gc drops ops (op count shrinks) while the read stays equal" <|
            \_ ->
                let
                    compacted =
                        OpDoc.gc (OpDoc.version sample) sample
                in
                Expect.all
                    [ \_ -> Expect.equal (read sample) (read compacted)
                    , \_ -> Expect.lessThan (OpDoc.opCount sample) (OpDoc.opCount compacted)
                    , \_ -> OpDoc.opCount compacted |> Expect.equal 0
                    ]
                    ()
        , test "gc at a MID-history cut still reads identically (partial compaction)" <|
            \_ ->
                let
                    -- capture a version partway, add more, then gc at that version
                    midDoc =
                        initDoc "alice" |> setTitle "half"

                    midVersion =
                        OpDoc.version midDoc

                    full =
                        midDoc |> addItem "later" |> setTitle "half done"

                    compacted =
                        OpDoc.gc midVersion full
                in
                Expect.all
                    [ \_ -> Expect.equal (read full) (read compacted)
                    , -- some ops folded into base, some remain above the cut
                      \_ -> Expect.lessThan (OpDoc.opCount full) (OpDoc.opCount compacted)
                    ]
                    ()
        , fuzz (Fuzz.listOfLengthBetween 1 8 (Fuzz.intRange 0 20)) "fuzz: gc at the current version preserves the read" <|
            \edits ->
                let
                    doc =
                        buildFrom edits
                in
                Expect.equal (read doc) (read (OpDoc.gc (OpDoc.version doc) doc))
        , test "a lagging peer's older ops merge cleanly into a compacted doc (no resurrection)" <|
            \_ ->
                let
                    -- shared starting point
                    base =
                        initDoc "alice" |> setTitle "shared" |> addItem "one"

                    -- bob forks from the shared state
                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode base)
                            |> Result.withDefault (initDoc "bob")

                    -- alice advances AND compacts everything she has
                    aliceAhead =
                        base |> addItem "two" |> setTitle "shared+"

                    aliceGc =
                        OpDoc.gc (OpDoc.version aliceAhead) aliceAhead

                    -- bob makes a concurrent edit (entirely below alice's cut in
                    -- the sense that bob hasn't seen alice's new ops) then they sync
                    bobEdit =
                        bob |> addItem "bob-item"

                    -- alice receives bob's ops; bob receives alice's (full) ops
                    aliceFinal =
                        aliceGc |> OpDoc.decodeInto (OpDoc.encode bobEdit) |> Result.withDefault aliceGc

                    bobFinal =
                        bobEdit |> OpDoc.decodeInto (OpDoc.encode aliceAhead) |> Result.withDefault bobEdit
                in
                -- both converge to the same read despite alice having compacted
                Expect.equal (read aliceFinal) (read bobFinal)
        , test "clock stays safe after gc: a later edit doesn't collide / loses nothing" <|
            \_ ->
                let
                    compacted =
                        OpDoc.gc (OpDoc.version sample) sample

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
                        OpDoc.cursorAt titlePath 3 doc |> Result.toMaybe

                    compacted =
                        OpDoc.gc (OpDoc.version doc) doc
                in
                case cur of
                    Just c ->
                        -- the insert ops are now in `base`, but the element lives
                        -- on, so the cursor still resolves to offset 3
                        OpDoc.cursorOffset c compacted |> Expect.equal (Just 3)

                    Nothing ->
                        Expect.fail "cursorAt failed"
        , test "snapshot transfer catches up a peer behind the compaction boundary" <|
            \_ ->
                let
                    -- alice builds history then GCs ALL of it (base holds everything)
                    alice =
                        sample |> (\d -> OpDoc.gc (OpDoc.version d) d)

                    -- a brand-new peer who has none of alice's (now-gone) ops
                    fresh =
                        initDoc "bob"

                    -- a plain op-delta can't catch them up (ops are gone); encode
                    -- must send a snapshot. decodeInto adopts it.
                    caught =
                        fresh
                            |> OpDoc.decodeInto (OpDoc.encode alice)
                            |> Result.withDefault fresh
                in
                Expect.equal (read alice) (read caught)
        , test "encodeSince sends a snapshot when the peer is behind baseFrontier" <|
            \_ ->
                let
                    -- bob's version is empty (knows nothing)
                    bobVersion =
                        OpDoc.version (initDoc "bob")

                    alice =
                        sample |> (\d -> OpDoc.gc (OpDoc.version d) d)

                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encodeSince bobVersion alice)
                            |> Result.withDefault (initDoc "bob")
                in
                Expect.equal (read alice) (read bob)
        , test "a peer keeps converging after receiving a snapshot, then more edits" <|
            \_ ->
                let
                    alice =
                        sample |> (\d -> OpDoc.gc (OpDoc.version d) d)

                    bob =
                        initDoc "bob"
                            |> OpDoc.decodeInto (OpDoc.encode alice)
                            |> Result.withDefault (initDoc "bob")

                    -- alice edits post-GC; bob should catch up via a plain op delta
                    aliceMore =
                        setTitle "Trip plan v2" alice

                    bobCaught =
                        bob
                            |> OpDoc.decodeInto (OpDoc.encodeSince (OpDoc.version bob) aliceMore)
                            |> Result.withDefault bob
                in
                Expect.equal (read aliceMore) (read bobCaught)
        , test "gc is idempotent: gc-ing an already-compacted doc is a no-op on the read" <|
            \_ ->
                let
                    once =
                        OpDoc.gc (OpDoc.version sample) sample

                    twice =
                        OpDoc.gc (OpDoc.version once) once
                in
                Expect.equal (read once) (read twice)
        ]
