module DictTests exposing (suite)

{-| **Dictionary keys: overwrite, re-add, and the delete-of-something-you-never-saw race.**

`Crdt.dict` was covered by a single round-trip example (`DocTests`: create a key, remove it),
which is the shape that works no matter what the implementation does. The three-replica fuzz
harness (`Helpers.Edits`) immediately found that everything past that shape was broken:
writing a key twice was a silent no-op, re-adding after a remove read back the _old_ value,
and removing a key this replica had never seen produced a document that no replica could
read at all.

All three had the same cause: `SetKeyPresence` carries a **seed**, and `OpLog.setKeyPresenceAt`
only ever installs that seed when it _creates_ the entry — a seed for a key that already
exists is dropped on the floor (which is what makes presence a clean LWW flag rather than a
value-clobbering write). So `setKey` could not be "emit a seeded `SetKeyPresence`"; it now
writes through the ordinary value-diff path (`seedNodeAt`) whenever the entry exists, and
`removeKey` emits nothing at all for a key with no entry to tombstone.

Writing through the diff path is not just a fix for the local read — it is what makes a
concurrent overwrite _merge_, since a text value emits a character diff and a record emits
ops only for the fields that actually changed. That is the third group here, including the
one place the diff does not reach (text nested inside a record value).

The last group covers what that left: **a map key is the only container slot two ops can
create.** An element or tree node is named by the id of the op that inserted it, so no two
ops ever make the same one; two replicas that both `setKey "k"` on an absent key are both
creating _that_ key, and only the first `SetKeyPresence` to be folded installs its seed. So a
seed carrying the value made the result depend on **arrival order** — which the incremental
fold (`Crdt.Doc.Internal.rebuildIncremental` folds in arrival order, `OpLog.materialize` in
causal order) turns into two different documents from one op set. Creation now carries the
value's canonical skeleton (`Node.vacate`, identical on every replica) and the value follows
as ordinary ops, so the race has nothing left to decide.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Dict exposing (Dict)
import Expect
import Json.Encode as JE
import Test exposing (Test, describe, test)



-- DOMAIN ----------------------------------------------------------------------


type alias Prefs =
    { theme : String, size : Int }


type alias Board =
    { meta : Dict String String
    , prefs : Dict String Prefs
    }


type alias PrefsDoc =
    { theme : Ref Prefs C.Settable String
    , size : Ref Prefs C.Settable Int
    , schema : C.Schema C.Nested Prefs
    }


prefsDoc : PrefsDoc
prefsDoc =
    C.record Prefs PrefsDoc
        |> C.field "theme" .theme C.text
        |> C.field "size" .size C.int
        |> C.build


metaDict :
    C.Crdt
        (C.DictK C.Settable String)
        (Dict String String)
        { key :
            String
            -> Ref Board (C.DictK C.Settable String) (Dict String String)
            -> Ref Board C.Settable String
        }
metaDict =
    C.dict C.text


prefsDict :
    C.Crdt
        (C.DictK C.Nested Prefs)
        (Dict String Prefs)
        { key :
            String
            -> Ref Board (C.DictK C.Nested Prefs) (Dict String Prefs)
            -> Ref Board C.Nested Prefs
        }
prefsDict =
    C.dict prefsDoc


type alias BoardDoc =
    { meta : Ref Board (C.DictK C.Settable String) (Dict String String)
    , prefs : Ref Board (C.DictK C.Nested Prefs) (Dict String Prefs)
    , schema : C.Schema C.Nested Board
    }


board : BoardDoc
board =
    C.record Board BoardDoc
        |> C.field "meta" .meta metaDict
        |> C.field "prefs" .prefs prefsDict
        |> C.build



-- SUITE -----------------------------------------------------------------------


suite : Test
suite =
    describe "dictionary keys"
        [ describe "a write to an existing key is a write, not a no-op"
            [ test "setting the same key twice keeps the second value" <|
                \_ ->
                    init "alice"
                        |> edit (Edit.setKey board.meta "k" "a")
                        |> edit (Edit.setKey board.meta "k" "b")
                        |> meta
                        |> Expect.equal (Ok [ ( "k", "b" ) ])
            , test "re-adding a removed key reads the new value, not the one it held before" <|
                \_ ->
                    init "alice"
                        |> edit (Edit.setKey board.meta "k" "a")
                        |> edit (Edit.removeKey board.meta "k")
                        |> edit (Edit.setKey board.meta "k" "c")
                        |> meta
                        |> Expect.equal (Ok [ ( "k", "c" ) ])
            , test "overwriting a nested record key writes every changed field" <|
                \_ ->
                    init "alice"
                        |> edit (Edit.setKey board.prefs "p" { theme = "dark", size = 12 })
                        |> edit (Edit.setKey board.prefs "p" { theme = "light", size = 14 })
                        |> prefs
                        |> Expect.equal (Ok [ ( "p", ( "light", 14 ) ) ])
            ]
        , describe "removing a key this replica has never seen"
            [ test "emits no op at all" <|
                \_ ->
                    let
                        before =
                            init "alice"
                    in
                    Doc.opCount (edit (Edit.removeKey board.meta "ghost") before)
                        |> Expect.equal (Doc.opCount before)
            , test "loses to a concurrent create, in both merge orders, and stays readable" <|
                \_ ->
                    -- The bug this pins: the removal used to *create* the entry, seeded
                    -- with a placeholder of the wrong shape. The concurrent `setKey` then
                    -- found the key already present, kept the placeholder as its value, and
                    -- every replica's `read` failed from then on.
                    let
                        a =
                            init "alice" |> edit (Edit.removeKey board.meta "k")

                        b =
                            init "bob" |> edit (Edit.setKey board.meta "k" "hello")
                    in
                    Expect.all
                        [ \_ -> meta (Doc.merge a b) |> Expect.equal (Ok [ ( "k", "hello" ) ])
                        , \_ -> meta (Doc.merge b a) |> Expect.equal (Ok [ ( "k", "hello" ) ])
                        , \_ -> Doc.cacheConsistent (Doc.merge a b) |> Expect.equal True
                        , \_ -> Doc.cacheConsistent (Doc.merge b a) |> Expect.equal True
                        ]
                        ()
            , test "a remove the replica HAS seen still wins over the write it follows" <|
                \_ ->
                    -- The counterpart, so the fix above cannot be "removes never work":
                    -- once the entry exists, `removeKey` tombstones it as before.
                    let
                        a =
                            init "alice" |> edit (Edit.setKey board.meta "k" "hello")

                        b =
                            Doc.merge (init "bob") a |> edit (Edit.removeKey board.meta "k")
                    in
                    Expect.all
                        [ \_ -> meta (Doc.merge a b) |> Expect.equal (Ok [])
                        , \_ -> meta (Doc.merge b a) |> Expect.equal (Ok [])
                        ]
                        ()
            ]
        , describe "writing through the value-diff path makes concurrent overwrites merge"
            [ test "two replicas overwriting the same text key keep both edits" <|
                \_ ->
                    -- Both start from the shared value "x", then overwrite it. Because each
                    -- `setKey` emits a *character diff* against what that replica saw, the
                    -- inserted characters are independent elements and both survive the
                    -- merge — a re-seed would have thrown one side away entirely.
                    let
                        base =
                            init "alice" |> edit (Edit.setKey board.meta "k" "x")

                        a =
                            base |> edit (Edit.setKey board.meta "k" "ax")

                        b =
                            Doc.merge (init "bob") base |> edit (Edit.setKey board.meta "k" "xy")

                        merged =
                            Doc.merge a b
                    in
                    Expect.all
                        [ \_ -> meta merged |> Expect.equal (meta (Doc.merge b a))
                        , \_ ->
                            meta merged
                                |> Result.map (List.map (Tuple.second >> String.toList >> List.sort >> String.fromList))
                                |> Expect.equal (Ok [ "axy" ])
                        , \_ -> Doc.cacheConsistent merged |> Expect.equal True
                        ]
                        ()
            , test "a peer's edit to a field the overwrite left unchanged survives" <|
                \_ ->
                    -- `setKey` re-writing `{theme, size}` only emits ops for the fields that
                    -- differ from what it saw, so a concurrent write to a field it did not
                    -- change is not clobbered.
                    let
                        base =
                            init "alice" |> edit (Edit.setKey board.prefs "p" { theme = "dark", size = 12 })

                        a =
                            base |> edit (Edit.setKey board.prefs "p" { theme = "light", size = 12 })

                        b =
                            Doc.merge (init "bob") base
                                |> edit (Edit.set (prefsDict.key "p" board.prefs |> C.at prefsDoc.size) 20)
                    in
                    Expect.all
                        [ \_ -> prefs (Doc.merge a b) |> Expect.equal (Ok [ ( "p", ( "light", 20 ) ) ])
                        , \_ -> prefs (Doc.merge b a) |> Expect.equal (Ok [ ( "p", ( "light", 20 ) ) ])
                        ]
                        ()
            , test "a text field NESTED in a record is character-diffed too, not replaced by identity" <|
                \_ ->
                    -- This was a documented limitation until text elements became typed
                    -- (`design-docs/16-typed-sequence-content.md`). The general node diff
                    -- matches sequence elements by **id**, and a seed's ids are freshly
                    -- minted, so writing "dark" over "dark" used to delete four live
                    -- characters and insert four new ones — converging, but on "darklight"
                    -- rather than "light" when a peer edited concurrently.
                    --
                    -- A `Txt` element now holds a character, so there is no id-matched
                    -- element diff to fall into: `restoreNode` diffs the STRING, the same way
                    -- a direct `setText` does. Rewriting an unchanged value emits nothing at
                    -- all, so a peer's concurrent write is the only edit and simply wins.
                    let
                        base =
                            init "alice" |> edit (Edit.setKey board.prefs "p" { theme = "dark", size = 12 })

                        a =
                            base |> edit (Edit.setKey board.prefs "p" { theme = "dark", size = 12 })

                        b =
                            Doc.merge (init "bob") base
                                |> edit (Edit.set (prefsDict.key "p" board.prefs |> C.at prefsDoc.theme) "light")
                    in
                    Expect.all
                        [ \_ -> prefs (Doc.merge a b) |> Expect.equal (Ok [ ( "p", ( "light", 12 ) ) ])
                        , \_ -> prefs (Doc.merge b a) |> Expect.equal (prefs (Doc.merge a b))
                        , \_ -> Doc.cacheConsistent (Doc.merge a b) |> Expect.equal True
                        ]
                        ()
            ]
        , describe "two replicas creating the same key"
            [ test "a text key created concurrently keeps both values, in either merge order" <|
                \_ ->
                    -- The seed race. When creation carried the value, one replica's seed was
                    -- installed and the other's was dropped as "a seed for a key that already
                    -- exists" — so which value survived depended on which `SetKeyPresence` was
                    -- folded first, and the two merge orders disagreed.
                    let
                        a =
                            init "alice" |> edit (Edit.setKey board.meta "k" "a")

                        b =
                            init "bob" |> edit (Edit.setKey board.meta "k" "b")

                        merged =
                            Doc.merge a b
                    in
                    Expect.all
                        [ \_ -> meta merged |> Expect.equal (meta (Doc.merge b a))
                        , \_ ->
                            meta merged
                                |> Result.map (List.map (Tuple.second >> String.toList >> List.sort >> String.fromList))
                                |> Expect.equal (Ok [ "ab" ])
                        , \_ -> Doc.cacheConsistent merged |> Expect.equal True
                        ]
                        ()
            , test "a record key created concurrently resolves field by field, not seed by seed" <|
                \_ ->
                    -- Same race one level down: both creations install the same empty record,
                    -- so the fields resolve by their own ops — LWW for the int, and the text
                    -- field merges its characters like any other concurrent text write.
                    let
                        a =
                            init "alice" |> edit (Edit.setKey board.prefs "p" { theme = "dark", size = 12 })

                        b =
                            init "bob" |> edit (Edit.setKey board.prefs "p" { theme = "light", size = 14 })

                        merged =
                            Doc.merge a b
                    in
                    Expect.all
                        [ \_ -> prefs merged |> Expect.equal (prefs (Doc.merge b a))
                        , \_ ->
                            prefs merged
                                |> Result.map (List.map (Tuple.second >> Tuple.second))
                                |> Expect.equal (Ok [ 14 ])
                        , \_ ->
                            prefs merged
                                |> Result.map (List.map (Tuple.second >> Tuple.first >> String.toList >> List.sort >> String.fromList))
                                |> Expect.equal (Ok [ "adghiklrt" ])
                        , \_ -> Doc.cacheConsistent merged |> Expect.equal True
                        ]
                        ()
            , test "the value's ops arriving BEFORE the creation still read as the value" <|
                \_ ->
                    -- `setKey` is two ops now (create the key empty, then write the value), so
                    -- a transport that re-cuts a batch can deliver them in either order. The
                    -- write creates the key on the way in when it has to — stamped
                    -- `Id.unwrittenStamp`, not with its own id, or the entry's presence stamp
                    -- would depend on which op got there first and the incremental cache would
                    -- drift from a full materialize.
                    let
                        source =
                            init "alice" |> edit (Edit.setKey board.meta "k" "a")

                        receiver =
                            List.foldl ingest (init "bob") (List.reverse (perOpDeltas source))
                    in
                    Expect.all
                        [ \_ -> meta receiver |> Expect.equal (Ok [ ( "k", "a" ) ])
                        , \_ -> meta receiver |> Expect.equal (meta source)
                        , \_ -> Doc.cacheConsistent receiver |> Expect.equal True
                        , \_ -> Doc.pendingCount receiver |> Expect.equal 0
                        ]
                        ()
            , test "a removal arriving before the creation waits for it rather than creating it" <|
                \_ ->
                    -- A removal must never create the entry it tombstones: it would install a
                    -- placeholder that the real creation then refuses to replace. So it is held
                    -- pending until the key exists — and it still wins once it lands, because it
                    -- happened after the creation it is being delivered ahead of.
                    let
                        source =
                            init "alice"
                                |> edit (Edit.setKey board.meta "k" "a")
                                |> edit (Edit.removeKey board.meta "k")

                        reversed =
                            List.reverse (perOpDeltas source)

                        held =
                            ingest (List.head reversed |> Maybe.withDefault JE.null) (init "bob")

                        receiver =
                            List.foldl ingest (init "bob") reversed
                    in
                    Expect.all
                        [ \_ -> Doc.pendingCount held |> Expect.equal 1
                        , \_ -> meta held |> Expect.equal (Ok [])
                        , \_ -> Doc.cacheConsistent held |> Expect.equal True
                        , \_ -> meta receiver |> Expect.equal (Ok [])
                        , \_ -> meta receiver |> Expect.equal (meta source)
                        , \_ -> Doc.cacheConsistent receiver |> Expect.equal True
                        , \_ -> Doc.pendingCount receiver |> Expect.equal 0
                        ]
                        ()
            ]
        ]



-- HELPERS ---------------------------------------------------------------------


init : String -> Doc Board
init name =
    Doc.init (Id.replica name) board.schema


edit : (Doc Board -> Result Edit.EditError (Doc Board)) -> Doc Board -> Doc Board
edit f doc =
    f doc |> Result.withDefault doc


meta : Doc Board -> Result String (List ( String, String ))
meta =
    readWith (.meta >> Dict.toList)


prefs : Doc Board -> Result String (List ( String, ( String, Int ) ))
prefs =
    readWith
        (.prefs
            >> Dict.toList
            >> List.map (Tuple.mapSecond (\p -> ( p.theme, p.size )))
        )


readWith : (Board -> a) -> Doc Board -> Result String a
readWith f doc =
    Doc.read doc |> Result.map f |> Result.mapError (\_ -> "READ-FAILED")


{-| Re-cut a document's history into **one delta per op**, so a delivery order can separate
ops one edit minted together (`DeliveryOrderTests` fuzzes the general claim; here it pins the
two orders that matter for a key's creation). `forkAt` keeps only the ancestors of the version
it is given and leaves op ids alone, so this really is a re-cut of the same history.
-}
perOpDeltas : Doc Board -> List JE.Value
perOpDeltas doc =
    List.range 0 (Doc.historyLength doc - 1)
        |> List.map
            (\i ->
                Doc.forkAt (Id.replica "cut") (Doc.versionAt (i + 1) doc) doc
                    |> Doc.encodeSince (Doc.versionAt i doc)
            )


{-| Ingest a payload the way an application's incoming-message handler does.
-}
ingest : JE.Value -> Doc Board -> Doc Board
ingest payload doc =
    Doc.decodeInto payload doc |> Result.withDefault doc
