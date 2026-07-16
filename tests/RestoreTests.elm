module RestoreTests exposing (suite)

{-| History scrubbing (`Doc.historyLength` / `versionAt`) and **collaborative
restore** (`Doc.restoreTo`).

`restoreTo` is not a local rewind: it diffs a past version against the present
and emits the fresh, winning ops that turn the present back into the past, so the
revert syncs and converges like any other edit. The properties that matter:

  - scrubbing: `readAt (versionAt n)` walks the document through its edit history;
  - a restore makes the live document _read_ as the past version;
  - the restore is expressed as ops (the op count goes UP, never down);
  - a restore **propagates through merge** — a peer that merges the restoring
    replica also ends up at the restored value (the whole point vs. a local rewind);
  - identity survives where it can: a register untouched since the version keeps
    its id, so a stable cursor still resolves after the restore;
  - works across every container: register, text, counter, dict, and a
    `movableList` (membership + reordering).

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Dict
import Expect
import Test exposing (Test, describe, test)


type alias Board =
    { title : String
    , todos : List Todo
    , notes : Dict.Dict String String
    , votes : Int
    }


type alias Todo =
    { text : String
    , done : Bool
    }


schema : Crdt S.Nested Board
schema =
    S.record Board
        |> S.field "title" .title S.text
        |> S.field "todos" .todos (S.movableList todoSchema)
        |> S.field "notes" .notes (S.dict S.text)
        |> S.field "votes" .votes S.counter
        |> S.build


todoSchema : Crdt S.Nested Todo
todoSchema =
    S.record Todo
        |> S.field "text" .text S.text
        |> S.field "done" .done S.bool
        |> S.build


titlePath : Path
titlePath =
    Path.root |> Path.field "title"


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


notesPath : Path
notesPath =
    Path.root |> Path.field "notes"


votesPath : Path
votesPath =
    Path.root |> Path.field "votes"


initDoc : String -> Doc Board
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Board -> Result Doc.Error (Doc Board) -> Doc Board
ok fallback =
    Result.withDefault fallback


todo : String -> S.Seed
todo t =
    todoSchema |> S.with (Todo t False)


texts : Board -> List String
texts board =
    List.map .text board.todos


{-| Build a doc with three todos A, B, C and a title; return it.
-}
abcDoc : Doc Board
abcDoc =
    let
        a0 =
            initDoc "alice"

        a1 =
            Doc.setText titlePath "Trip" a0 |> ok a0

        a2 =
            Doc.listAppend todosPath (todo "A") a1 |> ok a1

        a3 =
            Doc.listAppend todosPath (todo "B") a2 |> ok a2
    in
    Doc.listAppend todosPath (todo "C") a3 |> ok a3


suite : Test
suite =
    describe "Doc history scrubber + restore"
        [ describe "scrubber (historyLength / versionAt)"
            [ test "historyLength counts every edit op" <|
                \_ ->
                    -- title diff = "Trip" is 4 char-insert ops; 3 list appends
                    Doc.historyLength abcDoc
                        |> Expect.equal (Doc.opCount abcDoc)
            , test "versionAt 0 is the empty document" <|
                \_ ->
                    Doc.readAt (Doc.versionAt 0 abcDoc) abcDoc
                        |> Expect.equal (Ok (Board "" [] Dict.empty 0))
            , test "versionAt historyLength is the current state" <|
                \_ ->
                    let
                        n =
                            Doc.historyLength abcDoc
                    in
                    Doc.readAt (Doc.versionAt n abcDoc) abcDoc
                        |> Result.map texts
                        |> Expect.equal (Ok [ "A", "B", "C" ])
            , test "scrubbing is monotonic: each later step has >= todos" <|
                \_ ->
                    let
                        n =
                            Doc.historyLength abcDoc

                        counts =
                            List.range 0 n
                                |> List.map
                                    (\i ->
                                        Doc.readAt (Doc.versionAt i abcDoc) abcDoc
                                            |> Result.map (.todos >> List.length)
                                            |> Result.withDefault -1
                                    )
                    in
                    Expect.equal counts (List.sort counts)
            , test "versionAt clamps out-of-range steps" <|
                \_ ->
                    Expect.equal
                        (Doc.readAt (Doc.versionAt 9999 abcDoc) abcDoc |> Result.map texts)
                        (Ok [ "A", "B", "C" ])
            ]
        , describe "restoreTo — local effect"
            [ test "restoring to an earlier version reads as that version" <|
                \_ ->
                    let
                        -- snapshot after A,B,C; then add D and edit the title
                        v =
                            Doc.version abcDoc

                        d1 =
                            Doc.listAppend todosPath (todo "D") abcDoc |> ok abcDoc

                        d2 =
                            Doc.setText titlePath "Changed" d1 |> ok d1

                        restored =
                            Doc.restoreTo v d2
                    in
                    Doc.read restored
                        |> Result.map (\b -> ( b.title, texts b ))
                        |> Expect.equal (Ok ( "Trip", [ "A", "B", "C" ] ))
            , test "restore is expressed as ops: the op count grows, history is not truncated" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        d1 =
                            Doc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        before =
                            Doc.opCount d1

                        restored =
                            Doc.restoreTo v d1
                    in
                    Expect.greaterThan before (Doc.opCount restored)
            , test "restoring to the current version is a no-op on the read" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        restored =
                            Doc.restoreTo v abcDoc
                    in
                    Expect.equal (Doc.read abcDoc |> Result.map texts)
                        (Doc.read restored |> Result.map texts)
            , test "restore re-creates a todo that was deleted since the version" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        d1 =
                            Doc.listRemove todosPath 1 abcDoc |> ok abcDoc

                        -- B is gone now
                        gone =
                            Doc.read d1 |> Result.map texts

                        restored =
                            Doc.restoreTo v d1
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "A", "C" ]) gone
                        , \_ -> Expect.equal (Ok [ "A", "B", "C" ]) (Doc.read restored |> Result.map texts)
                        ]
                        ()
            , test "restore reverts a counter via a compensating increment" <|
                \_ ->
                    let
                        base =
                            Doc.increment votesPath 3 (initDoc "alice") |> ok (initDoc "alice")

                        v =
                            Doc.version base

                        more =
                            Doc.increment votesPath 10 base |> ok base

                        restored =
                            Doc.restoreTo v more
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok 13) (Doc.read more |> Result.map .votes)
                        , \_ -> Expect.equal (Ok 3) (Doc.read restored |> Result.map .votes)
                        ]
                        ()
            , test "restore reverts dict add/remove" <|
                \_ ->
                    let
                        withNote =
                            Doc.setKey notesPath "k" (S.text |> S.with "hello") (initDoc "alice")
                                |> ok (initDoc "alice")

                        v =
                            Doc.version withNote

                        changed =
                            withNote
                                |> (\d -> Doc.removeKey notesPath "k" d |> ok d)
                                |> (\d -> Doc.setKey notesPath "j" (S.text |> S.with "new") d |> ok d)

                        restored =
                            Doc.restoreTo v changed
                    in
                    Expect.equal (Ok (Dict.fromList [ ( "k", "hello" ) ]))
                        (Doc.read restored |> Result.map .notes)
            , test "restore reverts a reordering on a movableList" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        moved =
                            Doc.listMove todosPath 0 2 abcDoc |> ok abcDoc

                        restored =
                            Doc.restoreTo v moved
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "B", "C", "A" ]) (Doc.read moved |> Result.map texts)
                        , \_ -> Expect.equal (Ok [ "A", "B", "C" ]) (Doc.read restored |> Result.map texts)
                        ]
                        ()
            ]
        , describe "restoreTo — collaborative (the point vs. a local rewind)"
            [ test "a peer merging the restoring replica converges to the restored value" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        -- alice keeps editing past the snapshot
                        alice1 =
                            Doc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        alice2 =
                            Doc.listAppend todosPath (todo "D") alice1 |> ok alice1

                        -- bob has alice's full history (simulate by sharing the doc)
                        bob =
                            alice2

                        -- alice restores to the snapshot, then bob merges her in
                        aliceRestored =
                            Doc.restoreTo v alice2

                        bobMerged =
                            Doc.merge bob aliceRestored
                    in
                    Expect.equal (Ok ( "Trip", [ "A", "B", "C" ] ))
                        (Doc.read bobMerged |> Result.map (\b -> ( b.title, texts b )))
            , test "restore then merge is order-independent (converges both ways)" <|
                \_ ->
                    let
                        v =
                            Doc.version abcDoc

                        edited =
                            Doc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        restored =
                            Doc.restoreTo v edited

                        ab =
                            Doc.merge edited restored

                        ba =
                            Doc.merge restored edited
                    in
                    Expect.equal
                        (Doc.read ab |> Result.map (\b -> ( b.title, texts b )))
                        (Doc.read ba |> Result.map (\b -> ( b.title, texts b )))
            ]
        , describe "fork / branch"
            [ test "editing a branch does not touch the original document" <|
                \_ ->
                    let
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "X") d |> ok d)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "A", "B", "C" ]) (Doc.read abcDoc |> Result.map texts)
                        , \_ -> Expect.equal (Ok [ "A", "B", "C", "X" ]) (Doc.read branch |> Result.map texts)
                        ]
                        ()
            , test "a branch merges back onto the mainline" <|
                \_ ->
                    let
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "X") d |> ok d)

                        merged =
                            Doc.merge abcDoc branch
                    in
                    Doc.read merged
                        |> Result.map texts
                        |> Expect.equal (Ok [ "A", "B", "C", "X" ])
            , test "concurrent edits on branch and mainline BOTH survive (distinct replica — no id collision)" <|
                \_ ->
                    let
                        -- fork, then edit BOTH sides after the fork point. Under a shared
                        -- replica id these appends would mint the same OpId and one would
                        -- be lost; the branch's fresh replica keeps them concurrent.
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "fromBranch") d |> ok d)

                        mainline =
                            Doc.listAppend todosPath (todo "fromMain") abcDoc |> ok abcDoc

                        merged =
                            Doc.merge mainline branch
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok True) (Doc.read merged |> Result.map (texts >> List.member "fromBranch"))
                        , \_ -> Expect.equal (Ok True) (Doc.read merged |> Result.map (texts >> List.member "fromMain"))
                        , \_ -> Expect.equal (Ok 5) (Doc.read merged |> Result.map (.todos >> List.length))
                        ]
                        ()
            , test "merge-back converges regardless of order" <|
                \_ ->
                    let
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.setText titlePath "Branched" d |> ok d)

                        mainline =
                            Doc.listAppend todosPath (todo "M") abcDoc |> ok abcDoc

                        ab =
                            Doc.merge mainline branch

                        ba =
                            Doc.merge branch mainline
                    in
                    Expect.equal
                        (Doc.read ab |> Result.map (\b -> ( b.title, texts b )))
                        (Doc.read ba |> Result.map (\b -> ( b.title, texts b )))
            , test "forkAt a past version drops ops after the fork point, and the original is untouched" <|
                \_ ->
                    let
                        -- fork point is after A,B,C; mainline then adds D
                        v =
                            Doc.version abcDoc

                        mainline =
                            Doc.listAppend todosPath (todo "D") abcDoc |> ok abcDoc

                        -- branch from the pre-D version off the FULL (post-D) doc
                        branch =
                            Doc.forkAt (Id.replica "branch") v mainline
                                |> (\d -> Doc.listAppend todosPath (todo "Y") d |> ok d)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "A", "B", "C", "D" ]) (Doc.read mainline |> Result.map texts)
                        , \_ -> Expect.equal (Ok [ "A", "B", "C", "Y" ]) (Doc.read branch |> Result.map texts)
                        ]
                        ()
            , test "a branch and its unedited origin have zero divergence" <|
                \_ ->
                    let
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                    in
                    Doc.divergence { branch = branch, mainline = abcDoc }
                        |> Expect.equal { ahead = 0, behind = 0 }
            , test "divergence counts ops each side holds that the other lacks" <|
                \_ ->
                    let
                        -- branch gets one new op (a title char would be several — use a bool)
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "X") d |> ok d)

                        -- mainline gets two: append + a counter increment
                        mainline =
                            abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "P") d |> ok d)
                                |> (\d -> Doc.increment votesPath 1 d |> ok d)

                        div =
                            Doc.divergence { branch = branch, mainline = mainline }
                    in
                    Expect.all
                        [ \_ -> Expect.equal 1 div.ahead
                        , \_ -> Expect.equal 2 div.behind
                        ]
                        ()
            , test "after merging the branch back, the branch is no longer ahead" <|
                \_ ->
                    let
                        branch =
                            Doc.fork (Id.replica "branch") abcDoc
                                |> (\d -> Doc.listAppend todosPath (todo "X") d |> ok d)

                        merged =
                            Doc.merge abcDoc branch
                    in
                    (Doc.divergence { branch = branch, mainline = merged }).ahead
                        |> Expect.equal 0
            ]
        , describe "restoreTo — identity preservation"
            [ test "a cursor on an unchanged todo still resolves after a restore" <|
                \_ ->
                    let
                        -- cursor at end of the first todo's text ("A" => offset 1)
                        cur =
                            Doc.cursorAt (Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "text") 1 abcDoc
                                |> Result.toMaybe

                        v =
                            Doc.version abcDoc

                        -- change the title (todo 0 untouched), then restore
                        d1 =
                            Doc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        restored =
                            Doc.restoreTo v d1
                    in
                    case cur of
                        Just c ->
                            Doc.cursorOffset c restored |> Expect.equal (Just 1)

                        Nothing ->
                            Expect.fail "expected a cursor"
            ]
        ]
