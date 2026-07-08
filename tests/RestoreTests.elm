module RestoreTests exposing (suite)

{-| History scrubbing (`OpDoc.historyLength` / `versionAt`) and **collaborative
restore** (`OpDoc.restoreTo`).

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

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
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


initDoc : String -> OpDoc Board
initDoc name =
    OpDoc.init (Id.replica name) schema


ok : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
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
abcDoc : OpDoc Board
abcDoc =
    let
        a0 =
            initDoc "alice"

        a1 =
            OpDoc.setText titlePath "Trip" a0 |> ok a0

        a2 =
            OpDoc.listAppend todosPath (todo "A") a1 |> ok a1

        a3 =
            OpDoc.listAppend todosPath (todo "B") a2 |> ok a2
    in
    OpDoc.listAppend todosPath (todo "C") a3 |> ok a3


suite : Test
suite =
    describe "OpDoc history scrubber + restore"
        [ describe "scrubber (historyLength / versionAt)"
            [ test "historyLength counts every edit op" <|
                \_ ->
                    -- title diff = "Trip" is 4 char-insert ops; 3 list appends
                    OpDoc.historyLength abcDoc
                        |> Expect.equal (OpDoc.opCount abcDoc)
            , test "versionAt 0 is the empty document" <|
                \_ ->
                    OpDoc.readAt (OpDoc.versionAt 0 abcDoc) abcDoc
                        |> Expect.equal (Ok (Board "" [] Dict.empty 0))
            , test "versionAt historyLength is the current state" <|
                \_ ->
                    let
                        n =
                            OpDoc.historyLength abcDoc
                    in
                    OpDoc.readAt (OpDoc.versionAt n abcDoc) abcDoc
                        |> Result.map texts
                        |> Expect.equal (Ok [ "A", "B", "C" ])
            , test "scrubbing is monotonic: each later step has >= todos" <|
                \_ ->
                    let
                        n =
                            OpDoc.historyLength abcDoc

                        counts =
                            List.range 0 n
                                |> List.map
                                    (\i ->
                                        OpDoc.readAt (OpDoc.versionAt i abcDoc) abcDoc
                                            |> Result.map (.todos >> List.length)
                                            |> Result.withDefault -1
                                    )
                    in
                    Expect.equal counts (List.sort counts)
            , test "versionAt clamps out-of-range steps" <|
                \_ ->
                    Expect.equal
                        (OpDoc.readAt (OpDoc.versionAt 9999 abcDoc) abcDoc |> Result.map texts)
                        (Ok [ "A", "B", "C" ])
            ]
        , describe "restoreTo — local effect"
            [ test "restoring to an earlier version reads as that version" <|
                \_ ->
                    let
                        -- snapshot after A,B,C; then add D and edit the title
                        v =
                            OpDoc.version abcDoc

                        d1 =
                            OpDoc.listAppend todosPath (todo "D") abcDoc |> ok abcDoc

                        d2 =
                            OpDoc.setText titlePath "Changed" d1 |> ok d1

                        restored =
                            OpDoc.restoreTo v d2
                    in
                    OpDoc.read restored
                        |> Result.map (\b -> ( b.title, texts b ))
                        |> Expect.equal (Ok ( "Trip", [ "A", "B", "C" ] ))
            , test "restore is expressed as ops: the op count grows, history is not truncated" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        d1 =
                            OpDoc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        before =
                            OpDoc.opCount d1

                        restored =
                            OpDoc.restoreTo v d1
                    in
                    Expect.greaterThan before (OpDoc.opCount restored)
            , test "restoring to the current version is a no-op on the read" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        restored =
                            OpDoc.restoreTo v abcDoc
                    in
                    Expect.equal (OpDoc.read abcDoc |> Result.map texts)
                        (OpDoc.read restored |> Result.map texts)
            , test "restore re-creates a todo that was deleted since the version" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        d1 =
                            OpDoc.listRemove todosPath 1 abcDoc |> ok abcDoc

                        -- B is gone now
                        gone =
                            OpDoc.read d1 |> Result.map texts

                        restored =
                            OpDoc.restoreTo v d1
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "A", "C" ]) gone
                        , \_ -> Expect.equal (Ok [ "A", "B", "C" ]) (OpDoc.read restored |> Result.map texts)
                        ]
                        ()
            , test "restore reverts a counter via a compensating increment" <|
                \_ ->
                    let
                        base =
                            OpDoc.increment votesPath 3 (initDoc "alice") |> ok (initDoc "alice")

                        v =
                            OpDoc.version base

                        more =
                            OpDoc.increment votesPath 10 base |> ok base

                        restored =
                            OpDoc.restoreTo v more
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok 13) (OpDoc.read more |> Result.map .votes)
                        , \_ -> Expect.equal (Ok 3) (OpDoc.read restored |> Result.map .votes)
                        ]
                        ()
            , test "restore reverts dict add/remove" <|
                \_ ->
                    let
                        withNote =
                            OpDoc.setKey notesPath "k" (S.text |> S.with "hello") (initDoc "alice")
                                |> ok (initDoc "alice")

                        v =
                            OpDoc.version withNote

                        changed =
                            withNote
                                |> (\d -> OpDoc.removeKey notesPath "k" d |> ok d)
                                |> (\d -> OpDoc.setKey notesPath "j" (S.text |> S.with "new") d |> ok d)

                        restored =
                            OpDoc.restoreTo v changed
                    in
                    Expect.equal (Ok (Dict.fromList [ ( "k", "hello" ) ]))
                        (OpDoc.read restored |> Result.map .notes)
            , test "restore reverts a reordering on a movableList" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        moved =
                            OpDoc.listMove todosPath 0 2 abcDoc |> ok abcDoc

                        restored =
                            OpDoc.restoreTo v moved
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "B", "C", "A" ]) (OpDoc.read moved |> Result.map texts)
                        , \_ -> Expect.equal (Ok [ "A", "B", "C" ]) (OpDoc.read restored |> Result.map texts)
                        ]
                        ()
            ]
        , describe "restoreTo — collaborative (the point vs. a local rewind)"
            [ test "a peer merging the restoring replica converges to the restored value" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        -- alice keeps editing past the snapshot
                        alice1 =
                            OpDoc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        alice2 =
                            OpDoc.listAppend todosPath (todo "D") alice1 |> ok alice1

                        -- bob has alice's full history (simulate by sharing the doc)
                        bob =
                            alice2

                        -- alice restores to the snapshot, then bob merges her in
                        aliceRestored =
                            OpDoc.restoreTo v alice2

                        bobMerged =
                            OpDoc.merge bob aliceRestored
                    in
                    Expect.equal (Ok ( "Trip", [ "A", "B", "C" ] ))
                        (OpDoc.read bobMerged |> Result.map (\b -> ( b.title, texts b )))
            , test "restore then merge is order-independent (converges both ways)" <|
                \_ ->
                    let
                        v =
                            OpDoc.version abcDoc

                        edited =
                            OpDoc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        restored =
                            OpDoc.restoreTo v edited

                        ab =
                            OpDoc.merge edited restored

                        ba =
                            OpDoc.merge restored edited
                    in
                    Expect.equal
                        (OpDoc.read ab |> Result.map (\b -> ( b.title, texts b )))
                        (OpDoc.read ba |> Result.map (\b -> ( b.title, texts b )))
            ]
        , describe "restoreTo — identity preservation"
            [ test "a cursor on an unchanged todo still resolves after a restore" <|
                \_ ->
                    let
                        -- cursor at end of the first todo's text ("A" => offset 1)
                        cur =
                            OpDoc.cursorAt (Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "text") 1 abcDoc
                                |> Result.toMaybe

                        v =
                            OpDoc.version abcDoc

                        -- change the title (todo 0 untouched), then restore
                        d1 =
                            OpDoc.setText titlePath "Changed" abcDoc |> ok abcDoc

                        restored =
                            OpDoc.restoreTo v d1
                    in
                    case cur of
                        Just c ->
                            OpDoc.cursorOffset c restored |> Expect.equal (Just 1)

                        Nothing ->
                            Expect.fail "expected a cursor"
            ]
        ]
