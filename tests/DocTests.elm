module DocTests exposing (suite)

{-| Phase 1 tail: the op-log driven through its **public** surface (`Crdt.Doc`)
— init, path-addressed edits that emit ops, op-store `merge`, and `read`.

This is the op-model counterpart of the state-based `ConvergenceTests`: the same
real-world properties (concurrent edits converge regardless of merge order, text
merges character-wise, LWW resolves consistently), but every edit goes through
the index-addressed public API and is resolved to position-independent ops.

-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Dict
import Expect
import Test exposing (Test, describe, test)



-- FIXTURE (mirrors the demo's board) -----------------------------------------


type alias Board =
    { title : String
    , todos : List Todo
    , notes : Dict.Dict String String
    , revision : Int
    , owner : String
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
        |> S.field "todos" .todos (S.list todoSchema)
        |> S.field "notes" .notes (S.dict S.text)
        |> S.field "revision" .revision S.int
        |> S.field "owner" .owner S.string
        |> S.field "votes" .votes S.counter
        |> S.build


votesPath : Path
votesPath =
    Path.root |> Path.field "votes"


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


initDoc : String -> Doc Board
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Board -> Result Doc.Error (Doc Board) -> Doc Board
ok fallback =
    Result.withDefault fallback


read : Doc Board -> Result S.Error Board
read =
    Doc.read


suite : Test
suite =
    describe "Doc — op-log through the public API"
        [ test "fresh doc reads as the empty typed value" <|
            \_ ->
                read (initDoc "alice")
                    |> Expect.equal (Ok (Board "" [] Dict.empty 0 "" 0))
        , test "setInt and setString on register fields read back" <|
            \_ ->
                let
                    a =
                        initDoc "alice"

                    a1 =
                        Doc.setInt (Path.root |> Path.field "revision") 7 a |> ok a

                    a2 =
                        Doc.setString (Path.root |> Path.field "owner") "alice" a1 |> ok a1
                in
                read a2
                    |> Result.map (\b -> ( b.revision, b.owner ))
                    |> Expect.equal (Ok ( 7, "alice" ))
        , test "setText then read round-trips" <|
            \_ ->
                let
                    a =
                        initDoc "alice"

                    a1 =
                        Doc.setText titlePath "Trip" a |> ok a
                in
                read a1 |> Result.map .title |> Expect.equal (Ok "Trip")
        , test "incremental text edits accumulate (insert into the middle)" <|
            \_ ->
                let
                    a =
                        initDoc "alice"

                    a1 =
                        Doc.setText titlePath "Tip" a |> ok a

                    a2 =
                        Doc.setText titlePath "Trip" a1 |> ok a1
                in
                read a2 |> Result.map .title |> Expect.equal (Ok "Trip")
        , test "concurrent title edits from two replicas converge (same both merge orders)" <|
            \_ ->
                let
                    a =
                        Doc.setText titlePath "Trip plan" (initDoc "alice") |> ok (initDoc "alice")

                    b =
                        Doc.setText titlePath "Trip" (initDoc "bob") |> ok (initDoc "bob")
                in
                Expect.equal
                    (read (Doc.merge a b))
                    (read (Doc.merge b a))
        , test "concurrent list appends: both todos survive" <|
            \_ ->
                let
                    a =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo "pack" False)) (initDoc "alice")
                            |> ok (initDoc "alice")

                    b =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo "tickets" False)) (initDoc "bob")
                            |> ok (initDoc "bob")
                in
                read (Doc.merge a b)
                    |> Result.map (.todos >> List.length)
                    |> Expect.equal (Ok 2)
        , test "list append is commutative on the read value" <|
            \_ ->
                let
                    a =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo "a" False)) (initDoc "alice")
                            |> ok (initDoc "alice")

                    b =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo "b" True)) (initDoc "bob")
                            |> ok (initDoc "bob")
                in
                Expect.equal (read (Doc.merge a b)) (read (Doc.merge b a))
        , test "append fast-path: a run of appends reads back in append order" <|
            \_ ->
                let
                    labels =
                        [ "one", "two", "three", "four", "five" ]

                    doc =
                        List.foldl
                            (\label d ->
                                Doc.listAppend todosPath (todoSchema |> S.with (Todo label False)) d |> ok d
                            )
                            (initDoc "alice")
                            labels
                in
                read doc
                    |> Result.map (.todos >> List.map .text)
                    |> Expect.equal (Ok labels)
        , test "append order is preserved even when other edits interleave (cache invalidation)" <|
            \_ ->
                let
                    titleEdit d =
                        Doc.setText titlePath "x" d |> ok d

                    appendTodo label d =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo label False)) d |> ok d

                    -- interleave a non-append edit between appends, which clears
                    -- the fast-path; order must still be correct
                    doc =
                        initDoc "alice"
                            |> appendTodo "a"
                            |> titleEdit
                            |> appendTodo "b"
                            |> appendTodo "c"
                            |> titleEdit
                            |> appendTodo "d"
                in
                read doc
                    |> Result.map (.todos >> List.map .text)
                    |> Expect.equal (Ok [ "a", "b", "c", "d" ])
        , test "append after a merge lands at the end (fast-path cleared on merge)" <|
            \_ ->
                let
                    appendTodo label d =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo label False)) d |> ok d

                    a =
                        initDoc "alice" |> appendTodo "a1" |> appendTodo "a2"

                    b =
                        initDoc "bob" |> appendTodo "b1"

                    -- after merging in bob's op, alice appends again; the new
                    -- item must still read at the tail and both orders converge
                    merged =
                        Doc.merge a b |> appendTodo "a3"
                in
                Expect.all
                    [ \_ ->
                        read merged
                            |> Result.map (.todos >> List.length)
                            |> Expect.equal (Ok 4)
                    , \_ ->
                        -- a3 is the last thing alice appended; it must be last
                        read merged
                            |> Result.map (.todos >> List.reverse >> List.head >> Maybe.map .text)
                            |> Expect.equal (Ok (Just "a3"))
                    ]
                    ()
        , test "set then toggle a todo's done flag (LWW) reads the latest" <|
            \_ ->
                let
                    a =
                        Doc.listAppend todosPath (todoSchema |> S.with (Todo "x" False)) (initDoc "alice")
                            |> ok (initDoc "alice")

                    donePath =
                        Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

                    a1 =
                        Doc.setBool donePath True a |> ok a
                in
                read a1
                    |> Result.map (.todos >> List.map .done)
                    |> Expect.equal (Ok [ True ])
        , test "list remove tombstones the element" <|
            \_ ->
                let
                    a =
                        initDoc "alice"
                            |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "x" False)) d |> ok d)

                    a1 =
                        Doc.listRemove todosPath 0 a |> ok a
                in
                read a1 |> Result.map (.todos >> List.length) |> Expect.equal (Ok 0)
        , test "dict setKey / removeKey round-trip with LWW presence" <|
            \_ ->
                let
                    a =
                        initDoc "alice"

                    withKey =
                        Doc.setKey notesPath "g" (S.text |> S.with "milk") a |> ok a

                    removed =
                        Doc.removeKey notesPath "g" withKey |> ok withKey
                in
                Expect.all
                    [ \_ -> read withKey |> Result.map (.notes >> Dict.get "g") |> Expect.equal (Ok (Just "milk"))
                    , \_ -> read removed |> Result.map (.notes >> Dict.member "g") |> Expect.equal (Ok False)
                    ]
                    ()
        , test "opCount reflects the emitted ops" <|
            \_ ->
                let
                    a =
                        Doc.setText titlePath "Hi" (initDoc "alice") |> ok (initDoc "alice")
                in
                -- "Hi" => two character-insert ops
                Doc.opCount a |> Expect.equal 2
        , test "edit on a non-existent field reports PathNotFound" <|
            \_ ->
                let
                    badPath =
                        Path.root |> Path.field "nope"
                in
                case Doc.setText badPath "x" (initDoc "alice") of
                    Err (Doc.PathNotFound msg) ->
                        Expect.equal True (String.contains "nope" msg)

                    _ ->
                        Expect.fail "expected PathNotFound"
        , test "setText on a non-text field reports WrongNodeType" <|
            \_ ->
                let
                    -- 'todos' is a list, not text
                    listAsText =
                        Path.root |> Path.field "todos"
                in
                case Doc.setText listAsText "x" (initDoc "alice") of
                    Err (Doc.WrongNodeType msg) ->
                        Expect.equal True (String.contains "text" msg)

                    _ ->
                        Expect.fail "expected WrongNodeType"
        , test "cache invariant: cache equals a full re-fold after a chain of local edits" <|
            \_ ->
                let
                    donePath =
                        Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

                    doc =
                        initDoc "alice"
                            |> (\d -> Doc.setText titlePath "Trip" d |> ok d)
                            |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "pack" False)) d |> ok d)
                            |> (\d -> Doc.setText titlePath "Trip plan" d |> ok d)
                            |> (\d -> Doc.setBool donePath True d |> ok d)
                            |> (\d -> Doc.setKey notesPath "n" (S.text |> S.with "hi") d |> ok d)
                in
                Doc.cacheConsistent doc |> Expect.equal True
        , test "cache invariant: cache equals a full re-fold after a merge" <|
            \_ ->
                let
                    a =
                        initDoc "alice"
                            |> (\d -> Doc.setText titlePath "Trip" d |> ok d)
                            |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "pack" False)) d |> ok d)

                    b =
                        initDoc "bob"
                            |> (\d -> Doc.setText titlePath "Tour" d |> ok d)
                            |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "map" True)) d |> ok d)

                    merged =
                        Doc.merge a b
                in
                Doc.cacheConsistent merged |> Expect.equal True
        , test "cache invariant holds across edit-then-merge-then-edit" <|
            \_ ->
                let
                    a =
                        initDoc "alice" |> (\d -> Doc.setText titlePath "A" d |> ok d)

                    b =
                        initDoc "bob" |> (\d -> Doc.setText titlePath "B" d |> ok d)

                    doc =
                        Doc.merge a b
                            |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "after-merge" False)) d |> ok d)
                in
                Doc.cacheConsistent doc |> Expect.equal True
        , test "concurrent edits to different fields both land after merge" <|
            \_ ->
                let
                    -- alice edits the title; bob adds a note — fully concurrent
                    a =
                        Doc.setText titlePath "Plan" (initDoc "alice") |> ok (initDoc "alice")

                    b =
                        Doc.setKey notesPath "n" (S.text |> S.with "hi") (initDoc "bob") |> ok (initDoc "bob")

                    merged =
                        Doc.merge a b
                in
                Expect.all
                    [ \_ -> read merged |> Result.map .title |> Expect.equal (Ok "Plan")
                    , \_ -> read merged |> Result.map (.notes >> Dict.get "n") |> Expect.equal (Ok (Just "hi"))
                    ]
                    ()
        , describe "wire format (encode / decodeInto)"
            [ test "ops round-trip through JSON and converge on a fresh replica" <|
                \_ ->
                    let
                        alice =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "Trip" d |> ok d)
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "pack" False)) d |> ok d)
                                |> (\d -> Doc.setKey notesPath "n" (S.text |> S.with "hi") d |> ok d)

                        -- ship alice's ops to a fresh bob over JSON
                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode alice)
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.equal (read alice) (read bob)
            , test "decodeInto is idempotent (re-applying the same ops changes nothing)" <|
                \_ ->
                    let
                        alice =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "x" d |> ok d)

                        json =
                            Doc.encode alice

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto json
                                |> Result.andThen (Doc.decodeInto json)
                                |> Result.withDefault (initDoc "bob")
                    in
                    Expect.equal (read alice) (read bob)
            , test "concurrent edits exchanged both ways converge" <|
                \_ ->
                    let
                        a =
                            initDoc "alice" |> (\d -> Doc.setText titlePath "A" d |> ok d)

                        b =
                            initDoc "bob" |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "b" False)) d |> ok d)

                        aGetsB =
                            a |> Doc.decodeInto (Doc.encode b) |> Result.withDefault a

                        bGetsA =
                            b |> Doc.decodeInto (Doc.encode a) |> Result.withDefault b
                    in
                    Expect.equal (read aGetsB) (read bGetsA)
            , test "B can edit a register inside a todo A created (clock advances past seed stamps)" <|
                \_ ->
                    let
                        -- A appends a todo. Its `done`/`text` registers are stamped
                        -- with counters *higher* than the insert op's own id (the
                        -- seed is built after the elemId is minted).
                        a =
                            initDoc "alice"
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "from-alice" False)) d |> ok d)

                        -- B receives the op and immediately toggles `done`. B's new
                        -- stamp must beat the seeded `done` stamp, or the toggle is
                        -- silently dropped (the reported bug).
                        donePath =
                            Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

                        b =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode a)
                                |> Result.withDefault (initDoc "bob")
                                |> (\d -> Doc.setBool donePath True d |> ok d)
                    in
                    read b
                        |> Result.map (.todos >> List.map .done)
                        |> Expect.equal (Ok [ True ])
            , test "same, via in-memory Doc.merge (clock = max of ctx counters, not a tree scan)" <|
                \_ ->
                    let
                        -- A appends a todo; its seed registers carry counters higher
                        -- than the insert op's own id.
                        a =
                            initDoc "alice"
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "from-alice" False)) d |> ok d)

                        donePath =
                            Path.root |> Path.field "todos" |> Path.index 0 |> Path.field "done"

                        -- B merges A *in memory* (not decode), then toggles `done`. B's
                        -- clock must have advanced past A's seed stamps or the toggle
                        -- loses LWW. This exercises the O(1) `max(ctx)` merge clock rule
                        -- (which must be >= every stamp, including buried seeds).
                        b =
                            Doc.merge (initDoc "bob") a
                                |> (\d -> Doc.setBool donePath True d |> ok d)
                    in
                    read b
                        |> Result.map (.todos >> List.map .done)
                        |> Expect.equal (Ok [ True ])
            ]
        , describe "counter (PN-counter)"
            [ test "increments accumulate locally" <|
                \_ ->
                    let
                        doc =
                            initDoc "alice"
                                |> (\d -> Doc.increment votesPath 1 d |> ok d)
                                |> (\d -> Doc.increment votesPath 1 d |> ok d)
                                |> (\d -> Doc.increment votesPath 3 d |> ok d)
                    in
                    read doc |> Result.map .votes |> Expect.equal (Ok 5)
            , test "decrement with a negative delta" <|
                \_ ->
                    let
                        doc =
                            initDoc "alice"
                                |> (\d -> Doc.increment votesPath 10 d |> ok d)
                                |> (\d -> Doc.increment votesPath -4 d |> ok d)
                    in
                    read doc |> Result.map .votes |> Expect.equal (Ok 6)
            , test "concurrent increments SUM (the whole point — not LWW)" <|
                \_ ->
                    let
                        -- both start empty, each increments once, concurrently
                        a =
                            initDoc "alice" |> (\d -> Doc.increment votesPath 1 d |> ok d)

                        b =
                            initDoc "bob" |> (\d -> Doc.increment votesPath 1 d |> ok d)

                        merged =
                            Doc.merge a b
                    in
                    -- an LWW register would read 1 here; a counter reads 2
                    read merged |> Result.map .votes |> Expect.equal (Ok 2)
            , test "concurrent increments converge regardless of merge order" <|
                \_ ->
                    let
                        a =
                            initDoc "alice" |> (\d -> Doc.increment votesPath 5 d |> ok d)

                        b =
                            initDoc "bob" |> (\d -> Doc.increment votesPath 7 d |> ok d)
                    in
                    Expect.equal
                        (read (Doc.merge a b))
                        (read (Doc.merge b a))
            , test "counter increments survive the JSON wire round-trip and sum" <|
                \_ ->
                    let
                        a =
                            initDoc "alice" |> (\d -> Doc.increment votesPath 2 d |> ok d)

                        b =
                            initDoc "bob"
                                |> (\d -> Doc.increment votesPath 3 d |> ok d)
                                |> Doc.decodeInto (Doc.encode a)
                                |> Result.withDefault (initDoc "bob")
                    in
                    read b |> Result.map .votes |> Expect.equal (Ok 5)
            ]
        , describe "delta sync (encodeSince)"
            [ test "a delta brings a behind peer up to the sender's read value" <|
                \_ ->
                    let
                        -- bob is at some shared point, captures his version
                        shared =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "shared" d |> ok d)

                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode shared)
                                |> Result.withDefault (initDoc "bob")

                        bobVersion =
                            Doc.version bob

                        -- alice (continuing from shared) makes more edits
                        alice2 =
                            shared
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "x" False)) d |> ok d)
                                |> (\d -> Doc.setText titlePath "shared!" d |> ok d)

                        -- ship ONLY the delta bob is missing
                        bob2 =
                            bob
                                |> Doc.decodeInto (Doc.encodeSince bobVersion alice2)
                                |> Result.withDefault bob
                    in
                    Expect.equal (read alice2) (read bob2)
            , test "delta ≡ full state: same read whether you send opsSince or every op" <|
                \_ ->
                    let
                        bob0 =
                            initDoc "bob"

                        bobVersion =
                            Doc.version bob0

                        alice =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "T" d |> ok d)
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "t" True)) d |> ok d)

                        viaDelta =
                            bob0 |> Doc.decodeInto (Doc.encodeSince bobVersion alice) |> Result.withDefault bob0

                        viaFull =
                            bob0 |> Doc.decodeInto (Doc.encode alice) |> Result.withDefault bob0
                    in
                    Expect.equal (read viaDelta) (read viaFull)
            , test "delta to a fully-caught-up peer is empty (nothing to send)" <|
                \_ ->
                    let
                        alice =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "done" d |> ok d)

                        -- bob has everything alice has
                        bob =
                            initDoc "bob"
                                |> Doc.decodeInto (Doc.encode alice)
                                |> Result.withDefault (initDoc "bob")

                        -- a delta since alice's own current version carries no ops
                        deltaJson =
                            Doc.encodeSince (Doc.version alice) alice

                        -- decoding an empty delta changes nothing
                        bob2 =
                            bob |> Doc.decodeInto deltaJson |> Result.withDefault bob
                    in
                    Expect.equal (read bob) (read bob2)
            ]
        , describe "named checkpoints"
            [ test "a checkpoint records label + author and time-travels to its version" <|
                \_ ->
                    let
                        doc =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "v1" d |> ok d)
                                |> Doc.checkpoint "first draft"
                                |> (\d -> Doc.setText titlePath "v2" d |> ok d)

                        saved =
                            Doc.checkpoints doc |> List.head
                    in
                    case saved of
                        Just cp ->
                            Expect.all
                                [ \_ -> Doc.checkpointMessage cp |> Expect.equal "first draft"
                                , \_ -> Doc.checkpointAuthor cp |> Expect.equal (Id.replica "alice")
                                , -- reading at the checkpoint shows the state when it was saved
                                  \_ ->
                                    Doc.readAt (Doc.checkpointVersion cp) doc
                                        |> Result.map .title
                                        |> Expect.equal (Ok "v1")
                                , -- the live doc has moved on
                                  \_ -> read doc |> Result.map .title |> Expect.equal (Ok "v2")
                                ]
                                ()

                        Nothing ->
                            Expect.fail "expected a checkpoint"
            , test "checkpoints accumulate most-recent-first and don't emit ops" <|
                \_ ->
                    let
                        before =
                            initDoc "alice" |> (\d -> Doc.setText titlePath "x" d |> ok d)

                        after =
                            before
                                |> Doc.checkpoint "one"
                                |> Doc.checkpoint "two"
                    in
                    Expect.all
                        [ \_ ->
                            Doc.checkpoints after
                                |> List.map Doc.checkpointMessage
                                |> Expect.equal [ "two", "one" ]
                        , -- checkpointing emits no ops, so the op count is unchanged
                          \_ ->
                            Doc.opCount after
                                |> Expect.equal (Doc.opCount before)
                        ]
                        ()
            ]
        , describe "collaborative history / time-travel"
            [ test "readAt a captured version returns past state; live doc is unchanged" <|
                \_ ->
                    let
                        v1 =
                            initDoc "alice" |> (\d -> Doc.setText titlePath "draft" d |> ok d)

                        atDraft =
                            Doc.version v1

                        v2 =
                            Doc.setText titlePath "final" v1 |> ok v1
                    in
                    Expect.all
                        [ -- time-travel to the captured version shows the old value
                          \_ -> Doc.readAt atDraft v2 |> Result.map .title |> Expect.equal (Ok "draft")
                        , -- the live document still has the latest
                          \_ -> read v2 |> Result.map .title |> Expect.equal (Ok "final")
                        ]
                        ()
            , test "a version captured before later edits excludes those edits" <|
                \_ ->
                    let
                        base =
                            initDoc "alice"
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "first" False)) d |> ok d)

                        v =
                            Doc.version base

                        later =
                            base
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "second" False)) d |> ok d)
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "third" False)) d |> ok d)
                    in
                    Expect.all
                        [ \_ -> Doc.readAt v later |> Result.map (.todos >> List.length) |> Expect.equal (Ok 1)
                        , \_ -> read later |> Result.map (.todos >> List.length) |> Expect.equal (Ok 3)
                        ]
                        ()
            , test "collaborative: a version is meaningful after merging a peer's concurrent ops" <|
                \_ ->
                    let
                        -- alice reaches a version, captures it, then a peer's work
                        -- arrives by merge and alice keeps editing
                        alice0 =
                            initDoc "alice"
                                |> (\d -> Doc.setText titlePath "shared" d |> ok d)

                        vShared =
                            Doc.version alice0

                        bob =
                            initDoc "bob"
                                |> (\d -> Doc.listAppend todosPath (todoSchema |> S.with (Todo "bob-todo" False)) d |> ok d)

                        merged =
                            Doc.merge alice0 bob
                                |> (\d -> Doc.setText titlePath "shared+" d |> ok d)
                    in
                    Expect.all
                        [ -- checkout of the pre-merge version sees neither bob's
                          -- concurrent todo nor alice's later title edit
                          \_ -> Doc.readAt vShared merged |> Result.map .title |> Expect.equal (Ok "shared")
                        , \_ -> Doc.readAt vShared merged |> Result.map (.todos >> List.length) |> Expect.equal (Ok 0)
                        , -- the live document has everything
                          \_ -> read merged |> Result.map .title |> Expect.equal (Ok "shared+")
                        , \_ -> read merged |> Result.map (.todos >> List.length) |> Expect.equal (Ok 1)
                        ]
                        ()
            ]
        ]
