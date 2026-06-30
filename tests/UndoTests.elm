module UndoTests exposing (suite)

{-| Loro-style **local** undo/redo (`OpDoc.recordEdit` / `undo` / `redo`).

This is per-replica undo, not global time-travel: `undo` inverts _this_ replica's
own ops as fresh ops, so

  - undo/redo **sync** (they emit ops, they're not a local rewind);
  - a peer's **concurrent** edit to another field is **preserved** by my undo —
    the property that separates local undo from the global scrubber/`restoreTo`;
  - a same-field conflict resolves by LWW, with the undo (a newer op) winning;
  - the usual editor invariants hold: redo replays, a fresh edit clears redo, undo
    past the bottom is a no-op.

Undo of a delete cannot truly resurrect the tombstoned element (deletes are
permanent), so it re-creates the content with a fresh id — value/position restored,
identity not. That is asserted by value.

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema as S exposing (Crdt)
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


schema : Crdt Board
schema =
    S.record Board
        |> S.field "title" .title S.text
        |> S.field "todos" .todos (S.movableList todoSchema)
        |> S.field "notes" .notes (S.dict S.text)
        |> S.field "votes" .votes S.counter
        |> S.build


todoSchema : Crdt Todo
todoSchema =
    S.record Todo
        |> S.field "text" .text S.text
        |> S.field "done" .done S.bool
        |> S.build


todosPath : Path
todosPath =
    Path.root |> Path.field "todos"


notesPath : Path
notesPath =
    Path.root |> Path.field "notes"


votesPath : Path
votesPath =
    Path.root |> Path.field "votes"


todoDonePath : Int -> Path
todoDonePath i =
    Path.root |> Path.field "todos" |> Path.index i |> Path.field "done"


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


{-| Make one tracked edit: capture the version, apply `f`, record it for undo.
-}
edit : (OpDoc Board -> OpDoc Board) -> OpDoc Board -> OpDoc Board
edit f doc =
    let
        before =
            OpDoc.version doc
    in
    OpDoc.recordEdit before (f doc)


appendTodo : String -> OpDoc Board -> OpDoc Board
appendTodo t doc =
    edit (\d -> OpDoc.listAppend todosPath (todo t) d |> ok d) doc


readTexts : OpDoc Board -> Result S.Error (List String)
readTexts =
    OpDoc.read >> Result.map texts


suite : Test
suite =
    describe "OpDoc local undo/redo"
        [ describe "basics"
            [ test "undo reverts the last edit" <|
                \_ ->
                    initDoc "alice"
                        |> appendTodo "A"
                        |> appendTodo "B"
                        |> OpDoc.undo
                        |> readTexts
                        |> Expect.equal (Ok [ "A" ])
            , test "undo then redo restores it" <|
                \_ ->
                    initDoc "alice"
                        |> appendTodo "A"
                        |> appendTodo "B"
                        |> OpDoc.undo
                        |> OpDoc.redo
                        |> readTexts
                        |> Expect.equal (Ok [ "A", "B" ])
            , test "multiple undos peel edits in reverse order" <|
                \_ ->
                    initDoc "alice"
                        |> appendTodo "A"
                        |> appendTodo "B"
                        |> appendTodo "C"
                        |> OpDoc.undo
                        |> OpDoc.undo
                        |> readTexts
                        |> Expect.equal (Ok [ "A" ])
            , test "canUndo / canRedo track stack state" <|
                \_ ->
                    let
                        d0 =
                            initDoc "alice"

                        d1 =
                            appendTodo "A" d0

                        d2 =
                            OpDoc.undo d1
                    in
                    Expect.equal
                        ( OpDoc.canUndo d0, OpDoc.canUndo d1, OpDoc.canRedo d2 )
                        ( False, True, True )
            , test "a fresh edit clears the redo stack" <|
                \_ ->
                    let
                        d =
                            initDoc "alice"
                                |> appendTodo "A"
                                |> appendTodo "B"
                                |> OpDoc.undo
                                |> appendTodo "C"
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ "A", "C" ]) (readTexts d)
                        , \_ -> Expect.equal False (OpDoc.canRedo d)
                        ]
                        ()
            , test "undoing past the bottom of the stack is a no-op" <|
                \_ ->
                    let
                        d =
                            appendTodo "A" (initDoc "alice")
                    in
                    -- one real undo (-> []), then two extra undos do nothing
                    OpDoc.undo (OpDoc.undo (OpDoc.undo d))
                        |> readTexts
                        |> Expect.equal (Ok [])
            , test "recordEdit of a no-op change records nothing" <|
                \_ ->
                    let
                        d =
                            appendTodo "A" (initDoc "alice")

                        before =
                            OpDoc.version d

                        -- no edit between capture and record
                        d2 =
                            OpDoc.recordEdit before d
                    in
                    -- the only undoable thing is still the append
                    OpDoc.undo d2 |> readTexts |> Expect.equal (Ok [])
            ]
        , describe "across edit kinds"
            [ test "undo a register edit restores the prior value" <|
                \_ ->
                    let
                        d =
                            initDoc "alice"
                                |> appendTodo "A"
                                |> edit (\x -> OpDoc.setBool (todoDonePath 0) True x |> ok x)
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok [ True ]) (OpDoc.read d |> Result.map (.todos >> List.map .done))
                        , \_ -> Expect.equal (Ok [ False ]) (OpDoc.undo d |> OpDoc.read |> Result.map (.todos >> List.map .done))
                        ]
                        ()
            , test "undo a counter increment subtracts it back" <|
                \_ ->
                    initDoc "alice"
                        |> edit (\d -> OpDoc.increment votesPath 5 d |> ok d)
                        |> edit (\d -> OpDoc.increment votesPath 3 d |> ok d)
                        |> OpDoc.undo
                        |> OpDoc.read
                        |> Result.map .votes
                        |> Expect.equal (Ok 5)
            , test "undo a dict key add removes it; redo re-adds" <|
                \_ ->
                    let
                        d =
                            initDoc "alice"
                                |> edit (\x -> OpDoc.setKey notesPath "k" (S.text |> S.with "v") x |> ok x)

                        undone =
                            OpDoc.undo d
                    in
                    Expect.all
                        [ \_ -> Expect.equal (Ok Dict.empty) (OpDoc.read undone |> Result.map .notes)
                        , \_ -> Expect.equal (Ok (Dict.fromList [ ( "k", "v" ) ])) (OpDoc.redo undone |> OpDoc.read |> Result.map .notes)
                        ]
                        ()
            , test "undo a delete re-creates the item (fresh identity, same value/position)" <|
                \_ ->
                    initDoc "alice"
                        |> appendTodo "A"
                        |> appendTodo "B"
                        |> appendTodo "C"
                        |> edit (\d -> OpDoc.listRemove todosPath 1 d |> ok d)
                        |> OpDoc.undo
                        |> readTexts
                        |> Expect.equal (Ok [ "A", "B", "C" ])
            , test "undo a move puts the item back in place" <|
                \_ ->
                    initDoc "alice"
                        |> appendTodo "A"
                        |> appendTodo "B"
                        |> appendTodo "C"
                        |> edit (\d -> OpDoc.listMove todosPath 0 2 d |> ok d)
                        |> OpDoc.undo
                        |> readTexts
                        |> Expect.equal (Ok [ "A", "B", "C" ])
            , test "undo/redo cycle is stable across repetition" <|
                \_ ->
                    let
                        d =
                            initDoc "alice" |> appendTodo "A" |> appendTodo "B"

                        cycled =
                            d
                                |> OpDoc.undo
                                |> OpDoc.redo
                                |> OpDoc.undo
                                |> OpDoc.redo
                    in
                    Expect.equal (readTexts d) (readTexts cycled)
            ]
        , describe "collaborative: local undo preserves concurrent remote edits"
            [ test "undoing my insert keeps a peer's concurrent insert in the same list" <|
                \_ ->
                    let
                        -- shared starting point with one todo
                        start =
                            initDoc "alice" |> appendTodo "A"

                        -- alice and bob both branch from `start`
                        alice =
                            start

                        bob =
                            -- bob is a different replica that has alice's ops
                            OpDoc.merge (initDoc "bob") start

                        -- alice appends X (tracked); bob concurrently appends Y
                        aliceX =
                            appendTodo "X" alice

                        bobY =
                            OpDoc.listAppend todosPath (todo "Y") bob |> ok bob

                        -- they sync: alice merges bob's Y
                        aliceSynced =
                            OpDoc.merge aliceX bobY

                        -- alice undoes her own X — Y must remain
                        afterUndo =
                            OpDoc.undo aliceSynced

                        result =
                            OpDoc.read afterUndo |> Result.map (texts >> List.sort)
                    in
                    Expect.equal (Ok [ "A", "Y" ]) result
            , test "my undo syncs: a peer merging me also sees the undo" <|
                \_ ->
                    let
                        start =
                            initDoc "alice" |> appendTodo "A"

                        bob =
                            OpDoc.merge (initDoc "bob") start

                        aliceB =
                            appendTodo "B" start

                        -- bob has B too
                        bobWithB =
                            OpDoc.merge bob aliceB

                        -- alice undoes B, bob merges alice's undo ops
                        aliceUndone =
                            OpDoc.undo aliceB

                        bobMerged =
                            OpDoc.merge bobWithB aliceUndone
                    in
                    Expect.equal (Ok [ "A" ]) (readTexts bobMerged)
            , test "undo survives a merge in between (stacks preserved across merge)" <|
                \_ ->
                    let
                        start =
                            initDoc "alice" |> appendTodo "A"

                        bob =
                            OpDoc.merge (initDoc "bob") start

                        aliceB =
                            appendTodo "B" start

                        bobC =
                            OpDoc.listAppend todosPath (todo "C") bob |> ok bob

                        -- alice merges bob's C, THEN undoes her own B
                        merged =
                            OpDoc.merge aliceB bobC

                        afterUndo =
                            OpDoc.undo merged
                    in
                    Expect.equal (Ok [ "A", "C" ]) (OpDoc.read afterUndo |> Result.map (texts >> List.sort))
            ]
        ]
