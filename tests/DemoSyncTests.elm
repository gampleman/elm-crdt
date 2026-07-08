module DemoSyncTests exposing (suite)

{-| Replicates the DEMO's exact schema (sum-type status + movable list of records)
and its sync path (Ref-based edits → `encodeSince` delta → `decodeInto` on a peer)
to check the two windows actually converge. The generic wire tests use a simpler
schema and the Path edit API; this pins the real thing.
-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Schema as S
import Expect
import Test exposing (Test, describe, test)


type Status
    = Planning
    | Active
    | Archived String


type alias Todo =
    { text : String, done : Bool }


type alias Board =
    { title : String, status : Status, todos : List Todo }


type alias StatusRefs =
    { archived : Ref Status S.Settable String }


statusDoc : Ref.CustomRefs Status StatusRefs
statusDoc =
    Ref.custom
        (\planning active archived v ->
            case v of
                Planning ->
                    planning

                Active ->
                    active

                Archived reason ->
                    archived reason
        )
        StatusRefs
        |> Ref.variant0 "planning" Planning
        |> Ref.variant0 "active" Active
        |> Ref.variant1 "archived" Archived S.text
        |> Ref.buildCustom


type alias TodoRefs =
    { text : Ref Todo S.Settable String, done : Ref Todo S.Settable Bool }


todoDoc : Ref.RecordRefs Todo TodoRefs
todoDoc =
    Ref.record Todo TodoRefs
        |> Ref.field "text" .text S.text
        |> Ref.field "done" .done S.bool
        |> Ref.build


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , status : Ref Board (S.Variants Status) Status
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "status" .status statusDoc.schema
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.build


refs : BoardRefs
refs =
    boardDoc.refs


init : String -> OpDoc Board
init name =
    OpDoc.init (Id.replica name) boardDoc.schema


ok : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
ok fb =
    Result.withDefault fb


{-| The demo's steady-state send: `encodeSince lastSent`, applied to the peer.
-}
deltaSync : Version -> OpDoc Board -> OpDoc Board -> OpDoc Board
deltaSync since from to =
    OpDoc.decodeInto (OpDoc.encodeSince since from) to |> Result.withDefault to


type alias Version =
    OpDoc.Version


suite : Test
suite =
    describe "demo schema sync (Ref edits + delta wire)"
        [ test "full-state exchange converges (the connect handshake)" <|
            \_ ->
                let
                    alice =
                        init "alice"
                            |> (\d -> Ref.set refs.title "Trip" d |> ok d)
                            |> (\d -> Ref.append todoDoc.schema (Todo "pack" False) refs.todos d |> ok d)

                    bob =
                        OpDoc.decodeInto (OpDoc.encode alice) (init "bob") |> Result.withDefault (init "bob")
                in
                Expect.equal (OpDoc.read alice) (OpDoc.read bob)
        , test "a title edit delta reaches a peer" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    alice =
                        Ref.set refs.title "Hello" (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob |> Result.map .title |> Expect.equal (Ok "Hello")
        , test "a status variant switch delta reaches a peer" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    alice =
                        Ref.switch refs.status (Archived "old") (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob |> Result.map .status |> Expect.equal (Ok (Archived "old"))
        , test "an appended todo delta reaches a peer" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    alice =
                        Ref.append todoDoc.schema (Todo "buy milk" False) refs.todos (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "buy milk" ])
        ]
