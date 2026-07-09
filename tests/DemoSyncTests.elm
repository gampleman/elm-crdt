module DemoSyncTests exposing (suite)

{-| Replicates the DEMO's exact schema (sum-type status + movable list of records)
and its sync path (Ref-based edits → `encodeSince` delta → `decodeInto` on a peer)
to check the two windows actually converge. The generic wire tests use a simpler
schema and the Path edit API; this pins the real thing.
-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText as RichText exposing (MarkValue(..), Span)
import Crdt.Schema as S
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)


type Status
    = Planning
    | Active
    | Archived String


type alias Todo =
    { text : String, done : Bool }


type alias OutlineNode =
    { text : String }


type alias Board =
    { title : String
    , status : Status
    , todos : List Todo
    , outline : Tree.Forest OutlineNode
    , files : Dict String (List Span)
    , likes : Int
    }


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


type alias OutlineRefs =
    { text : Ref OutlineNode S.Settable String }


outlineDoc : Ref.RecordRefs OutlineNode OutlineRefs
outlineDoc =
    Ref.record OutlineNode OutlineRefs
        |> Ref.field "text" .text S.text
        |> Ref.build


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , status : Ref Board (S.Variants Status) Status
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    , outline : Ref Board (S.TreeK S.Nested OutlineNode) (Tree.Forest OutlineNode)
    , files : Ref Board (S.DictK S.RichK (List Span)) (Dict String (List Span))
    , likes : Ref Board S.Counter Int
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "status" .status statusDoc.schema
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.field "outline" .outline (S.tree outlineDoc.schema)
        |> Ref.field "files" .files (S.dict S.richText)
        |> Ref.field "likes" .likes S.counter
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
        , test "an added outline node delta reaches a peer" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    alice =
                        Ref.addChild outlineDoc.schema (OutlineNode "root") Nothing refs.outline (init "alice")
                            |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob
                    |> Result.map (.outline >> List.map (Tree.itemValue >> .text))
                    |> Expect.equal (Ok [ "root" ])
        , test "a file's rich-text edit + mark delta reaches a peer (the demo's editor path)" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    file =
                        refs.files |> Ref.key "notes.md" S.richText

                    -- create a file, type text, then bold a range — the demo's intents
                    alice =
                        init "alice"
                            |> (\d -> Ref.setKey S.richText "notes.md" [] refs.files d |> ok d)
                            |> (\d -> Ref.setRich file "hello" d |> ok d)
                            |> (\d -> Ref.mark file 0 3 "bold" Flag d |> ok d)

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob
                    |> Result.toMaybe
                    |> Maybe.andThen (\b -> Dict.get "notes.md" b.files)
                    |> Maybe.map (List.map (\s -> ( s.text, Dict.keys s.marks )))
                    |> Expect.equal (Just [ ( "hel", [ "bold" ] ), ( "lo", [] ) ])
        , test "a likes-counter increment delta reaches a peer" <|
            \_ ->
                let
                    before =
                        OpDoc.version (init "alice")

                    alice =
                        init "alice"
                            |> (\d -> Ref.increment refs.likes 1 d |> ok d)
                            |> (\d -> Ref.increment refs.likes 1 d |> ok d)

                    bob =
                        deltaSync before alice (init "bob")
                in
                OpDoc.read bob |> Result.map .likes |> Expect.equal (Ok 2)
        ]
