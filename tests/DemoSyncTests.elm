module DemoSyncTests exposing (suite)

{-| Replicates the DEMO's exact schema (sum-type status + movable list of records)
and its sync path (Ref-based edits → `encodeSince` delta → `decodeInto` on a peer)
to check the two windows actually converge. The generic wire tests use a simpler
schema and the Path edit API; this pins the real thing.
-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.RichText exposing (MarkValue(..), Span)
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
    { archived : Ref Status C.Settable String }


statusDoc : C.CustomRefs Status StatusRefs
statusDoc =
    C.custom
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
        |> C.variant0 "planning" Planning
        |> C.variant0 "active" Active
        |> C.variant1 "archived" Archived C.text
        |> C.buildCustom


type alias TodoRefs =
    { text : Ref Todo C.Settable String, done : Ref Todo C.Settable Bool }


todoDoc : C.RecordRefs Todo TodoRefs
todoDoc =
    C.record Todo TodoRefs
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias OutlineRefs =
    { text : Ref OutlineNode C.Settable String }


outlineDoc : C.RecordRefs OutlineNode OutlineRefs
outlineDoc =
    C.record OutlineNode OutlineRefs
        |> C.field "text" .text C.text
        |> C.build


type alias BoardRefs =
    { title : Ref Board C.Settable String
    , status : Ref Board (C.Variants Status) Status
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , outline : Ref Board (C.TreeK C.Nested OutlineNode) (Tree.Forest OutlineNode)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , likes : Ref Board C.Counter Int
    }


boardDoc : C.RecordRefs Board BoardRefs
boardDoc =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "status" .status statusDoc.schema
        |> C.field "todos" .todos (C.movableList todoDoc.schema)
        |> C.field "outline" .outline (C.tree outlineDoc.schema)
        |> C.field "files" .files (C.dict C.richText)
        |> C.field "likes" .likes C.counter
        |> C.build


refs : BoardRefs
refs =
    boardDoc.refs


init : String -> Doc Board
init name =
    C.init (Id.replica name) boardDoc.schema


ok : Doc Board -> Result C.EditError (Doc Board) -> Doc Board
ok fb =
    Result.withDefault fb


{-| The demo's steady-state send: `encodeSince lastSent`, applied to the peer.
-}
deltaSync : Version -> Doc Board -> Doc Board -> Doc Board
deltaSync since from to =
    Doc.decodeInto (Doc.encodeSince since from) to |> Result.withDefault to


type alias Version =
    Doc.Version


suite : Test
suite =
    describe "demo schema sync (Ref edits + delta wire)"
        [ test "full-state exchange converges (the connect handshake)" <|
            \_ ->
                let
                    alice =
                        init "alice"
                            |> (\d -> C.set refs.title "Trip" d |> ok d)
                            |> (\d -> C.append todoDoc.schema (Todo "pack" False) refs.todos d |> ok d)

                    bob =
                        Doc.decodeInto (Doc.encode alice) (init "bob") |> Result.withDefault (init "bob")
                in
                Expect.equal (C.read alice) (C.read bob)
        , test "a title edit delta reaches a peer" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    alice =
                        C.set refs.title "Hello" (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob |> Result.map .title |> Expect.equal (Ok "Hello")
        , test "a status variant switch delta reaches a peer" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    alice =
                        C.switch refs.status (Archived "old") (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob |> Result.map .status |> Expect.equal (Ok (Archived "old"))
        , test "an appended todo delta reaches a peer" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    alice =
                        C.append todoDoc.schema (Todo "buy milk" False) refs.todos (init "alice") |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob |> Result.map (.todos >> List.map .text) |> Expect.equal (Ok [ "buy milk" ])
        , test "an added outline node delta reaches a peer" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    alice =
                        C.addChild outlineDoc.schema (OutlineNode "root") Nothing refs.outline (init "alice")
                            |> ok (init "alice")

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob
                    |> Result.map (.outline >> List.map (Tree.itemValue >> .text))
                    |> Expect.equal (Ok [ "root" ])
        , test "a file's rich-text edit + mark delta reaches a peer (the demo's editor path)" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    file =
                        refs.files |> C.key "notes.md" C.richText

                    -- create a file, type text, then bold a range — the demo's intents
                    alice =
                        init "alice"
                            |> (\d -> C.setKey C.richText "notes.md" [] refs.files d |> ok d)
                            |> (\d -> C.setRich file "hello" d |> ok d)
                            |> (\d -> C.mark file 0 3 "bold" Flag d |> ok d)

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob
                    |> Result.toMaybe
                    |> Maybe.andThen (\b -> Dict.get "notes.md" b.files)
                    |> Maybe.map (List.map (\s -> ( s.text, Dict.keys s.marks )))
                    |> Expect.equal (Just [ ( "hel", [ "bold" ] ), ( "lo", [] ) ])
        , test "a likes-counter increment delta reaches a peer" <|
            \_ ->
                let
                    before =
                        Doc.version (init "alice")

                    alice =
                        init "alice"
                            |> (\d -> C.increment refs.likes 1 d |> ok d)
                            |> (\d -> C.increment refs.likes 1 d |> ok d)

                    bob =
                        deltaSync before alice (init "bob")
                in
                C.read bob |> Result.map .likes |> Expect.equal (Ok 2)
        ]
