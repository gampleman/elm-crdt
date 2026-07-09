module TreeTests exposing (suite)

{-| The movable tree through the **public API**: `S.tree` schema, `Crdt.Ref` edits
(`addChild`/`moveInto`/`moveBefore`/`moveAfter`/`removeNode`, per-node payload
refs), read back as a `Crdt.Tree.Forest`, and synced through the op-log wire.
-}

import Crdt.Id as Id exposing (OpId)
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Schema as S
import Crdt.Tree as Tree
import Expect
import Test exposing (Test, describe, test)


{-| Each node is a labelled item; the document is a single tree of them.
-}
type alias Item =
    { label : String }


type alias ItemRefs =
    { label : Ref Item S.Settable String }


itemDoc : Ref.RecordRefs Item ItemRefs
itemDoc =
    Ref.record Item ItemRefs
        |> Ref.field "label" .label S.text
        |> Ref.build


type alias Doc =
    { outline : Tree.Forest Item }


type alias DocRefs =
    { outline : Ref Doc (S.TreeK S.Nested Item) (Tree.Forest Item) }


boardDoc : Ref.RecordRefs Doc DocRefs
boardDoc =
    Ref.record Doc DocRefs
        |> Ref.field "outline" .outline (S.tree itemDoc.schema)
        |> Ref.build


refs : DocRefs
refs =
    boardDoc.refs


init : String -> OpDoc Doc
init name =
    OpDoc.init (Id.replica name) boardDoc.schema


ok : OpDoc Doc -> Result OpDoc.Error (OpDoc Doc) -> OpDoc Doc
ok fb =
    Result.withDefault fb


read : OpDoc Doc -> Tree.Forest Item
read doc =
    OpDoc.read doc |> Result.map .outline |> Result.withDefault []


{-| Flat bracketed render of a forest, e.g. `A[A1] B`, for single-`Expect.equal`
assertions on `String`.
-}
shape : Tree.Forest Item -> String
shape forest =
    forest
        |> List.map
            (\item ->
                let
                    label =
                        Tree.itemValue item |> .label

                    kids =
                        Tree.itemChildren item
                in
                if List.isEmpty kids then
                    label

                else
                    label ++ "[" ++ shape kids ++ "]"
            )
        |> String.join " "


{-| Add a root node, returning its id + the new doc. (We read the id back from the
forest by matching the label, since add doesn't return it.)
-}
addRoot : String -> OpDoc Doc -> ( Maybe OpId, OpDoc Doc )
addRoot label doc =
    let
        doc1 =
            Ref.addChild itemDoc.schema (Item label) Nothing refs.outline doc |> ok doc
    in
    ( findId label doc1, doc1 )


addUnder : String -> OpId -> OpDoc Doc -> ( Maybe OpId, OpDoc Doc )
addUnder label parent doc =
    let
        doc1 =
            Ref.addChild itemDoc.schema (Item label) (Just parent) refs.outline doc |> ok doc
    in
    ( findId label doc1, doc1 )


{-| Run one outline edit bracketed as a single undo step (as the demo does).
-}
tracked : (OpDoc Doc -> Result OpDoc.Error (OpDoc Doc)) -> OpDoc Doc -> OpDoc Doc
tracked edit doc =
    OpDoc.recordEdit (OpDoc.version doc) (edit doc |> ok doc)


{-| Find a node's id by its (unique) label, searching the whole forest.
-}
findId : String -> OpDoc Doc -> Maybe OpId
findId label doc =
    let
        go forest =
            forest
                |> List.concatMap
                    (\item ->
                        if (Tree.itemValue item |> .label) == label then
                            [ Tree.itemId item ]

                        else
                            go (Tree.itemChildren item)
                    )
    in
    go (read doc) |> List.head


suite : Test
suite =
    describe "Tree — public API (S.tree + Crdt.Ref)"
        [ test "fresh tree is empty" <|
            \_ ->
                shape (read (init "a")) |> Expect.equal ""
        , test "add roots and children, read the structure" <|
            \_ ->
                let
                    ( mA, d1 ) =
                        addRoot "A" (init "a")

                    d2 =
                        case mA of
                            Just aId ->
                                addUnder "A1" aId d1 |> Tuple.second

                            Nothing ->
                                d1

                    ( _, d3 ) =
                        addRoot "B" d2
                in
                shape (read d3)
                    |> Expect.equal "A[A1] B"
        , test "moveInto re-parents a node" <|
            \_ ->
                let
                    ( mA, d1 ) =
                        addRoot "A" (init "a")

                    ( mB, d2 ) =
                        addRoot "B" d1

                    -- move B under A
                    d3 =
                        case ( mA, mB ) of
                            ( Just aId, Just bId ) ->
                                Ref.moveInto bId (Just aId) refs.outline d2 |> ok d2

                            _ ->
                                d2
                in
                shape (read d3) |> Expect.equal "A[B]"
        , test "editing a node's payload via treeNode ref" <|
            \_ ->
                let
                    ( mA, d1 ) =
                        addRoot "A" (init "a")
                in
                case mA of
                    Just aId ->
                        let
                            d2 =
                                Ref.set (refs.outline |> Ref.treeNode aId itemDoc.schema |> Ref.at itemDoc.refs.label) "A!" d1
                                    |> ok d1
                        in
                        shape (read d2) |> Expect.equal "A!"

                    Nothing ->
                        Expect.fail "no A id"
        , test "removeNode drops a subtree" <|
            \_ ->
                let
                    ( mA, d1 ) =
                        addRoot "A" (init "a")

                    d2 =
                        case mA of
                            Just aId ->
                                addUnder "A1" aId d1 |> Tuple.second

                            Nothing ->
                                d1

                    d3 =
                        case mA of
                            Just aId ->
                                Ref.removeNode aId refs.outline d2 |> ok d2

                            Nothing ->
                                d2
                in
                shape (read d3) |> Expect.equal ""
        , test "moveBefore / moveAfter reorder siblings" <|
            \_ ->
                let
                    ( _, d1 ) =
                        addRoot "A" (init "a")

                    ( _, d2 ) =
                        addRoot "B" d1

                    ( _, d3 ) =
                        addRoot "C" d2

                    -- move C before A  →  C, A, B
                    d4 =
                        case ( findId "C" d3, findId "A" d3 ) of
                            ( Just cId, Just aId ) ->
                                Ref.moveBefore cId aId refs.outline d3 |> ok d3

                            _ ->
                                d3
                in
                shape (read d4) |> Expect.equal "C A B"
        , describe "sync"
            [ test "a fresh peer converges via full-state exchange" <|
                \_ ->
                    let
                        ( mA, d1 ) =
                            addRoot "A" (init "alice")

                        d2 =
                            case mA of
                                Just aId ->
                                    addUnder "A1" aId d1 |> Tuple.second

                                Nothing ->
                                    d1

                        bob =
                            OpDoc.decodeInto (OpDoc.encode d2) (init "bob") |> Result.withDefault (init "bob")
                    in
                    Expect.equal (shape (read d2)) (shape (read bob))
            , test "concurrent cycle-forming moves converge (both orders agree)" <|
                \_ ->
                    let
                        -- shared start: A and B at root
                        ( mA, s1 ) =
                            addRoot "A" (init "alice")

                        ( mB, start ) =
                            addRoot "B" s1
                    in
                    case ( mA, mB ) of
                        ( Just aId, Just bId ) ->
                            let
                                -- bob is a genuinely separate replica that first
                                -- syncs `start`, so the two concurrent moves get
                                -- distinct (different-replica) move-op ids.
                                bobStart =
                                    OpDoc.decodeInto (OpDoc.encode start) (init "bob") |> Result.withDefault (init "bob")

                                -- alice moves A under B; bob moves B under A
                                alice =
                                    Ref.moveInto aId (Just bId) refs.outline start |> ok start

                                bob =
                                    Ref.moveInto bId (Just aId) refs.outline bobStart |> ok bobStart

                                ab =
                                    OpDoc.decodeInto (OpDoc.encode bob) alice |> Result.withDefault alice

                                ba =
                                    OpDoc.decodeInto (OpDoc.encode alice) bob |> Result.withDefault bob
                            in
                            Expect.all
                                [ \_ -> Expect.equal (shape (read ab)) (shape (read ba))

                                -- both nodes still present, exactly one nesting
                                , \_ -> Expect.notEqual "" (shape (read ab))
                                ]
                                ()

                        _ ->
                            Expect.fail "missing ids"
            ]
        , describe "undo / redo"
            [ test "undo a delete restores the node and its subtree" <|
                \_ ->
                    -- build A[A1] B, then delete A (as a tracked edit), then undo
                    let
                        ( mA, d1 ) =
                            addRoot "A" (init "alice")

                        d2 =
                            case mA of
                                Just aId ->
                                    addUnder "A1" aId d1 |> Tuple.second

                                Nothing ->
                                    d1

                        ( _, d3 ) =
                            addRoot "B" d2

                        deleted =
                            case mA of
                                Just aId ->
                                    tracked (Ref.removeNode aId refs.outline) d3

                                Nothing ->
                                    d3

                        restored =
                            OpDoc.undo deleted
                    in
                    Expect.all
                        [ \_ -> Expect.equal "B" (shape (read deleted))
                        , \_ -> Expect.equal "A[A1] B" (shape (read restored))
                        ]
                        ()
            , test "undo an add removes the node" <|
                \_ ->
                    let
                        added =
                            tracked (Ref.addChild itemDoc.schema (Item "X") Nothing refs.outline) (init "alice")
                    in
                    Expect.all
                        [ \_ -> Expect.equal "X" (shape (read added))
                        , \_ -> Expect.equal "" (shape (read (OpDoc.undo added)))
                        ]
                        ()
            , test "undo a move puts the node back" <|
                \_ ->
                    let
                        ( mA, d1 ) =
                            addRoot "A" (init "alice")

                        ( mB, d2 ) =
                            addRoot "B" d1

                        moved =
                            case ( mA, mB ) of
                                ( Just aId, Just bId ) ->
                                    tracked (Ref.moveInto bId (Just aId) refs.outline) d2

                                _ ->
                                    d2
                    in
                    Expect.all
                        [ \_ -> Expect.equal "A[B]" (shape (read moved))
                        , \_ -> Expect.equal "A B" (shape (read (OpDoc.undo moved)))
                        ]
                        ()
            , test "delete then undo then redo re-deletes" <|
                \_ ->
                    let
                        ( mA, d1 ) =
                            addRoot "A" (init "alice")

                        deleted =
                            case mA of
                                Just aId ->
                                    tracked (Ref.removeNode aId refs.outline) d1

                                Nothing ->
                                    d1

                        cycled =
                            deleted |> OpDoc.undo |> OpDoc.redo
                    in
                    Expect.equal "" (shape (read cycled))
            , test "add a node, edit its text, undo twice, redo twice restores both" <|
                \_ ->
                    -- add A (edit 1), set A's label to A! (edit 2), then undo×2, redo×2.
                    -- redo must rebuild the node AND its text, in order.
                    let
                        d1 =
                            tracked (Ref.addChild itemDoc.schema (Item "A") Nothing refs.outline) (init "alice")

                        edited =
                            case findId "A" d1 of
                                Just aId ->
                                    tracked
                                        (Ref.set (refs.outline |> Ref.treeNode aId itemDoc.schema |> Ref.at itemDoc.refs.label) "A!")
                                        d1

                                Nothing ->
                                    d1

                        cycled =
                            edited
                                |> OpDoc.undo
                                |> OpDoc.undo
                                |> OpDoc.redo
                                |> OpDoc.redo
                    in
                    Expect.all
                        [ \_ -> Expect.equal "A!" (shape (read edited))
                        , \_ -> Expect.equal "A!" (shape (read cycled))
                        ]
                        ()
            ]
        ]
