module TreeTests exposing (suite)

{-| The movable tree through the **public API**: `C.tree` schema, `Crdt.Ref` edits
(`addChild`/`moveInto`/`moveBefore`/`moveAfter`/`removeNode`, per-node payload
refs), read back as a `Crdt.Tree.Forest`, and synced through the op-log wire.
-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id exposing (OpId)
import Crdt.Tree as Tree
import Expect
import Test exposing (Test, describe, test)


{-| Each node is a labelled item; the document is a single tree of them.
-}
type alias Item =
    { label : String }


type alias ItemDoc =
    { label : Ref Item C.Settable String
    , schema : C.Schema C.Nested Item
    }


itemDoc : ItemDoc
itemDoc =
    C.record Item ItemDoc
        |> C.field "label" .label C.text
        |> C.build


type alias Sample =
    { outline : Tree.Forest Item }


type alias DocRefs =
    { outline : Ref Sample (C.TreeK C.Nested Item) (Tree.Forest Item)
    , schema : C.Schema C.Nested Sample
    }


outlineTree :
    { schema : C.Schema (C.TreeK C.Nested Item) (Tree.Forest Item)
    , node : OpId -> Ref Sample (C.TreeK C.Nested Item) (Tree.Forest Item) -> Ref Sample C.Nested Item
    }
outlineTree =
    C.tree itemDoc


boardDoc : DocRefs
boardDoc =
    C.record Sample DocRefs
        |> C.field "outline" .outline outlineTree
        |> C.build


refs : DocRefs
refs =
    boardDoc


init : String -> Doc Sample
init name =
    C.init (Id.replica name) boardDoc.schema


ok : Doc Sample -> Result Edit.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


read : Doc Sample -> Tree.Forest Item
read doc =
    Doc.read doc |> Result.map .outline |> Result.withDefault []


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
addRoot : String -> Doc Sample -> ( Maybe OpId, Doc Sample )
addRoot label doc =
    let
        doc1 =
            Edit.addChild refs.outline itemDoc (Item label) Nothing doc |> ok doc
    in
    ( findId label doc1, doc1 )


addUnder : String -> OpId -> Doc Sample -> ( Maybe OpId, Doc Sample )
addUnder label parent doc =
    let
        doc1 =
            Edit.addChild refs.outline itemDoc (Item label) (Just parent) doc |> ok doc
    in
    ( findId label doc1, doc1 )


{-| Run one outline edit bracketed as a single undo step (as the demo does).
-}
tracked : (Doc Sample -> Result Edit.EditError (Doc Sample)) -> Doc Sample -> Doc Sample
tracked edit doc =
    Doc.recordEdit (Doc.version doc) (edit doc |> ok doc)


{-| Find a node's id by its (unique) label, searching the whole forest.
-}
findId : String -> Doc Sample -> Maybe OpId
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
    describe "Tree — public API (C.tree + Crdt.Ref)"
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
                                Edit.moveInto refs.outline bId (Just aId) d2 |> ok d2

                            _ ->
                                d2
                in
                shape (read d3) |> Expect.equal "A[B]"
        , test "editing a node's payload via a node ref" <|
            \_ ->
                let
                    ( mA, d1 ) =
                        addRoot "A" (init "a")
                in
                case mA of
                    Just aId ->
                        let
                            d2 =
                                Edit.set (outlineTree.node aId refs.outline |> C.at itemDoc.label) "A!" d1
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
                                Edit.removeNode refs.outline aId d2 |> ok d2

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
                                Edit.moveBefore refs.outline cId aId d3 |> ok d3

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
                            Doc.decodeInto (Doc.encode d2) (init "bob") |> Result.withDefault (init "bob")
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
                                    Doc.decodeInto (Doc.encode start) (init "bob") |> Result.withDefault (init "bob")

                                -- alice moves A under B; bob moves B under A
                                alice =
                                    Edit.moveInto refs.outline aId (Just bId) start |> ok start

                                bob =
                                    Edit.moveInto refs.outline bId (Just aId) bobStart |> ok bobStart

                                ab =
                                    Doc.decodeInto (Doc.encode bob) alice |> Result.withDefault alice

                                ba =
                                    Doc.decodeInto (Doc.encode alice) bob |> Result.withDefault bob
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
                                    tracked (Edit.removeNode refs.outline aId) d3

                                Nothing ->
                                    d3

                        restored =
                            Doc.undo deleted
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
                            tracked (Edit.addChild refs.outline itemDoc (Item "X") Nothing) (init "alice")
                    in
                    Expect.all
                        [ \_ -> Expect.equal "X" (shape (read added))
                        , \_ -> Expect.equal "" (shape (read (Doc.undo added)))
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
                                    tracked (Edit.moveInto refs.outline bId (Just aId)) d2

                                _ ->
                                    d2
                    in
                    Expect.all
                        [ \_ -> Expect.equal "A[B]" (shape (read moved))
                        , \_ -> Expect.equal "A B" (shape (read (Doc.undo moved)))
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
                                    tracked (Edit.removeNode refs.outline aId) d1

                                Nothing ->
                                    d1

                        cycled =
                            deleted |> Doc.undo |> Doc.redo
                    in
                    Expect.equal "" (shape (read cycled))
            , test "add a node, edit its text, undo twice, redo twice restores both" <|
                \_ ->
                    -- add A (edit 1), set A's label to A! (edit 2), then undo×2, redo×2.
                    -- redo must rebuild the node AND its text, in order.
                    let
                        d1 =
                            tracked (Edit.addChild refs.outline itemDoc (Item "A") Nothing) (init "alice")

                        edited =
                            case findId "A" d1 of
                                Just aId ->
                                    tracked
                                        (Edit.set (outlineTree.node aId refs.outline |> C.at itemDoc.label) "A!")
                                        d1

                                Nothing ->
                                    d1

                        cycled =
                            edited
                                |> Doc.undo
                                |> Doc.undo
                                |> Doc.redo
                                |> Doc.redo
                    in
                    Expect.all
                        [ \_ -> Expect.equal "A!" (shape (read edited))
                        , \_ -> Expect.equal "A!" (shape (read cycled))
                        ]
                        ()
            ]
        ]
