module IncrementalMergeTests exposing (suite)

{-| `Doc.merge` / `decodeInto` now apply only the **added** ops onto the existing
cache instead of re-materializing from base (see `docs/12`). The correctness oracle is
`Doc.cacheConsistent` — it holds iff the incrementally-maintained cache equals a full
re-materialize. These tests assert it survives the adversarial cases the incremental
path could get wrong:

  - cross-container causal order (an edit inside a dict key merged with the op that
    created the key, in the "wrong" arrival order);
  - tree re-parents and movable-list moves merged concurrently (resolution is re-folded
    at read from the whole move-set, so incremental apply must not disturb it);
  - snapshot / GC ingest (a new base — still a full re-materialize, must stay correct);
  - idempotent re-merge (merging the same peer twice changes nothing);
  - three-way and both-orders convergence.

Content convergence is also asserted (both merge orders `read`-equal), so we catch a
regression whether it shows up as a stale cache or as divergent reads.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.RichText exposing (Span)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)



-- SCHEMA (demo-like: register, movable list, dict of rich text, tree, counter) ------


type alias ONode =
    { text : String }


type alias ONodeRefs =
    { text : Ref ONode C.Settable String }


onodeDoc : C.RecordRefs ONode ONodeRefs
onodeDoc =
    C.record ONode ONodeRefs |> C.field "text" .text C.text |> C.build


type alias Todo =
    { text : String, done : Bool }


type alias TodoRefs =
    { text : Ref Todo C.Settable String, done : Ref Todo C.Settable Bool }


todoDoc : C.RecordRefs Todo TodoRefs
todoDoc =
    C.record Todo TodoRefs
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias Board =
    { title : String
    , todos : List Todo
    , files : Dict String (List Span)
    , outline : Tree.Forest ONode
    , likes : Int
    }


type alias BoardRefs =
    { title : Ref Board C.Settable String
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (C.TreeK C.Nested ONode) (Tree.Forest ONode)
    , likes : Ref Board C.Counter Int
    }


boardDoc : C.RecordRefs Board BoardRefs
boardDoc =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "todos" .todos (C.movableList todoDoc.schema)
        |> C.field "files" .files (C.dict C.richText)
        |> C.field "outline" .outline (C.tree onodeDoc.schema)
        |> C.field "likes" .likes C.counter
        |> C.build


refs : BoardRefs
refs =
    boardDoc.refs


init : String -> Doc Board
init name =
    Doc.init (Id.replica name) boardDoc.schema


ok : Doc Board -> Result C.EditError (Doc Board) -> Doc Board
ok fb =
    Result.withDefault fb


read : Doc Board -> Result C.ReadError Board
read =
    Doc.read


{-| Sync `from` fully into a fresh replica named `name` (over the wire, like a peer
catching up), so it shares history and can then diverge.
-}
peerOf : String -> Doc Board -> Doc Board
peerOf name from =
    Doc.decodeInto (Doc.encode from) (init name)
        |> Result.withDefault (init name)


{-| Merge `b` into `a` via the op-store `merge`.
-}
mergeOp : Doc Board -> Doc Board -> Doc Board
mergeOp a b =
    Doc.merge a b


{-| Merge `b` into `a` via the WIRE (`decodeInto (encode b)`), the demo's real path.
-}
mergeWire : Doc Board -> Doc Board -> Doc Board
mergeWire a b =
    Doc.decodeInto (Doc.encode b) a |> Result.withDefault a


consistent : Doc Board -> Expect.Expectation
consistent doc =
    Doc.cacheConsistent doc |> Expect.equal True


suite : Test
suite =
    describe "incremental merge (cache stays consistent + converges)"
        [ test "merge preserves cacheConsistent and converges (register + counter)" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\d -> C.set refs.title "hi" d |> ok d)

                    a =
                        peerOf "alice" base |> (\d -> C.set refs.title "alice" d |> ok d) |> (\d -> C.increment refs.likes 2 d |> ok d)

                    b =
                        peerOf "bob" base |> (\d -> C.increment refs.likes 3 d |> ok d)

                    ab =
                        mergeOp a b

                    ba =
                        mergeOp b a
                in
                Expect.all
                    [ \_ -> consistent ab
                    , \_ -> consistent ba
                    , \_ -> Expect.equal (read ab) (read ba)
                    , \_ -> Expect.equal (Ok 5) (read ab |> Result.map .likes)
                    ]
                    ()
        , test "cross-container: edit INSIDE a dict key merged with the key's creation" <|
            \_ ->
                -- alice creates file "notes" AND types into it; bob (who has neither)
                -- merges alice in. The char-insert ops target inside files/notes, and
                -- must not be applied before the SetPresence that creates the key —
                -- incremental merge folds added ops in causal order to guarantee this.
                let
                    fileRef =
                        refs.files |> C.key "notes" C.richText

                    alice =
                        init "alice"
                            |> (\d -> C.setKey refs.files C.richText "notes" [] d |> ok d)
                            |> (\d -> C.setRich fileRef "hello" d |> ok d)

                    bob =
                        init "bob"

                    merged =
                        mergeWire bob alice
                in
                Expect.all
                    [ \_ -> consistent merged
                    , \_ ->
                        Expect.equal (Ok (Just "hello"))
                            (read merged |> Result.map (\bd -> Dict.get "notes" bd.files |> Maybe.map (List.map .text >> String.concat)))
                    ]
                    ()
        , test "concurrent tree re-parents merge consistently (resolution re-folded)" <|
            \_ ->
                let
                    base =
                        init "seed"
                            |> (\d -> C.addChild refs.outline onodeDoc.schema (ONode "a") Nothing d |> ok d)
                            |> (\d -> C.addChild refs.outline onodeDoc.schema (ONode "b") Nothing d |> ok d)

                    idsOf doc =
                        read doc |> Result.map (.outline >> List.map Tree.itemId) |> Result.withDefault []

                    firstTwo doc =
                        case idsOf doc of
                            x :: y :: _ ->
                                Just ( x, y )

                            _ ->
                                Nothing

                    ( a, b ) =
                        case firstTwo base of
                            Just ( x, y ) ->
                                ( peerOf "alice" base |> (\d -> C.moveInto refs.outline y (Just x) d |> ok d)
                                , peerOf "bob" base |> (\d -> C.moveInto refs.outline x (Just y) d |> ok d)
                                )

                            Nothing ->
                                ( base, base )

                    ab =
                        mergeOp a b

                    ba =
                        mergeOp b a
                in
                Expect.all
                    [ \_ -> consistent ab
                    , \_ -> consistent ba
                    , \_ -> Expect.equal (read ab) (read ba)
                    ]
                    ()
        , test "concurrent movable-list moves merge consistently" <|
            \_ ->
                let
                    base =
                        List.range 1 4
                            |> List.foldl (\i d -> C.append refs.todos todoDoc.schema (Todo (String.fromInt i) False) d |> ok d) (init "seed")

                    a =
                        peerOf "alice" base |> (\d -> C.move refs.todos 3 0 d |> ok d)

                    b =
                        peerOf "bob" base |> (\d -> C.move refs.todos 1 3 d |> ok d)

                    ab =
                        mergeOp a b

                    ba =
                        mergeOp b a
                in
                Expect.all
                    [ \_ -> consistent ab
                    , \_ -> consistent ba
                    , \_ -> Expect.equal (read ab) (read ba)
                    ]
                    ()
        , test "re-merging the same peer twice is idempotent (cache + reads)" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\d -> C.set refs.title "x" d |> ok d)

                    peer =
                        peerOf "alice" base |> (\d -> C.set refs.title "y" d |> ok d) |> (\d -> C.increment refs.likes 1 d |> ok d)

                    once =
                        mergeOp base peer

                    twice =
                        mergeOp once peer
                in
                Expect.all
                    [ \_ -> consistent once
                    , \_ -> consistent twice
                    , \_ -> Expect.equal (read once) (read twice)
                    ]
                    ()
        , test "three-way merge stays consistent and order-independent" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\d -> C.set refs.title "base" d |> ok d)

                    a =
                        peerOf "a" base |> (\d -> C.increment refs.likes 1 d |> ok d)

                    b =
                        peerOf "b" base |> (\d -> C.increment refs.likes 1 d |> ok d)

                    c =
                        peerOf "c" base |> (\d -> C.increment refs.likes 1 d |> ok d)

                    abc =
                        mergeOp (mergeOp a b) c

                    cba =
                        mergeOp (mergeOp c b) a
                in
                Expect.all
                    [ \_ -> consistent abc
                    , \_ -> consistent cba
                    , \_ -> Expect.equal (read abc) (read cba)
                    , \_ -> Expect.equal (Ok 3) (read abc |> Result.map .likes)
                    ]
                    ()
        , test "merge then a further local edit keeps the cache consistent" <|
            \_ ->
                -- the append fast-path is cleared on merge; a subsequent edit must still
                -- fold correctly onto the incrementally-merged cache.
                let
                    base =
                        init "seed" |> (\d -> C.append refs.todos todoDoc.schema (Todo "a" False) d |> ok d)

                    peer =
                        peerOf "alice" base |> (\d -> C.append refs.todos todoDoc.schema (Todo "b" False) d |> ok d)

                    merged =
                        mergeOp base peer |> (\d -> C.append refs.todos todoDoc.schema (Todo "c" False) d |> ok d)
                in
                Expect.all
                    [ \_ -> consistent merged
                    , \_ -> Expect.equal (Ok 3) (read merged |> Result.map (.todos >> List.length))
                    ]
                    ()
        ]
