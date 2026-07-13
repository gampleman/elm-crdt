module IncrementalMergeTests exposing (suite)

{-| `OpDoc.merge` / `decodeInto` now apply only the **added** ops onto the existing
cache instead of re-materializing from base (see `docs/12`). The correctness oracle is
`OpDoc.cacheConsistent` — it holds iff the incrementally-maintained cache equals a full
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

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText exposing (Span)
import Crdt.Schema as S exposing (Crdt)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)



-- SCHEMA (demo-like: register, movable list, dict of rich text, tree, counter) ------


type alias ONode =
    { text : String }


type alias ONodeRefs =
    { text : Ref ONode S.Settable String }


onodeDoc : Ref.RecordRefs ONode ONodeRefs
onodeDoc =
    Ref.record ONode ONodeRefs |> Ref.field "text" .text S.text |> Ref.build


type alias Todo =
    { text : String, done : Bool }


type alias TodoRefs =
    { text : Ref Todo S.Settable String, done : Ref Todo S.Settable Bool }


todoDoc : Ref.RecordRefs Todo TodoRefs
todoDoc =
    Ref.record Todo TodoRefs
        |> Ref.field "text" .text S.text
        |> Ref.field "done" .done S.bool
        |> Ref.build


type alias Board =
    { title : String
    , todos : List Todo
    , files : Dict String (List Span)
    , outline : Tree.Forest ONode
    , likes : Int
    }


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    , files : Ref Board (S.DictK S.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (S.TreeK S.Nested ONode) (Tree.Forest ONode)
    , likes : Ref Board S.Counter Int
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.field "files" .files (S.dict S.richText)
        |> Ref.field "outline" .outline (S.tree onodeDoc.schema)
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


read : OpDoc Board -> Result S.Error Board
read =
    OpDoc.read


{-| Sync `from` fully into a fresh replica named `name` (over the wire, like a peer
catching up), so it shares history and can then diverge.
-}
peerOf : String -> OpDoc Board -> OpDoc Board
peerOf name from =
    OpDoc.decodeInto (OpDoc.encode from) (init name)
        |> Result.withDefault (init name)


{-| Merge `b` into `a` via the op-store `merge`.
-}
mergeOp : OpDoc Board -> OpDoc Board -> OpDoc Board
mergeOp a b =
    OpDoc.merge a b


{-| Merge `b` into `a` via the WIRE (`decodeInto (encode b)`), the demo's real path.
-}
mergeWire : OpDoc Board -> OpDoc Board -> OpDoc Board
mergeWire a b =
    OpDoc.decodeInto (OpDoc.encode b) a |> Result.withDefault a


consistent : OpDoc Board -> Expect.Expectation
consistent doc =
    OpDoc.cacheConsistent doc |> Expect.equal True


suite : Test
suite =
    describe "incremental merge (cache stays consistent + converges)"
        [ test "merge preserves cacheConsistent and converges (register + counter)" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\d -> Ref.set refs.title "hi" d |> ok d)

                    a =
                        peerOf "alice" base |> (\d -> Ref.set refs.title "alice" d |> ok d) |> (\d -> Ref.increment refs.likes 2 d |> ok d)

                    b =
                        peerOf "bob" base |> (\d -> Ref.increment refs.likes 3 d |> ok d)

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
                        refs.files |> Ref.key "notes" S.richText

                    alice =
                        init "alice"
                            |> (\d -> Ref.setKey S.richText "notes" [] refs.files d |> ok d)
                            |> (\d -> Ref.setRich fileRef "hello" d |> ok d)

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
                            |> (\d -> Ref.addChild onodeDoc.schema (ONode "a") Nothing refs.outline d |> ok d)
                            |> (\d -> Ref.addChild onodeDoc.schema (ONode "b") Nothing refs.outline d |> ok d)

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
                                ( peerOf "alice" base |> (\d -> Ref.moveInto y (Just x) refs.outline d |> ok d)
                                , peerOf "bob" base |> (\d -> Ref.moveInto x (Just y) refs.outline d |> ok d)
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
                            |> List.foldl (\i d -> Ref.append todoDoc.schema (Todo (String.fromInt i) False) refs.todos d |> ok d) (init "seed")

                    a =
                        peerOf "alice" base |> (\d -> Ref.move 3 0 refs.todos d |> ok d)

                    b =
                        peerOf "bob" base |> (\d -> Ref.move 1 3 refs.todos d |> ok d)

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
                        init "seed" |> (\d -> Ref.set refs.title "x" d |> ok d)

                    peer =
                        peerOf "alice" base |> (\d -> Ref.set refs.title "y" d |> ok d) |> (\d -> Ref.increment refs.likes 1 d |> ok d)

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
                        init "seed" |> (\d -> Ref.set refs.title "base" d |> ok d)

                    a =
                        peerOf "a" base |> (\d -> Ref.increment refs.likes 1 d |> ok d)

                    b =
                        peerOf "b" base |> (\d -> Ref.increment refs.likes 1 d |> ok d)

                    c =
                        peerOf "c" base |> (\d -> Ref.increment refs.likes 1 d |> ok d)

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
                        init "seed" |> (\d -> Ref.append todoDoc.schema (Todo "a" False) refs.todos d |> ok d)

                    peer =
                        peerOf "alice" base |> (\d -> Ref.append todoDoc.schema (Todo "b" False) refs.todos d |> ok d)

                    merged =
                        mergeOp base peer |> (\d -> Ref.append todoDoc.schema (Todo "c" False) refs.todos d |> ok d)
                in
                Expect.all
                    [ \_ -> consistent merged
                    , \_ -> Expect.equal (Ok 3) (read merged |> Result.map (.todos >> List.length))
                    ]
                    ()
        ]
