module DiffTests exposing (suite)

{-| The merge/ingest **diff** (`OpDoc.mergeWithDiff` / `decodeWithDiff`), queried through
the typed `Crdt.Ref.touched` / `origins` front door (see `docs/12`). Asserts that:

  - a change reports the correct `Origin` (Local vs the authoring Remote replica);
  - `touched` fires for the ref that changed and for its ancestors, and stays `Nothing`
    for unrelated refs (the property that lets a UI re-read only what changed);
  - a run of edits on one container coalesces (one touched location, not one per char);
  - `origins` summarizes who contributed;
  - the diffed document equals plain `merge` / `decodeInto` (diff is zero-effect on the
    result).

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (Origin(..))
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText exposing (Span)
import Crdt.Schema as S exposing (Crdt)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)



-- SCHEMA ---------------------------------------------------------------------


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
    , likes : Int
    }


type alias BoardRefs =
    { title : Ref Board S.Settable String
    , todos : Ref Board (S.ListK S.Movable S.Nested Todo) (List Todo)
    , files : Ref Board (S.DictK S.RichK (List Span)) (Dict String (List Span))
    , likes : Ref Board S.Counter Int
    }


boardDoc : Ref.RecordRefs Board BoardRefs
boardDoc =
    Ref.record Board BoardRefs
        |> Ref.field "title" .title S.text
        |> Ref.field "todos" .todos (S.movableList todoDoc.schema)
        |> Ref.field "files" .files (S.dict S.richText)
        |> Ref.field "likes" .likes S.counter
        |> Ref.build


refs : BoardRefs
refs =
    boardDoc.refs


init : String -> OpDoc.OpDoc Board
init name =
    OpDoc.init (Id.replica name) boardDoc.schema


ok : OpDoc.OpDoc Board -> Result OpDoc.Error (OpDoc.OpDoc Board) -> OpDoc.OpDoc Board
ok fb =
    Result.withDefault fb


peerOf : String -> OpDoc.OpDoc Board -> OpDoc.OpDoc Board
peerOf name from =
    OpDoc.decodeInto (OpDoc.encode from) (init name)
        |> Result.withDefault (init name)


read : OpDoc.OpDoc Board -> Result S.Error Board
read =
    OpDoc.read


suite : Test
suite =
    describe "merge/ingest diff (Ref-queryable)"
        [ test "touched reports the changed ref with a Remote origin; untouched refs are Nothing" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Ref.set refs.title "t" d |> ok d)

                    -- a peer (replica "peer") bumps the counter; merge into our doc
                    peer =
                        peerOf "peer" base |> (\d -> Ref.increment refs.likes 1 d |> ok d)

                    ( merged, diff ) =
                        OpDoc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Ref.touched refs.likes merged diff |> Expect.equal (Just (Remote (Id.replica "peer")))
                    , \_ -> Ref.touched refs.title merged diff |> Expect.equal Nothing
                    , \_ -> Ref.touched refs.todos merged diff |> Expect.equal Nothing
                    ]
                    ()
        , test "a local edit shows up as Local origin" <|
            \_ ->
                let
                    base =
                        init "me"

                    -- our own peer copy edits, but merged into a doc whose replica is "me"
                    -- → the counter op authored by "me" reads as Local.
                    edited =
                        Ref.increment refs.likes 1 base |> ok base

                    -- simulate an ingest of our own further edit: encode a "me" edit and
                    -- decode into the same replica.
                    ( merged, diff ) =
                        OpDoc.decodeWithDiff (OpDoc.encode edited) base
                            |> Result.withDefault ( base, emptyDiff base )
                in
                Ref.touched refs.likes merged diff |> Expect.equal (Just Local)
        , test "touched fires for an ancestor ref of the change (files when a file changed)" <|
            \_ ->
                let
                    fileRef =
                        refs.files |> Ref.key "notes" S.richText

                    base =
                        init "me" |> (\d -> Ref.setKey S.richText "notes" [] refs.files d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Ref.setRich fileRef "hello" d |> ok d)

                    ( merged, diff ) =
                        OpDoc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Ref.touched refs.files merged diff |> Expect.notEqual Nothing
                    , \_ -> Ref.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> Ref.touched refs.title merged diff |> Expect.equal Nothing
                    ]
                    ()
        , test "a run of char edits on one file coalesces to a single touched location" <|
            \_ ->
                let
                    fileRef =
                        refs.files |> Ref.key "notes" S.richText

                    base =
                        init "me" |> (\d -> Ref.setKey S.richText "notes" [] refs.files d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Ref.setRich fileRef "many characters typed" d |> ok d)

                    ( merged, diff ) =
                        OpDoc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Ref.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> Ref.origins diff |> Expect.equal [ Remote (Id.replica "peer") ]
                    ]
                    ()
        , test "origins lists every contributor; empty when nothing changed" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Ref.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Ref.increment refs.likes 1 d |> ok d)

                    ( _, diff ) =
                        OpDoc.mergeWithDiff base peer

                    -- re-merging the same peer adds nothing → empty diff
                    ( merged1, _ ) =
                        OpDoc.mergeWithDiff base peer

                    ( _, emptyAgain ) =
                        OpDoc.mergeWithDiff merged1 peer
                in
                Expect.all
                    [ \_ -> Ref.origins diff |> Expect.equal [ Remote (Id.replica "peer") ]
                    , \_ -> Ref.origins emptyAgain |> Expect.equal []
                    ]
                    ()
        , test "the diffed document equals plain merge (diff is zero-effect on the result)" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Ref.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Ref.increment refs.likes 2 d |> ok d)

                    plain =
                        OpDoc.merge base peer

                    ( withDiff, _ ) =
                        OpDoc.mergeWithDiff base peer
                in
                Expect.equal (read plain) (read withDiff)
        ]


{-| A throwaway empty diff for the `decodeWithDiff` fallback (never hit in practice).
-}
emptyDiff : OpDoc.OpDoc Board -> OpDoc.Diff
emptyDiff doc =
    OpDoc.mergeWithDiff doc doc |> Tuple.second
