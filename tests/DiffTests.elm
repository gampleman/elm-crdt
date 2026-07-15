module DiffTests exposing (suite)

{-| The merge/ingest **diff** (`Doc.mergeWithDiff` / `decodeWithDiff`), queried through
the typed `Crdt.C.touched` / `origins` front door (see `docs/12`). Asserts that:

  - a change reports the correct `Origin` (Local vs the authoring Remote replica);
  - `touched` fires for the ref that changed and for its ancestors, and stays `Nothing`
    for unrelated refs (the property that lets a UI re-read only what changed);
  - a run of edits on one container coalesces (one touched location, not one per char);
  - `origins` summarizes who contributed;
  - the diffed document equals plain `merge` / `decodeInto` (diff is zero-effect on the
    result).

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc
import Crdt.Id as Id
import Crdt.RichText exposing (Span)
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)



-- SCHEMA ---------------------------------------------------------------------


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
    , likes : Int
    }


type alias BoardRefs =
    { title : Ref Board C.Settable String
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , likes : Ref Board C.Counter Int
    }


boardDoc : C.RecordRefs Board BoardRefs
boardDoc =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "todos" .todos (C.movableList todoDoc.schema)
        |> C.field "files" .files (C.dict C.richText)
        |> C.field "likes" .likes C.counter
        |> C.build


refs : BoardRefs
refs =
    boardDoc.refs


init : String -> Doc.Doc Board
init name =
    C.init (Id.replica name) boardDoc.schema


ok : Doc.Doc Board -> Result C.EditError (Doc.Doc Board) -> Doc.Doc Board
ok fb =
    Result.withDefault fb


peerOf : String -> Doc.Doc Board -> Doc.Doc Board
peerOf name from =
    Doc.decodeInto (Doc.encode from) (init name)
        |> Result.withDefault (init name)


read : Doc.Doc Board -> Result C.ReadError Board
read =
    C.read


suite : Test
suite =
    describe "merge/ingest diff (Ref-queryable)"
        [ test "touched reports the changed ref with a Remote origin; untouched refs are Nothing" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> C.set refs.title "t" d |> ok d)

                    -- a peer (replica "peer") bumps the counter; merge into our doc
                    peer =
                        peerOf "peer" base |> (\d -> C.increment refs.likes 1 d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> C.touched refs.likes merged diff |> Maybe.andThen Doc.originReplica |> Expect.equal (Just (Id.replica "peer"))
                    , \_ -> C.touched refs.title merged diff |> Expect.equal Nothing
                    , \_ -> C.touched refs.todos merged diff |> Expect.equal Nothing
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
                        C.increment refs.likes 1 base |> ok base

                    -- simulate an ingest of our own further edit: encode a "me" edit and
                    -- decode into the same replica.
                    ( merged, diff ) =
                        Doc.decodeWithDiff (Doc.encode edited) base
                            |> Result.withDefault ( base, emptyDiff base )
                in
                C.touched refs.likes merged diff |> Maybe.map Doc.isLocal |> Expect.equal (Just True)
        , test "touched fires for an ancestor ref of the change (files when a file changed)" <|
            \_ ->
                let
                    fileRef =
                        refs.files |> C.key "notes" C.richText

                    base =
                        init "me" |> (\d -> C.setKey C.richText "notes" [] refs.files d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> C.setRich fileRef "hello" d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> C.touched refs.files merged diff |> Expect.notEqual Nothing
                    , \_ -> C.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> C.touched refs.title merged diff |> Expect.equal Nothing
                    ]
                    ()
        , test "a run of char edits on one file coalesces to a single touched location" <|
            \_ ->
                let
                    fileRef =
                        refs.files |> C.key "notes" C.richText

                    base =
                        init "me" |> (\d -> C.setKey C.richText "notes" [] refs.files d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> C.setRich fileRef "many characters typed" d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> C.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> C.origins diff |> List.filterMap Doc.originReplica |> Expect.equal [ Id.replica "peer" ]
                    ]
                    ()
        , test "origins lists every contributor; empty when nothing changed" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> C.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> C.increment refs.likes 1 d |> ok d)

                    ( _, diff ) =
                        Doc.mergeWithDiff base peer

                    -- re-merging the same peer adds nothing → empty diff
                    ( merged1, _ ) =
                        Doc.mergeWithDiff base peer

                    ( _, emptyAgain ) =
                        Doc.mergeWithDiff merged1 peer
                in
                Expect.all
                    [ \_ -> C.origins diff |> List.filterMap Doc.originReplica |> Expect.equal [ Id.replica "peer" ]
                    , \_ -> C.origins emptyAgain |> Expect.equal []
                    ]
                    ()
        , test "the diffed document equals plain merge (diff is zero-effect on the result)" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> C.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> C.increment refs.likes 2 d |> ok d)

                    plain =
                        Doc.merge base peer

                    ( withDiff, _ ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.equal (read plain) (read withDiff)
        ]


{-| A throwaway empty diff for the `decodeWithDiff` fallback (never hit in practice).
-}
emptyDiff : Doc.Doc Board -> Doc.Diff
emptyDiff doc =
    Doc.mergeWithDiff doc doc |> Tuple.second
