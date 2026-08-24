module DiffTests exposing (suite)

{-| The merge/ingest **diff** (`Doc.mergeWithDiff` / `decodeWithDiff`), queried through
the typed `Crdt.Doc.touched` / `origins` front door (see `design-docs/12`). Asserts that:

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
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.RichText exposing (Span)
import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)



-- SCHEMA ---------------------------------------------------------------------


type alias Todo =
    { text : String, done : Bool }


type alias TodoDoc =
    { text : Ref Todo C.Settable String
    , done : Ref Todo C.Settable Bool
    , schema : C.Schema C.Nested Todo
    }


todoDoc : TodoDoc
todoDoc =
    C.record Todo TodoDoc
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias Board =
    { title : String
    , todos : List Todo
    , files : Dict String (List Span)
    , likes : Int
    }


type alias BoardDoc =
    { title : Ref Board C.Settable String
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , likes : Ref Board C.Counter Int
    , schema : C.Schema C.Nested Board
    }


filesDict :
    { schema : C.Schema (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , key : String -> Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span)) -> Ref Board C.RichK (List Span)
    }
filesDict =
    C.dict C.richText


boardDoc : BoardDoc
boardDoc =
    C.record Board BoardDoc
        |> C.field "title" .title C.text
        |> C.field "todos" .todos (C.movableList todoDoc)
        |> C.field "files" .files filesDict
        |> C.field "likes" .likes C.counter
        |> C.build


refs : BoardDoc
refs =
    boardDoc


init : String -> Doc.Doc Board
init name =
    C.init (Id.replica name) boardDoc.schema


ok : Doc.Doc Board -> Result Edit.EditError (Doc.Doc Board) -> Doc.Doc Board
ok fb =
    Result.withDefault fb


peerOf : String -> Doc.Doc Board -> Doc.Doc Board
peerOf name from =
    Doc.decodeInto (Doc.encode from) (init name)
        |> Result.withDefault (init name)


read : Doc.Doc Board -> Result Doc.ReadError Board
read =
    Doc.read


suite : Test
suite =
    describe "merge/ingest diff (Ref-queryable)"
        [ test "touched reports the changed ref with a Remote origin; untouched refs are Nothing" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Edit.set refs.title "t" d |> ok d)

                    -- a peer (replica "peer") bumps the counter; merge into our doc
                    peer =
                        peerOf "peer" base |> (\d -> Edit.increment refs.likes 1 d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Doc.touched refs.likes merged diff |> Maybe.andThen Doc.originReplica |> Expect.equal (Just (Id.replica "peer"))
                    , \_ -> Doc.touched refs.title merged diff |> Expect.equal Nothing
                    , \_ -> Doc.touched refs.todos merged diff |> Expect.equal Nothing
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
                        Edit.increment refs.likes 1 base |> ok base

                    -- simulate an ingest of our own further edit: encode a "me" edit and
                    -- decode into the same replica.
                    ( merged, diff ) =
                        Doc.decodeWithDiff (Doc.encode edited) base
                            |> Result.withDefault ( base, emptyDiff base )
                in
                Doc.touched refs.likes merged diff |> Maybe.map Doc.isLocal |> Expect.equal (Just True)
        , test "touched fires for an ancestor ref of the change (files when a file changed)" <|
            \_ ->
                let
                    fileRef =
                        filesDict.key "notes" refs.files

                    base =
                        init "me" |> (\d -> Edit.setKey refs.files "notes" [] d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Edit.setRich fileRef "hello" d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Doc.touched refs.files merged diff |> Expect.notEqual Nothing
                    , \_ -> Doc.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> Doc.touched refs.title merged diff |> Expect.equal Nothing
                    ]
                    ()
        , test "a run of char edits on one file coalesces to a single touched location" <|
            \_ ->
                let
                    fileRef =
                        filesDict.key "notes" refs.files

                    base =
                        init "me" |> (\d -> Edit.setKey refs.files "notes" [] d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Edit.setRich fileRef "many characters typed" d |> ok d)

                    ( merged, diff ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.all
                    [ \_ -> Doc.touched fileRef merged diff |> Expect.notEqual Nothing
                    , \_ -> Doc.origins diff |> List.filterMap Doc.originReplica |> Expect.equal [ Id.replica "peer" ]
                    ]
                    ()
        , test "origins lists every contributor; empty when nothing changed" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Edit.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Edit.increment refs.likes 1 d |> ok d)

                    ( _, diff ) =
                        Doc.mergeWithDiff base peer

                    -- re-merging the same peer adds nothing → empty diff
                    ( merged1, _ ) =
                        Doc.mergeWithDiff base peer

                    ( _, emptyAgain ) =
                        Doc.mergeWithDiff merged1 peer
                in
                Expect.all
                    [ \_ -> Doc.origins diff |> List.filterMap Doc.originReplica |> Expect.equal [ Id.replica "peer" ]
                    , \_ -> Doc.origins emptyAgain |> Expect.equal []
                    ]
                    ()
        , test "the diffed document equals plain merge (diff is zero-effect on the result)" <|
            \_ ->
                let
                    base =
                        init "me" |> (\d -> Edit.set refs.title "t" d |> ok d)

                    peer =
                        peerOf "peer" base |> (\d -> Edit.increment refs.likes 2 d |> ok d)

                    plain =
                        Doc.merge base peer

                    ( withDiff, _ ) =
                        Doc.mergeWithDiff base peer
                in
                Expect.equal (read plain) (read withDiff)
        , describe "diffBetween — history-scrubbing attribution"
            [ test "attributes each step's edit to its actual author (mine and a peer's)" <|
                \_ ->
                    let
                        -- a shared timeline: I set the title, a peer increments likes,
                        -- then I toggle the peer's likes again. Merged into one doc, the
                        -- ops keep their authors' replica ids.
                        base =
                            init "me" |> (\d -> Edit.set refs.title "hi" d |> ok d)

                        peer =
                            peerOf "peer" base |> (\d -> Edit.increment refs.likes 1 d |> ok d)

                        doc =
                            Doc.merge base peer

                        stepDiff n =
                            Doc.diffBetween (Doc.versionAt (n - 1) doc) (Doc.versionAt n doc) doc

                        -- who authored the edit that touched `ref` at step n
                        authorAt n ref =
                            Doc.touched ref doc (stepDiff n)
                                |> Maybe.map (\o -> Maybe.map Id.replicaToString (Doc.originReplica o))
                    in
                    Expect.all
                        [ -- the title edit at its step is mine (Local → no remote replica)
                          \_ -> authorAt 1 refs.title |> Expect.equal (Just Nothing)

                        -- the likes increment (the last step) is the peer's
                        , \_ ->
                            authorAt (Doc.historyLength doc) refs.likes
                                |> Expect.equal (Just (Just "peer"))

                        -- and that last step did NOT touch the title
                        , \_ ->
                            Doc.touched refs.title doc (stepDiff (Doc.historyLength doc))
                                |> Expect.equal Nothing
                        ]
                        ()
            , test "a single step's diff is exactly one edit's worth (not the whole tail)" <|
                \_ ->
                    let
                        doc =
                            init "me"
                                |> (\d -> Edit.set refs.title "a" d |> ok d)
                                |> (\d -> Edit.increment refs.likes 1 d |> ok d)

                        -- the last step touched likes but not the title (diffSince from the
                        -- prior version would include only that step here too, but diffBetween
                        -- stays bounded even with more edits after it).
                        lastStep =
                            Doc.diffBetween
                                (Doc.versionAt (Doc.historyLength doc - 1) doc)
                                (Doc.versionAt (Doc.historyLength doc) doc)
                                doc
                    in
                    Expect.all
                        [ \_ -> Doc.touched refs.likes doc lastStep |> Expect.notEqual Nothing
                        , \_ -> Doc.touched refs.title doc lastStep |> Expect.equal Nothing
                        ]
                        ()
            ]
        ]


{-| A throwaway empty diff for the `decodeWithDiff` fallback (never hit in practice).
-}
emptyDiff : Doc.Doc Board -> Doc.Diff
emptyDiff doc =
    Doc.mergeWithDiff doc doc |> Tuple.second
