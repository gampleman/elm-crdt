module UndoPropertyTests exposing (suite)

{-| Property tests over **random sequences of edits** and their undo/redo, on a
document that mixes the tricky containers (a movable tree + a movable list + text).
Each edit is bracketed as one undo step (`recordEdit`), exactly as the demo does.

The invariants a well-behaved undo stack must satisfy for _any_ edit sequence:

  - **full unwind** — after k tracked edits, k `undo`s return the read to the start;
  - **redo replays** — `undo` then `redo` returns to the pre-undo read;
  - **ping-pong stable** — `undo`/`redo` alternated many times converges (no drift,
    no loss). This is the property the delete→undo→redo→undo tree bug violated.

Edits are fuzzed as a list of opcodes applied left-to-right; ids for move/delete
are resolved against the live tree at apply time (out-of-range picks are no-ops).

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Id as Id exposing (OpId)
import Crdt.Tree as Tree
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)



-- SCHEMA ---------------------------------------------------------------------


type alias NodeItem =
    { label : String }


type alias NodeRefs =
    { label : Ref NodeItem C.Settable String }


nodeDoc : C.RecordRefs NodeItem NodeRefs
nodeDoc =
    C.record NodeItem NodeRefs
        |> C.field "label" .label C.text
        |> C.build


type alias Sample =
    { title : String
    , tags : List String
    , outline : Tree.Forest NodeItem
    }


type alias DocRefs =
    { title : Ref Sample C.Settable String
    , tags : Ref Sample (C.ListK C.Movable C.Settable String) (List String)
    , outline : Ref Sample (C.TreeK C.Nested NodeItem) (Tree.Forest NodeItem)
    }


docDoc : C.RecordRefs Sample DocRefs
docDoc =
    C.record Sample DocRefs
        |> C.field "title" .title C.text
        |> C.field "tags" .tags (C.movableList C.text)
        |> C.field "outline" .outline (C.tree nodeDoc.schema)
        |> C.build


refs : DocRefs
refs =
    docDoc.refs


init : Doc Sample
init =
    C.init (Id.replica "prop") docDoc.schema



-- EDIT MODEL -----------------------------------------------------------------


type Edit
    = SetTitle String
    | AddTag String
    | RemoveTag Int
    | AddRoot String
    | AddChildOfFirst String
    | SetFirstNodeLabel String
    | MoveFirstUnderLast
    | DeleteFirstRoot


{-| Every node id in the tree, in a stable pre-order (roots then children).
-}
allNodeIds : Doc Sample -> List OpId
allNodeIds doc =
    let
        forest =
            C.read doc |> Result.map .outline |> Result.withDefault []

        go items =
            items |> List.concatMap (\i -> Tree.itemId i :: go (Tree.itemChildren i))
    in
    go forest


rootIds : Doc Sample -> List OpId
rootIds doc =
    C.read doc
        |> Result.map (.outline >> List.map Tree.itemId)
        |> Result.withDefault []


{-| Apply one edit as a single tracked (undoable) step. Unresolvable targets (empty
tree, out-of-range index) are no-ops but still recorded — matching real UI where a
click may do nothing yet the user expects undo to skip it (recordEdit of a no-op
records nothing, so the stack stays clean).
-}
applyEdit : Edit -> Doc Sample -> Doc Sample
applyEdit edit doc =
    let
        before =
            Doc.version doc

        ok result =
            Result.withDefault doc result

        edited =
            case edit of
                SetTitle s ->
                    C.set refs.title s doc |> ok

                AddTag s ->
                    C.append C.text s refs.tags doc |> ok

                RemoveTag i ->
                    C.remove i refs.tags doc |> ok

                AddRoot s ->
                    C.addChild nodeDoc.schema (NodeItem s) Nothing refs.outline doc |> ok

                AddChildOfFirst s ->
                    case List.head (rootIds doc) of
                        Just p ->
                            C.addChild nodeDoc.schema (NodeItem s) (Just p) refs.outline doc |> ok

                        Nothing ->
                            doc

                SetFirstNodeLabel s ->
                    -- type into an existing node's text payload (the delete→undo→
                    -- redo class that lost inner text needs this to be fuzzed)
                    case List.head (allNodeIds doc) of
                        Just n ->
                            C.set (refs.outline |> C.treeNode n nodeDoc.schema |> C.at nodeDoc.refs.label) s doc |> ok

                        Nothing ->
                            doc

                MoveFirstUnderLast ->
                    case ( List.head (allNodeIds doc), List.reverse (allNodeIds doc) |> List.head ) of
                        ( Just first, Just last ) ->
                            if first == last then
                                doc

                            else
                                C.moveInto first (Just last) refs.outline doc |> ok

                        _ ->
                            doc

                DeleteFirstRoot ->
                    case List.head (rootIds doc) of
                        Just r ->
                            C.removeNode r refs.outline doc |> ok

                        Nothing ->
                            doc
    in
    Doc.recordEdit before edited


{-| The observable state we compare — the fully decoded document, rendered.
-}
render : Doc Sample -> String
render doc =
    case C.read doc of
        Ok d ->
            d.title ++ " | " ++ String.join "," d.tags ++ " | " ++ shape d.outline

        Err _ ->
            "<err>"


shape : Tree.Forest NodeItem -> String
shape f =
    f
        |> List.map
            (\i ->
                let
                    kids =
                        Tree.itemChildren i
                in
                (Tree.itemValue i |> .label)
                    ++ (if List.isEmpty kids then
                            ""

                        else
                            "[" ++ shape kids ++ "]"
                       )
            )
        |> String.join " "



-- FUZZERS --------------------------------------------------------------------


editFuzz : Fuzzer Edit
editFuzz =
    Fuzz.oneOf
        [ Fuzz.map SetTitle shortStr
        , Fuzz.map AddTag shortStr
        , Fuzz.map RemoveTag (Fuzz.intRange 0 4)
        , Fuzz.map AddRoot shortStr
        , Fuzz.map AddChildOfFirst shortStr
        , Fuzz.map SetFirstNodeLabel shortStr
        , Fuzz.constant MoveFirstUnderLast
        , Fuzz.constant DeleteFirstRoot
        ]


shortStr : Fuzzer String
shortStr =
    Fuzz.oneOfValues [ "a", "b", "c", "d", "x", "y" ]


edits : Fuzzer (List Edit)
edits =
    Fuzz.listOfLengthBetween 0 12 editFuzz


{-| Apply a list of edits. Because a no-op edit records nothing (`recordEdit` of an
unchanged version is a no-op), the number of _undo steps_ equals the number of edits
that actually changed the document, not `List.length es`. We therefore track the
render **after each recorded edit** (plus the initial), so undo-count assertions line
up with the real undo stack rather than with the raw edit list.

Returns `(finalDoc, recordedRenders)` where `recordedRenders` starts with the initial
render and appends one render per edit that advanced the version.

-}
run : List Edit -> ( Doc Sample, List String )
run es =
    List.foldl
        (\e ( doc, renders ) ->
            let
                d1 =
                    applyEdit e doc
            in
            if Doc.version d1 == Doc.version doc then
                -- no-op edit: nothing recorded, no new undo step
                ( d1, renders )

            else
                ( d1, renders ++ [ render d1 ] )
        )
        ( init, [ render init ] )
        es



-- SUITE ----------------------------------------------------------------------


suite : Test
suite =
    describe "undo/redo properties (random edit sequences)"
        [ fuzz edits "undo-all returns to the initial state" <|
            \es ->
                let
                    ( final, _ ) =
                        run es

                    -- undo as many times as there were (recorded) edits + slack
                    fullyUndone =
                        List.foldl (\_ d -> Doc.undo d) final (List.range 1 (List.length es + 2))
                in
                render fullyUndone |> Expect.equal (render init)
        , fuzz edits "undo then redo returns to the pre-undo state" <|
            \es ->
                let
                    ( final, _ ) =
                        run es

                    roundTrip =
                        Doc.redo (Doc.undo final)
                in
                -- if there was at least one recorded edit, undo+redo is identity on
                -- the read; if nothing was recorded, undo/redo are no-ops anyway.
                render roundTrip |> Expect.equal (render final)
        , fuzz edits "undo/redo ping-pong is stable (no drift or loss)" <|
            \es ->
                let
                    ( final, _ ) =
                        run es

                    once =
                        Doc.redo (Doc.undo final)

                    -- alternate several times; must equal a single round-trip
                    many =
                        List.foldl (\_ d -> Doc.redo (Doc.undo d)) final (List.range 1 5)
                in
                render many |> Expect.equal (render once)
        , fuzz edits "step-by-step: undoing the last edit matches the prior render" <|
            \es ->
                let
                    ( final, renders ) =
                        run es

                    -- the render just before the final *recorded* edit (or initial)
                    priorRender =
                        renders
                            |> List.reverse
                            |> List.drop 1
                            |> List.head
                            |> Maybe.withDefault (render init)
                in
                -- `renders` == [init] means no edit actually recorded → nothing to undo
                if List.length renders <= 1 then
                    Expect.pass

                else
                    render (Doc.undo final) |> Expect.equal priorRender
        ]
