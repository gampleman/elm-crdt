module BlockConvergenceTests exposing (suite)

{-| Convergence properties for **concurrent block edits**. Two replicas start from a
shared base document, each applies its own random sequence of block operations
(split / merge / setType / indent / outdent / setBlockText), then exchange full op sets
and merge. The properties:

  - **order-independence** — merging A-into-B reads identically to merging B-into-A
    (the CRDT convergence guarantee, at the block layer);
  - **idempotence** — merging a peer's state in twice reads the same as once.

Concurrent block edits are just `InsertElem`/`DeleteElem`/`AddMark` on the shared Fugue
sequence, so this exercises whether the block _read model_ (`toBlocks`) stays a
deterministic function of the converged element set no matter how the markers, nest
tokens, and characters interleave. Sync is the real op-log wire (`encode`/`decodeInto`),
as the demo uses it.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.RichText exposing (Block, Span)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz)



-- SCHEMA ---------------------------------------------------------------------


type alias Sample =
    { body : List Span }


type alias DocRefs =
    { body : Ref Sample C.RichK (List Span) }


docDoc : C.RecordRefs Sample DocRefs
docDoc =
    C.record Sample DocRefs
        |> C.field "body" .body C.richText
        |> C.build


refs : DocRefs
refs =
    docDoc.refs


bodyPath : Path
bodyPath =
    Path.root |> Path.field "body"


init : String -> Doc Sample
init name =
    Doc.init (Id.replica name) docDoc.schema


ok : Doc Sample -> Result C.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


blocks : Doc Sample -> List Block
blocks doc =
    Doc.readBlocks bodyPath doc |> Result.withDefault []


{-| The observable state we compare: type:depth"text" per block, joined by `|`.
-}
render : Doc Sample -> String
render doc =
    blocks doc
        |> List.map
            (\b ->
                b.type_
                    ++ ":"
                    ++ String.fromInt b.depth
                    ++ "\""
                    ++ (b.spans |> List.map .text |> String.concat)
                    ++ "\""
            )
        |> String.join " | "


{-| Merge `from`'s full op set into `to` (the op-log wire, as the demo syncs).
-}
mergeIn : Doc Sample -> Doc Sample -> Doc Sample
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to



-- EDIT MODEL -----------------------------------------------------------------


type Edit
    = SplitAt Int Int
    | MergeAt Int
    | SetType Int String
    | Indent Int
    | Outdent Int
    | SetBlockText Int String


{-| Apply one edit, resolving its block index against the current block count so a
fuzzed index is always in range.
-}
applyEdit : Edit -> Doc Sample -> Doc Sample
applyEdit edit doc =
    let
        n =
            List.length (blocks doc)

        clampIndex i =
            if n <= 0 then
                0

            else
                modBy n (abs i)

        blockLen i =
            blocks doc
                |> List.drop i
                |> List.head
                |> Maybe.map (\b -> b.spans |> List.map (.text >> String.length) |> List.sum)
                |> Maybe.withDefault 0
    in
    case edit of
        SplitAt i off ->
            let
                bi =
                    clampIndex i

                charOffset =
                    if blockLen bi <= 0 then
                        0

                    else
                        modBy (blockLen bi + 1) (abs off)
            in
            C.splitBlock refs.body bi charOffset doc |> ok doc

        MergeAt i ->
            C.mergeBlock refs.body (clampIndex i) doc |> ok doc

        SetType i t ->
            C.setBlockType refs.body
                (clampIndex i)
                (if t == "" then
                    Nothing

                 else
                    Just t
                )
                doc
                |> ok doc

        Indent i ->
            C.indentBlock refs.body (clampIndex i) doc |> ok doc

        Outdent i ->
            C.outdentBlock refs.body (clampIndex i) doc |> ok doc

        SetBlockText i s ->
            C.setBlockText refs.body (clampIndex i) s doc |> ok doc


runFrom : Doc Sample -> List Edit -> Doc Sample
runFrom =
    List.foldl applyEdit


{-| A shared starting document (a few blocks with text), synced to both replicas so
their concurrent edits build on a common base.
-}
sharedBase : Doc Sample
sharedBase =
    C.setRich refs.body "alphabravocharlie" (init "seed")
        |> ok (init "seed")
        |> (\d -> C.splitBlock refs.body 0 5 d |> ok d)
        |> (\d -> C.splitBlock refs.body 1 5 d |> ok d)



-- FUZZERS --------------------------------------------------------------------


shortStr : Fuzzer String
shortStr =
    Fuzz.oneOfValues [ "", "a", "ab", "zz" ]


typeStr : Fuzzer String
typeStr =
    Fuzz.oneOfValues [ "", "h1", "blockquote", "ul" ]


smallIndex : Fuzzer Int
smallIndex =
    Fuzz.intRange 0 5


editFuzz : Fuzzer Edit
editFuzz =
    Fuzz.oneOf
        [ Fuzz.map2 SplitAt smallIndex (Fuzz.intRange 0 8)
        , Fuzz.map MergeAt smallIndex
        , Fuzz.map2 SetType smallIndex typeStr
        , Fuzz.map Indent smallIndex
        , Fuzz.map Outdent smallIndex
        , Fuzz.map2 SetBlockText smallIndex shortStr
        ]


edits : Fuzzer (List Edit)
edits =
    Fuzz.listOfLengthBetween 0 8 editFuzz



-- SUITE ----------------------------------------------------------------------


suite : Test
suite =
    describe "block-edit convergence (concurrent random op sequences)"
        [ fuzz2 edits edits "merge order-independence: A◁B reads == B◁A" <|
            \aEdits bEdits ->
                let
                    -- both replicas start from the shared base
                    baseA =
                        mergeIn sharedBase (init "alice")

                    baseB =
                        mergeIn sharedBase (init "bob")

                    a =
                        runFrom baseA aEdits

                    b =
                        runFrom baseB bEdits

                    ab =
                        mergeIn b a

                    ba =
                        mergeIn a b
                in
                render ab |> Expect.equal (render ba)
        , fuzz2 edits edits "merge idempotence: merging a peer twice == once" <|
            \aEdits bEdits ->
                let
                    baseA =
                        mergeIn sharedBase (init "alice")

                    baseB =
                        mergeIn sharedBase (init "bob")

                    a =
                        runFrom baseA aEdits

                    b =
                        runFrom baseB bEdits

                    once =
                        mergeIn b a

                    twice =
                        mergeIn b once
                in
                render twice |> Expect.equal (render once)
        , fuzz edits "a peer's edits sync cleanly onto an unmodified replica" <|
            \aEdits ->
                let
                    baseA =
                        mergeIn sharedBase (init "alice")

                    a =
                        runFrom baseA aEdits

                    -- a fresh replica that only receives alice's state
                    bob =
                        mergeIn a (init "bob")
                in
                render bob |> Expect.equal (render a)
        ]


{-| Local `fuzz2` via `Fuzz.map2`, to avoid depending on the Test.fuzz2 export.
-}
fuzz2 : Fuzzer a -> Fuzzer b -> String -> (a -> b -> Expect.Expectation) -> Test
fuzz2 fa fb desc f =
    fuzz (Fuzz.map2 Tuple.pair fa fb) desc (\( a, b ) -> f a b)
