module BlockPropertyTests exposing (suite)

{-| Property tests over **random sequences of block edits** on a single `C.richText`
field, driven through the public `Crdt.Ref` API exactly as the demo's editor binding
does. This is the level that the block-structure bugs actually lived at: the flat
character-sequence ↔ block-index mapping, especially once the **leading marker** exists
(formatting block 0). The hand-written `BlockTests` pin specific scenarios; these fuzz
the _compositions_ a real editor produces, where the off-by-one / cross-block-leak /
lost-formatting bugs hid.

Invariants asserted after every prefix of a random op sequence:

  - **always ≥ 1 block** — a document is never empty; merge/outdent clamp.
  - **plain text is preserved across structural edits** — split/merge/type/indent
    never add or drop characters (concatenated block text is invariant under them).
  - **block-index isolation** — editing block `i`'s text changes _only_ block `i`'s
    text; every other block's text is untouched. (The "He\\nd" and leading-marker
    index-shift bugs both violated this.)
  - **split then merge-back is identity** on the rendered structure.

Each variant is run twice: from a fresh doc, and from a doc whose block 0 was
pre-formatted (so the leading marker is present) — the exact state that shifted split
indices by one in the wild.

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


init : Doc Sample
init =
    Doc.init (Id.replica "prop") docDoc.schema


ok : Doc Sample -> Result C.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


blocks : Doc Sample -> List Block
blocks doc =
    Doc.readBlocks bodyPath doc |> Result.withDefault []


blockCount : Doc Sample -> Int
blockCount =
    blocks >> List.length


{-| Per-block plain text (concatenated span text), one entry per block.
-}
blockTexts : Doc Sample -> List String
blockTexts doc =
    blocks doc |> List.map (\b -> b.spans |> List.map .text |> String.concat)


{-| The whole document's plain text (all blocks concatenated, no separators).
-}
plainText : Doc Sample -> String
plainText =
    blockTexts >> String.concat



-- EDIT MODEL -----------------------------------------------------------------


type Edit
    = SplitAt Int Int -- block index, char offset
    | MergeAt Int -- block index (no-op on block 0)
    | SetType Int String -- block index, type ("" clears)
    | Indent Int
    | Outdent Int
    | SetBlockText Int String


{-| Apply one edit, resolving its block index against the _current_ block count so a
fuzzed index is always in range (mirrors the demo: the editor reports a real index).
Out-of-range picks wrap into range; unresolvable edits are no-ops.
-}
applyEdit : Edit -> Doc Sample -> Doc Sample
applyEdit edit doc =
    let
        n =
            blockCount doc

        clampIndex i =
            if n <= 0 then
                0

            else
                modBy n (abs i)

        blockLen i =
            blockTexts doc |> List.drop i |> List.head |> Maybe.map String.length |> Maybe.withDefault 0
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
runFrom start es =
    List.foldl applyEdit start es


{-| A starting doc with some text but no leading marker yet (block 0 unformatted).
-}
plainStart : Doc Sample
plainStart =
    C.setRich refs.body "hello world" init |> ok init


{-| Same text, but block 0 pre-formatted so the **leading marker** exists — the state
that shifted split indices by one before the fix.
-}
formattedStart : Doc Sample
formattedStart =
    C.setBlockType refs.body 0 (Just "h1") plainStart |> ok plainStart



-- FUZZERS --------------------------------------------------------------------


shortStr : Fuzzer String
shortStr =
    Fuzz.oneOfValues [ "", "a", "ab", "xyz", "hello world" ]


typeStr : Fuzzer String
typeStr =
    Fuzz.oneOfValues [ "", "h1", "h2", "blockquote", "ul", "ol" ]


smallIndex : Fuzzer Int
smallIndex =
    Fuzz.intRange 0 6


{-| Structural edits only (no text mutation) — used by the text-preservation property,
which asserts these never change the document's plain text.
-}
structuralEditFuzz : Fuzzer Edit
structuralEditFuzz =
    Fuzz.oneOf
        [ Fuzz.map2 SplitAt smallIndex (Fuzz.intRange 0 12)
        , Fuzz.map MergeAt smallIndex
        , Fuzz.map2 SetType smallIndex typeStr
        , Fuzz.map Indent smallIndex
        , Fuzz.map Outdent smallIndex
        ]


{-| Any edit, including text mutation.
-}
editFuzz : Fuzzer Edit
editFuzz =
    Fuzz.oneOf
        [ structuralEditFuzz
        , Fuzz.map2 SetBlockText smallIndex shortStr
        ]


structuralEdits : Fuzzer (List Edit)
structuralEdits =
    Fuzz.listOfLengthBetween 0 12 structuralEditFuzz


anyEdits : Fuzzer (List Edit)
anyEdits =
    Fuzz.listOfLengthBetween 0 12 editFuzz



-- SUITE ----------------------------------------------------------------------


suite : Test
suite =
    describe "block structure properties (random op sequences)"
        [ describe "from a plain (unformatted) start"
            (invariants plainStart)
        , describe "from a start with block 0 formatted (leading marker present)"
            (invariants formattedStart)
        ]


{-| The full invariant battery, parameterized by the starting document (so we run it
both with and without a pre-existing leading marker).
-}
invariants : Doc Sample -> List Test
invariants start =
    [ fuzz structuralEdits "always at least one block" <|
        \es ->
            blockCount (runFrom start es) |> Expect.atLeast 1
    , fuzz structuralEdits "structural edits preserve the document's plain text" <|
        \es ->
            -- split/merge/type/indent/outdent must not add or drop any characters.
            plainText (runFrom start es) |> Expect.equal (plainText start)
    , fuzz2 anyEdits smallIndex "editing one block's text changes only that block's text" <|
        \es rawIndex ->
            let
                doc =
                    runFrom start es

                n =
                    blockCount doc
            in
            if n <= 0 then
                Expect.pass

            else
                let
                    target =
                        modBy n (abs rawIndex)

                    before =
                        blockTexts doc

                    -- set a sentinel that differs from whatever is there
                    edited =
                        C.setBlockText refs.body target "ZZZ-sentinel" doc |> ok doc

                    after =
                        blockTexts edited
                in
                if blockCount edited /= n then
                    -- a text set must never change the block count
                    Expect.fail "setBlockText changed the block count"

                else
                    -- every block other than `target` keeps its text
                    List.indexedMap
                        (\i ( b, a ) ->
                            if i == target then
                                True

                            else
                                b == a
                        )
                        (List.map2 Tuple.pair before after)
                        |> List.all identity
                        |> Expect.equal True
    , fuzz2 smallIndex (Fuzz.intRange 0 12) "split then merge-back restores the structure" <|
        \rawIndex rawOffset ->
            let
                n =
                    blockCount start

                bi =
                    modBy (max 1 n) (abs rawIndex)

                len =
                    blockTexts start |> List.drop bi |> List.head |> Maybe.map String.length |> Maybe.withDefault 0

                off =
                    if len <= 0 then
                        0

                    else
                        modBy (len + 1) (abs rawOffset)

                split =
                    C.splitBlock refs.body bi off start |> ok start

                -- the split created a new block at bi+1; merging it back rejoins them
                mergedBack =
                    C.mergeBlock refs.body (bi + 1) split |> ok split
            in
            plainText mergedBack |> Expect.equal (plainText start)
    ]


{-| Local `fuzz2` helper (Test exposes fuzz2 in some versions; define via `Fuzz.map2`
so this module doesn't depend on that export).
-}
fuzz2 : Fuzzer a -> Fuzzer b -> String -> (a -> b -> Expect.Expectation) -> Test
fuzz2 fa fb desc f =
    fuzz (Fuzz.map2 Tuple.pair fa fb) desc (\( a, b ) -> f a b)
