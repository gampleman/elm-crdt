module MarkCompactionTests exposing (suite)

{-| **Mark anchors across compaction.**

A Peritext mark anchors its endpoints to _character identities_, and the boundary math
indexes into the **tombstone-inclusive** element order (`Crdt.RichText.Internal.toSpans`)
so a mark whose edge character was deleted still has a stable position. Compaction
(`Doc.compact` at the current version) physically drops tombstones. Those two facts
collide: unless compaction does something about the marks, an anchor can be left
referencing an element that no longer exists, `boundaryPos` returns `Nothing`, and the
mark silently stops covering _all_ of its characters — including the survivors.

`compact` is documented as **equivalence-preserving** on the read model, so that is the
property this module pins, from both ends:

  - `read (compact (version d) d) == read d`, over hand-built boundary cases and over
    random edit scripts (text + marks + block structure);
  - it keeps holding after further edits and merges, in both directions, against a peer
    that never compacted — compaction is a _local_ choice, so a compacted and an
    uncompacted replica must still converge;
  - and it is idempotent and deterministic (two replicas compacting the same state at
    the same cut produce the same bytes).

The suite is deliberately agnostic about _how_ compaction fixes this — retaining the
tombstones a live anchor still references, or re-anchoring the marks onto survivors both
satisfy everything above. The two tests under "reclaims" are the ones that discriminate:
they require the tombstones to actually go away.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.RichText exposing (Block, MarkValue(..), Span)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, fuzz2, test)



-- SCHEMA ---------------------------------------------------------------------


type alias Sample =
    { body : List Span }


type alias DocDoc =
    { body : Ref Sample C.RichK (List Span)
    , schema : C.Schema C.Nested Sample
    }


docDoc : DocDoc
docDoc =
    C.record Sample DocDoc
        |> C.field "body" .body C.richText
        |> C.build


refs : DocDoc
refs =
    docDoc


init : String -> Doc Sample
init name =
    Doc.init (Id.replica name) docDoc.schema


ok : Doc Sample -> Result Edit.EditError (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb



-- EDIT SHORTHANDS ------------------------------------------------------------


text : String -> Doc Sample -> Doc Sample
text s doc =
    Edit.setRich refs.body s doc |> ok doc


mark : Int -> Int -> String -> Doc Sample -> Doc Sample
mark from to type_ doc =
    Edit.mark refs.body from to type_ Flag doc |> ok doc


markValue : Int -> Int -> String -> String -> Doc Sample -> Doc Sample
markValue from to type_ v doc =
    Edit.mark refs.body from to type_ (Value v) doc |> ok doc


unmark : Int -> Int -> String -> Doc Sample -> Doc Sample
unmark from to type_ doc =
    Edit.unmark refs.body from to type_ doc |> ok doc


{-| The documented local-save policy: fold the whole store into `base`, which is the
only cut at which tombstones are physically dropped.
-}
compacted : Doc Sample -> Doc Sample
compacted doc =
    Doc.compact (Doc.version doc) doc



-- RENDERING ------------------------------------------------------------------


{-| `text{mark,mark=value}` per span, joined by `|` — the whole inline read model in
one comparable string.
-}
render : Doc Sample -> String
render doc =
    case Doc.read doc |> Result.map .body of
        Ok spans ->
            spans
                |> List.map
                    (\s ->
                        s.text
                            ++ "{"
                            ++ (Dict.toList s.marks |> List.map pair |> String.join ",")
                            ++ "}"
                    )
                |> String.join "|"

        Err _ ->
            "READ-FAILED"


pair : ( String, MarkValue ) -> String
pair ( name, value ) =
    case value of
        Flag ->
            name

        Value v ->
            name ++ "=" ++ v


{-| The block read model: `type:depth[spans]` per block.
-}
renderBlocks : Doc Sample -> String
renderBlocks doc =
    case Edit.readBlocks refs.body doc of
        Ok bs ->
            bs |> List.map blockLine |> String.join "//"

        Err _ ->
            "BLOCKS-FAILED"


blockLine : Block -> String
blockLine b =
    b.type_
        ++ ":"
        ++ String.fromInt b.depth
        ++ "["
        ++ (b.spans
                |> List.map (\s -> s.text ++ "{" ++ (Dict.toList s.marks |> List.map pair |> String.join ",") ++ "}")
                |> String.join "|"
           )
        ++ "]"


json : Doc Sample -> String
json doc =
    JE.encode 0 (Doc.encode doc)


{-| How many RGA tombstones the encoded document still carries (`"d":true` per
`Crdt.Json`'s element codec).
-}
tombstones : Doc Sample -> Int
tombstones doc =
    List.length (String.split "\"d\":true" (json doc)) - 1



-- THE CENTRAL ASSERTION ------------------------------------------------------


{-| Compaction preserves the whole read model (inline spans _and_ blocks), leaves the
cache consistent with `materialize base store`, and is idempotent.
-}
preservesRead : Doc Sample -> Expectation
preservesRead doc =
    let
        once =
            compacted doc

        twice =
            compacted once
    in
    Expect.all
        [ \_ -> Expect.equal (render doc) (render once)
        , \_ -> Expect.equal (renderBlocks doc) (renderBlocks once)
        , \_ -> Expect.equal True (Doc.cacheConsistent once)
        , \_ -> Expect.equal (render once) (render twice)
        ]
        ()



-- SYNC HELPERS ---------------------------------------------------------------


{-| A second replica on the same lineage, over the wire.
-}
forkOf : String -> Doc Sample -> Doc Sample
forkOf name doc =
    Doc.decodeInto (Doc.encode doc) (init name) |> Result.withDefault (init name)


{-| Merge both ways and assert the two replicas read identically — the convergence
check that matters when only one side compacted.
-}
converges : Doc Sample -> Doc Sample -> Expectation
converges a b =
    Expect.all
        [ \_ -> Expect.equal (render (Doc.merge a b)) (render (Doc.merge b a))
        , \_ -> Expect.equal (renderBlocks (Doc.merge a b)) (renderBlocks (Doc.merge b a))
        , \_ -> Expect.equal True (Doc.cacheConsistent (Doc.merge a b))
        ]
        ()



-- FIXTURES -------------------------------------------------------------------


{-| "hello world" with `bold` over "hello".
-}
boldHello : Doc Sample
boldHello =
    init "a" |> text "hello world" |> mark 0 5 "bold"



-- TESTS ----------------------------------------------------------------------


suite : Test
suite =
    describe "mark anchors across compaction"
        [ describe "coverage is preserved when an anchor character is deleted"
            [ test "start anchor deleted (first char of the range)" <|
                \_ ->
                    -- "hello world" bold [0,5) then drop the leading 'h': the start
                    -- anchor now points at a tombstone.
                    preservesRead (boldHello |> text "ello world")
            , test "end anchor deleted (last char of the range)" <|
                \_ ->
                    preservesRead (boldHello |> text "hell world")
            , test "both anchors deleted" <|
                \_ ->
                    preservesRead (boldHello |> text "ell world")
            , test "interior character deleted (anchors intact — control)" <|
                \_ ->
                    preservesRead (boldHello |> text "helo world")
            , test "character before the range deleted" <|
                \_ ->
                    preservesRead (init "a" |> text "xhello" |> mark 1 6 "bold" |> text "hello")
            , test "whole marked range deleted (orphaned mark)" <|
                \_ ->
                    preservesRead (boldHello |> text " world")
            , test "marked range deleted, then new text typed where it was" <|
                \_ ->
                    -- the classic resurrection trap: the new characters must NOT
                    -- inherit the dead mark.
                    preservesRead (boldHello |> text " world" |> text "again world")
            , test "value mark (link) with a deleted edge" <|
                \_ ->
                    preservesRead
                        (init "a"
                            |> text "click here now"
                            |> markValue 6 10 "link" "x.com"
                            |> text "click ere now"
                        )
            , test "an unmark hole survives — bold must not come back" <|
                \_ ->
                    -- bold over the lot, cleared in the middle, then both the outer
                    -- mark's edge AND the clear op's edge deleted.
                    preservesRead
                        (init "a"
                            |> text "abcdefghij"
                            |> mark 0 10 "bold"
                            |> unmark 3 6 "bold"
                            |> text "bcdefghij"
                            |> text "bcefghij"
                        )
            , test "overlapping same-prefix marks (two comment threads)" <|
                \_ ->
                    -- the `comment:<id>` pattern: distinct keys so the ranges can
                    -- overlap. Each loses one edge character.
                    preservesRead
                        (init "a"
                            |> text "the quick brown fox"
                            |> mark 0 9 "comment:alice"
                            |> mark 4 15 "comment:bob"
                            |> text "he quick brown fox"
                            |> text "he quick brow fox"
                        )
            , test "many marks, many deletions" <|
                \_ ->
                    preservesRead
                        (init "a"
                            |> text "abcdefghijklmnop"
                            |> mark 0 4 "bold"
                            |> mark 3 8 "italic"
                            |> mark 7 12 "comment:x"
                            |> markValue 10 16 "link" "y.com"
                            |> text "bcdefghijklmno"
                            |> text "bcefghijklno"
                        )
            ]
        , describe "block structure"
            [ test "block types survive a compaction after boundary deletes" <|
                \_ ->
                    preservesRead
                        (init "a"
                            |> text "first second"
                            |> split 0 5
                            |> blockType 0 "h1"
                            |> blockType 1 "blockquote"
                            |> mark 0 4 "bold"
                            |> text "irst second"
                        )
            , test "a merged-away block's marker is gone from both sides" <|
                \_ ->
                    preservesRead
                        (init "a"
                            |> text "first second"
                            |> split 0 5
                            |> blockType 1 "h2"
                            |> mark 0 3 "bold"
                            |> mergeBlock 1
                            |> text "irst second"
                        )
            , test "indent survives" <|
                \_ ->
                    preservesRead
                        (init "a"
                            |> text "one two"
                            |> split 0 3
                            |> blockType 1 "ul"
                            |> indent 1
                            |> mark 0 3 "bold"
                            |> text "ne two"
                        )
            ]
        , describe "cut position"
            [ test "a mid-history cut preserves the read (tombstones are kept there)" <|
                \_ ->
                    let
                        doc =
                            boldHello

                        mid =
                            Doc.version doc

                        later =
                            doc |> text "ello world"
                    in
                    Expect.equal (render later) (render (Doc.compact mid later))
            , test "compacting twice is the same as once, down to the bytes" <|
                \_ ->
                    let
                        once =
                            compacted (boldHello |> text "ello world")
                    in
                    Expect.equal (json once) (json (compacted once))
            , test "two replicas compacting the same state agree byte for byte" <|
                \_ ->
                    let
                        shared =
                            boldHello |> text "ello world"

                        a =
                            compacted shared

                        b =
                            compacted (forkOf "b" shared)
                    in
                    -- same read; the encodings differ only in replica bookkeeping, so
                    -- compare the read model plus the op/tombstone counts.
                    Expect.all
                        [ \_ -> Expect.equal (render a) (render b)
                        , \_ -> Expect.equal (Doc.opCount a) (Doc.opCount b)
                        , \_ -> Expect.equal (String.length (json a)) (String.length (json b))
                        ]
                        ()
            ]
        , describe "convergence with a peer that did not compact"
            [ test "compacted replica vs uncompacted fork, no further edits" <|
                \_ ->
                    let
                        shared =
                            boldHello |> text "ello world"
                    in
                    converges (compacted shared) (forkOf "b" shared)
            , test "the uncompacted peer marks surviving text afterwards" <|
                \_ ->
                    let
                        shared =
                            boldHello |> text "ello world"

                        b =
                            forkOf "b" shared |> mark 5 10 "italic"
                    in
                    Expect.all
                        [ \_ -> converges (compacted shared) b
                        , \_ ->
                            -- and the merged result agrees with the never-compacted one
                            Expect.equal
                                (render (Doc.merge (forkOf "c" shared) b))
                                (render (Doc.merge (compacted shared) b))
                        ]
                        ()
            , test "the compacted replica keeps editing and the peer catches up" <|
                \_ ->
                    let
                        shared =
                            boldHello |> text "ello world"

                        a =
                            compacted shared |> text "ello there" |> mark 5 10 "italic"
                    in
                    converges a (forkOf "b" shared)
            , test "both sides edit the marked range concurrently" <|
                \_ ->
                    let
                        shared =
                            boldHello

                        a =
                            compacted (shared |> text "ello world")

                        b =
                            forkOf "b" shared |> mark 2 8 "italic"
                    in
                    converges a b
            ]
        , describe "reclaims"
            [ test "an orphaned mark's tombstones do not survive compaction" <|
                \_ ->
                    -- Every marked character is gone, so nothing can ever anchor the
                    -- mark again: compaction must not be forced to retain the
                    -- tombstones (or the mark) to keep the read model right.
                    let
                        doc =
                            compacted (boldHello |> mark 0 11 "comment:alice" |> text "")
                    in
                    Expect.all
                        [ \_ -> Expect.equal "" (render doc)
                        , \_ -> Expect.equal False (String.contains "comment:alice" (json doc))
                        , \_ -> Expect.equal False (String.contains "bold" (json doc))
                        ]
                        ()
            , test "no tombstone survives a full compaction, marks or no marks" <|
                \_ ->
                    -- The discriminating test: keeping the tombstones a live anchor
                    -- still references would also preserve the read model, but it
                    -- would leave them here.
                    let
                        doc =
                            init "a"
                                |> text "the quick brown fox jumps over the lazy dog"
                                |> mark 0 20 "bold"
                                |> mark 10 30 "italic"
                                |> markValue 5 25 "link" "v.com"
                                |> text "quick brown fox jumps over the lazy dog"
                                |> text "brown fox jumps over the lazy dog"
                                |> text "fox jumps over the lazy dog"
                    in
                    Expect.all
                        [ \_ -> Expect.equal 0 (tombstones (compacted doc))
                        , \_ ->
                            Expect.equal
                                "fox {bold,italic,link=v.com}|jumps{italic,link=v.com}| over{italic}| the lazy dog{}"
                                (render doc)
                        , \_ -> Expect.equal (render doc) (render (compacted doc))
                        ]
                        ()
            ]
        , describe "properties over random edit scripts"
            [ fuzz (Fuzz.list editFuzz) "read is unchanged by compaction" <|
                \es ->
                    preservesRead (runFrom (init "a") es)
            , fuzz (Fuzz.list editFuzz) "a compacted replica still converges with an uncompacted fork" <|
                \es ->
                    let
                        shared =
                            runFrom (init "a") es
                    in
                    converges (compacted shared) (forkOf "b" shared)
            , fuzz2 (Fuzz.list editFuzz)
                (Fuzz.list editFuzz)
                "compact, then keep editing on both sides"
              <|
                \before after ->
                    let
                        shared =
                            runFrom (init "a") before

                        a =
                            runFrom (compacted shared) after

                        b =
                            runFrom (forkOf "b" shared) after
                    in
                    converges a b
            ]
        ]



-- BLOCK EDIT SHORTHANDS ------------------------------------------------------


split : Int -> Int -> Doc Sample -> Doc Sample
split bi off doc =
    Edit.splitBlock refs.body bi off doc |> ok doc


mergeBlock : Int -> Doc Sample -> Doc Sample
mergeBlock bi doc =
    Edit.mergeBlock refs.body bi doc |> ok doc


blockType : Int -> String -> Doc Sample -> Doc Sample
blockType bi t doc =
    Edit.setBlockType refs.body bi (Just t) doc |> ok doc


indent : Int -> Doc Sample -> Doc Sample
indent bi doc =
    Edit.indentBlock refs.body bi doc |> ok doc



-- FUZZED EDIT SCRIPTS --------------------------------------------------------


{-| The edit vocabulary a rich-text editor actually produces. Indices are resolved
against the _current_ document so a fuzzed value is always in range.
-}
type Op
    = SetText String
    | Mark Int Int String
    | MarkValue Int Int String String
    | Unmark Int Int String
    | Split Int Int
    | Merge Int
    | SetType Int String
    | Indent Int


editFuzz : Fuzzer Op
editFuzz =
    Fuzz.oneOf
        [ Fuzz.map SetText (Fuzz.oneOfValues [ "", "a", "abc", "hello world", "hello", "ello world", "xyzzy plugh" ])
        , Fuzz.map3 Mark smallIndex smallIndex markName
        , Fuzz.map3 (\f t n -> MarkValue f t n "v.com") smallIndex smallIndex (Fuzz.oneOfValues [ "link" ])
        , Fuzz.map3 Unmark smallIndex smallIndex markName
        , Fuzz.map2 Split smallIndex smallIndex
        , Fuzz.map Merge smallIndex
        , Fuzz.map2 SetType smallIndex (Fuzz.oneOfValues [ "h1", "ul", "blockquote" ])
        , Fuzz.map Indent smallIndex
        ]


markName : Fuzzer String
markName =
    Fuzz.oneOfValues [ "bold", "italic", "comment:alice", "comment:bob" ]


smallIndex : Fuzzer Int
smallIndex =
    Fuzz.intRange 0 12


plainLength : Doc Sample -> Int
plainLength doc =
    Doc.read doc
        |> Result.map (.body >> List.map (.text >> String.length) >> List.sum)
        |> Result.withDefault 0


blockCount : Doc Sample -> Int
blockCount doc =
    Edit.readBlocks refs.body doc |> Result.map List.length |> Result.withDefault 1


applyOp : Op -> Doc Sample -> Doc Sample
applyOp op doc =
    let
        len =
            plainLength doc

        clampBlock i =
            modBy (max 1 (blockCount doc)) (abs i)

        range from to =
            -- a non-empty, in-range [from, to) or Nothing when there is no text
            if len == 0 then
                Nothing

            else
                let
                    f =
                        modBy len (abs from)

                    t =
                        modBy (len + 1) (abs to)
                in
                if f < t then
                    Just ( f, t )

                else if t < f then
                    Just ( t, f )

                else
                    Nothing
    in
    case op of
        SetText s ->
            text s doc

        Mark from to name ->
            case range from to of
                Just ( f, t ) ->
                    mark f t name doc

                Nothing ->
                    doc

        MarkValue from to name v ->
            case range from to of
                Just ( f, t ) ->
                    markValue f t name v doc

                Nothing ->
                    doc

        Unmark from to name ->
            case range from to of
                Just ( f, t ) ->
                    unmark f t name doc

                Nothing ->
                    doc

        Split bi off ->
            split (clampBlock bi) (modBy (max 1 (len + 1)) (abs off)) doc

        Merge bi ->
            mergeBlock (clampBlock bi) doc

        SetType bi t ->
            blockType (clampBlock bi) t doc

        Indent bi ->
            indent (clampBlock bi) doc


runFrom : Doc Sample -> List Op -> Doc Sample
runFrom start =
    List.foldl applyOp start
