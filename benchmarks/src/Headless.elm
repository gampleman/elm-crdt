port module Headless exposing (main)

{-| Headless **memory/size/latency** harness, driven from Node (`run-mem.js`,
`run-merge.js`, `run-wire.js`, `run-bench.js`).

For each requested workload it builds a document at scale `n` and reports structural
proxies for its footprint — the number of ops in the log, and the encoded byte size
(overall, and split by container). These proxies are deterministic (unlike a
GC-dependent heap reading) and attribute cost to a *structure*, so we can see which
one dominates. The runners additionally bracket the calls with timing /
`process.memoryUsage()`.

Workloads:

  - `"demo"` — a realistic demo-like doc: todos + a dict of rich-text files + an
    outline tree + settings (title/status/likes), scaled by `n`.
  - `"text"` — one rich-text file with `n` typed characters (the hypothesized
    memory hog: one `Node` per character).
  - `"typing"` — the same ~n characters typed **one keystroke at a time** (one `set` per
    char), so op count = chars. The interactive-typing contrast to `"text"` (whole-value
    set = one run-length op): shows where run-length ops do NOT cut op count.
  - `"list"` — a movable list of `n` small records.
  - `"churn"` — a movable list of `n`, then `n` pseudo-random `move`s (the
    arbitrary-position edit path; a single move is O(n), a batch is O(n²)).
  - `"dict"` — a dict of `n` short text entries.
  - `"tree"` — an outline tree of `n` nodes.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.RichText exposing (Span)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Json.Encode as JE



-- SCHEMA (mirrors the demo) --------------------------------------------------


type alias Board =
    { title : String
    , todos : List Todo
    , files : Dict String (List Span)
    , outline : Tree.Forest ONode
    , likes : Int
    , tags : List String
    }


type alias Todo =
    { text : String, done : Bool }


type alias ONode =
    { text : String }


type alias BoardRefs =
    { title : Ref Board C.Settable String
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (C.TreeK C.Nested ONode) (Tree.Forest ONode)
    , likes : Ref Board C.Counter Int
    , tags : Ref Board (C.ListK C.Fixed C.Settable String) (List String)
    }


boardDoc : C.RecordRefs Board BoardRefs
boardDoc =
    C.record Board BoardRefs
        |> C.field "title" .title C.text
        |> C.field "todos" .todos (C.movableList todoDoc.schema)
        |> C.field "files" .files (C.dict C.richText)
        |> C.field "outline" .outline (C.tree onodeDoc.schema)
        |> C.field "likes" .likes C.counter
        |> C.field "tags" .tags (C.list C.text)
        |> C.build


refs : BoardRefs
refs =
    boardDoc.refs


type alias TodoRefs =
    { text : Ref Todo C.Settable String, done : Ref Todo C.Settable Bool }


todoDoc : C.RecordRefs Todo TodoRefs
todoDoc =
    C.record Todo TodoRefs
        |> C.field "text" .text C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias ONodeRefs =
    { text : Ref ONode C.Settable String }


onodeDoc : C.RecordRefs ONode ONodeRefs
onodeDoc =
    C.record ONode ONodeRefs
        |> C.field "text" .text C.text
        |> C.build


init : Doc Board
init =
    C.init (Id.replica "bench") boardDoc.schema


ok : Doc Board -> Result C.EditError (Doc Board) -> Doc Board
ok fb =
    Result.withDefault fb



-- WORKLOADS ------------------------------------------------------------------


{-| Type `s` into a fresh file `name` via the demo's editor path (setRich diffs into
per-character insert ops), so the char-node cost is realistic.
-}
typeFile : String -> String -> Doc Board -> Doc Board
typeFile name s doc =
    let
        d1 =
            C.setKey refs.files C.richText name [] doc |> ok doc

        fileRef =
            refs.files |> C.key name C.richText
    in
    C.setRich fileRef s d1 |> ok d1


{-| A short pseudo-random-ish word from an index, so text isn't degenerate.
-}
word : Int -> String
word i =
    let
        base =
            [ "the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "and", "then" ]
    in
    List.drop (modBy 10 i) base |> List.head |> Maybe.withDefault "x"


sentence : Int -> Int -> String
sentence seed count =
    List.range 0 (count - 1)
        |> List.map (\i -> word (seed + i))
        |> String.join " "


buildText : Int -> Doc Board
buildText n =
    -- one file with ~n characters typed IN ONE SHOT (paste / load / set-whole-value):
    -- a single `setRich` diffs to one contiguous insert → ONE run-length op.
    typeFile "doc.md" (sentence 0 (max 1 (n // 5))) init


{-| The INTERACTIVE-typing workload: type `n` characters **one keystroke at a time**, each
a separate `C.set` on the title with the growing prefix (exactly what the demo's `onInput`
does per keystroke). Each keystroke is a 1-char diff → a 1-char run → still ONE op per
character, so this is the case where run-length ops do NOT reduce op count (they only make
each op a bit smaller). Contrast `buildText`, which sets the whole value at once.
-}
buildTyping : Int -> Doc Board
buildTyping n =
    let
        full =
            sentence 0 (max 1 (n // 5))

        chars =
            String.length full
    in
    List.range 1 chars
        |> List.foldl (\i doc -> C.set refs.title (String.left i full) doc |> ok doc) init


buildList : Int -> Doc Board
buildList n =
    List.range 1 n
        |> List.foldl
            (\i doc -> C.append refs.todos todoDoc.schema (Todo (word i) False) doc |> ok doc)
            init


{-| Random-index churn: build a movable list of `n` records, then perform `n`
pseudo-random `move`s. Each `move` resolves two visible indices to element ids and one
insert-anchor — the arbitrary-position edit path that (before an order index) walks the
whole RGA per op, so this is the workload that surfaces the O(n²) index-access tail.
The moves keep the list at constant size, so only the per-op index cost scales with `n`.
-}
buildChurn : Int -> Doc Board
buildChurn n =
    let
        list0 =
            buildList n

        -- a cheap deterministic LCG so the from/to indices roam the whole list
        step s =
            modBy 2147483647 (s * 1103515245 + 12345)
    in
    List.range 1 n
        |> List.foldl
            (\_ ( doc, s ) ->
                let
                    s1 =
                        step s

                    s2 =
                        step s1

                    from =
                        modBy n s1

                    to =
                        modBy n s2
                in
                ( C.move refs.todos from to doc |> ok doc, s2 )
            )
            ( list0, 1 )
        |> Tuple.first


buildFlat : Int -> Doc Board
buildFlat n =
    List.range 1 n
        |> List.foldl
            (\i doc -> C.append refs.tags C.text (word i) doc |> ok doc)
            init


{-| No-CRDT baseline: N record-appends onto a plain immutable Elm `Board` (no `Doc`).
Each append rebuilds the record and prepends to `.todos` — the idiomatic immutable Elm
way, and a fair "cost of adding CRDTs" floor (immutable, unlike plain JS's mutable
arrays). Returns a checksum so the build can't be elided.
-}
plainListBuild : Int -> Int
plainListBuild n =
    List.range 1 n
        |> List.foldl
            (\i acc -> { acc | todos = { text = word i, done = False } :: acc.todos })
            { title = "", todos = [], files = Dict.empty, outline = [], likes = 0, tags = [] }
        |> (\b -> List.length b.todos)


{-| No-CRDT baseline: N single-char appends onto a plain immutable Elm `String` field.
-}
plainTextBuild : Int -> Int
plainTextBuild n =
    List.range 1 n
        |> List.foldl
            (\i acc -> { acc | title = acc.title ++ String.left 1 (word (modBy 10 i)) })
            { title = "", todos = [], files = Dict.empty, outline = [], likes = 0, tags = [] }
        |> (\b -> String.length b.title)


buildDict : Int -> Doc Board
buildDict n =
    List.range 1 n
        |> List.foldl
            (\i doc -> C.setKey refs.files C.richText ("k" ++ String.fromInt i) [] doc |> ok doc)
            init


buildTree : Int -> Doc Board
buildTree n =
    List.range 1 n
        |> List.foldl
            (\i doc -> C.addChild refs.outline onodeDoc.schema (ONode (word i)) Nothing doc |> ok doc)
            init


{-| The realistic demo-like doc, scaled by `n`: `n` todos, `n/10` files each with a
short sentence, `n` outline nodes, a title, and some likes.
-}
buildDemo : Int -> Doc Board
buildDemo n =
    let
        withTitle d =
            C.set refs.title "Benchmark board" d |> ok d

        withTodos d =
            List.range 1 n
                |> List.foldl (\i acc -> C.append refs.todos todoDoc.schema (Todo (sentence i 4) False) acc |> ok acc) d

        withFiles d =
            List.range 1 (max 1 (n // 10))
                |> List.foldl (\i acc -> typeFile ("file" ++ String.fromInt i ++ ".md") (sentence i 12) acc) d

        withTree d =
            List.range 1 n
                |> List.foldl (\i acc -> C.addChild refs.outline onodeDoc.schema (ONode (word i)) Nothing acc |> ok acc) d

        withLikes d =
            List.range 1 (max 1 (n // 5))
                |> List.foldl (\_ acc -> C.increment refs.likes 1 acc |> ok acc) d
    in
    init |> withTitle |> withTodos |> withFiles |> withTree |> withLikes


build : String -> Int -> Doc Board
build workload n =
    case workload of
        "text" ->
            buildText n

        "typing" ->
            buildTyping n

        "list" ->
            buildList n

        "churn" ->
            buildChurn n

        "flat" ->
            buildFlat n

        "dict" ->
            buildDict n

        "tree" ->
            buildTree n

        _ ->
            buildDemo n


{-| One more *typical* edit on top of a doc — the realistic steady-state action whose
delta (`encodeSince`) is the small payload where gzip's cold dictionary hurts most.
Returns the doc after the edit; the caller diffs it against the pre-edit version.
-}
oneMoreEdit : String -> Doc Board -> Doc Board
oneMoreEdit workload doc =
    case workload of
        "text" ->
            -- type a few more characters into the existing file
            let
                fileRef =
                    refs.files |> C.key "doc.md" C.richText

                current =
                    C.read doc |> Result.toMaybe |> Maybe.andThen (\b -> Dict.get "doc.md" b.files) |> Maybe.map plain |> Maybe.withDefault ""
            in
            C.setRich fileRef (current ++ " more") doc |> ok doc

        "typing" ->
            -- one more keystroke: append a single char to the title (1-char run op)
            let
                current =
                    C.read doc |> Result.toMaybe |> Maybe.map .title |> Maybe.withDefault ""
            in
            C.set refs.title (current ++ "x") doc |> ok doc

        "list" ->
            C.append refs.todos todoDoc.schema (Todo "one more" False) doc |> ok doc

        "dict" ->
            C.setKey refs.files C.richText "newkey" [] doc |> ok doc

        "tree" ->
            C.addChild refs.outline onodeDoc.schema (ONode "one more") Nothing doc |> ok doc

        _ ->
            C.append refs.todos todoDoc.schema (Todo "one more" False) doc |> ok doc


{-| Plain text of a rich-text span list, for `oneMoreEdit` on "text".
-}
plain : List Span -> String
plain spans =
    List.map .text spans |> String.concat



-- STATS ----------------------------------------------------------------------


{-| The encoded byte size of a document (its full wire form — a good proxy for the
information content, and correlated with heap since every op/element is retained).
-}
encodedBytes : Doc Board -> Int
encodedBytes doc =
    Doc.encode doc |> JE.encode 0 |> String.length


{-| A command. `mode` selects the harness: "" = build/retain (mem), "merge", "wire",
"build" (latency of constructing the doc), "read" (latency of `iters` reads).
-}
type alias Command =
    { workload : String, n : Int, retain : Bool, reset : Bool, roundtrip : Bool, mode : String, iters : Int }


type alias Model =
    { retained : List (Doc Board) }


{-| The wire payloads for a workload at size `n`: the full-document encoding, and the
delta (`encodeSince`) of exactly one more typical edit on top.
-}
wirePayloads : String -> Int -> ( String, String )
wirePayloads workload n =
    let
        doc =
            build workload n

        before =
            Doc.version doc

        edited =
            oneMoreEdit workload doc

        full =
            Doc.encode doc |> JE.encode 0

        delta =
            Doc.encodeSince before edited |> JE.encode 0
    in
    ( full, delta )


{-| A doc as it would arrive over the wire: encode then decode into a *fresh* replica.
-}
received : Doc Board -> Doc Board
received doc =
    Doc.decodeInto (Doc.encode doc) (C.init (Id.replica "recv") boardDoc.schema)
        |> Result.withDefault doc


{-| Merge-timing setup for a workload at size `n`: a `local` doc, and a `peer` doc
that shares `local`'s history plus one extra edit on a DIFFERENT part of the tree — so
the merge integrates a small remote delta into a large doc.
-}
mergeSetup : String -> Int -> ( Doc Board, Doc Board )
mergeSetup workload n =
    let
        local =
            build workload n

        peer =
            Doc.decodeInto (Doc.encode local) (C.init (Id.replica "peer") boardDoc.schema)
                |> Result.withDefault local
                |> (\d -> C.set refs.title "peer edit" d |> ok d)
    in
    ( local, peer )


{-| Merge `peer` into `local` `iters` times and return a checksum that FORCES each
merge's materialization. JS times the whole batch.
-}
mergeBench : String -> Int -> Int -> Int
mergeBench workload n iters =
    let
        ( local, peer ) =
            mergeSetup workload n

        once acc =
            acc + Doc.opCount (Doc.merge local peer)
    in
    List.range 1 iters |> List.foldl (\_ acc -> once acc) 0


{-| Delta-ingest latency: decode a one-edit `encodeSince` delta into a size-`n` doc
`iters` times (the demo's real per-incoming-message path — `decodeWithDiff`). Unlike
full-doc `merge` (which unions two N-sized op stores, inherently O(N)), this is the case
that _should_ cost O(delta): the payload carries one op. Returns a forced checksum.
-}
deltaBench : String -> Int -> Int -> Int
deltaBench workload n iters =
    let
        local =
            build workload n

        before =
            Doc.version local

        -- the small wire delta of one more typical edit, as it goes over the socket
        delta =
            Doc.encodeSince before (oneMoreEdit workload local)

        -- Elm is strict, so constructing the decoded `Doc` already forces its materialized
        -- cache; `acc + 1` is enough to keep the decode from being elided. (Avoid
        -- `Doc.opCount`, which is an O(store) walk that would swamp the O(delta) decode
        -- we're trying to measure.)
        once acc =
            case Doc.decodeInto delta local of
                Ok _ ->
                    acc + 1

                Err _ ->
                    acc
    in
    List.range 1 iters |> List.foldl (\_ acc -> once acc) 0


{-| Read-latency: `iters` reads of a size-`n` doc, returning a forced checksum. JS times
the batch and subtracts the build cost (reported separately by "build" mode).
-}
readBench : String -> Int -> Int -> Int
readBench workload n iters =
    let
        doc =
            build workload n

        once acc =
            case C.read doc of
                Ok b ->
                    acc + List.length b.todos + Dict.size b.files + List.length b.outline

                Err _ ->
                    acc
    in
    List.range 1 iters |> List.foldl (\_ acc -> once acc) 0


main : Program () Model Command
main =
    Platform.worker
        { init = \_ -> ( { retained = [] }, Cmd.none )
        , update =
            \cmd model ->
                if cmd.mode == "merge" then
                    let
                        iters =
                            max 1 cmd.iters
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "iters", JE.int iters )
                            , ( "checksum", JE.int (mergeBench cmd.workload cmd.n iters) )
                            ]
                        )
                    )

                else if cmd.mode == "delta" then
                    let
                        iters =
                            max 1 cmd.iters
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "iters", JE.int iters )
                            , ( "checksum", JE.int (deltaBench cmd.workload cmd.n iters) )
                            ]
                        )
                    )

                else if cmd.mode == "read" then
                    let
                        iters =
                            max 1 cmd.iters
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "iters", JE.int iters )
                            , ( "checksum", JE.int (readBench cmd.workload cmd.n iters) )
                            ]
                        )
                    )

                else if cmd.mode == "build" then
                    -- build-latency: JS times `iters` invocations of this command; the
                    -- checksum forces the build. Keeps nothing.
                    let
                        ops =
                            case cmd.workload of
                                -- no-CRDT baselines: the same N edits on a plain immutable
                                -- Elm record (no `Doc`), so "what does adding CRDTs to a
                                -- normal Elm app cost" is measured through the identical
                                -- harness. Plain Elm (immutable) is the honest floor —
                                -- not plain JS's mutable arrays.
                                "plainlist" ->
                                    plainListBuild cmd.n

                                "plaintext" ->
                                    plainTextBuild cmd.n

                                _ ->
                                    Doc.opCount (build cmd.workload cmd.n)
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "ops", JE.int ops )
                            ]
                        )
                    )

                else if cmd.mode == "wire" then
                    let
                        ( full, delta ) =
                            wirePayloads cmd.workload cmd.n
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "full", JE.string full )
                            , ( "delta", JE.string delta )
                            ]
                        )
                    )

                else
                    let
                        doc =
                            if cmd.roundtrip then
                                received (build cmd.workload cmd.n)

                            else
                                build cmd.workload cmd.n

                        base =
                            if cmd.reset then
                                []

                            else
                                model.retained

                        retained =
                            if cmd.retain then
                                doc :: base

                            else
                                base
                    in
                    ( { retained = retained }
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "ops", JE.int (Doc.opCount doc) )
                            , ( "bytes", JE.int (encodedBytes doc) )
                            , ( "retainedCount", JE.int (List.length retained) )
                            ]
                        )
                    )
        , subscriptions = \_ -> command identity
        }


{-| JS -> Elm: build this command and report stats (retaining the doc if asked).
-}
port command : (Command -> msg) -> Sub msg


{-| Elm -> JS: the stat record for the built doc.
-}
port done : JE.Value -> Cmd msg
