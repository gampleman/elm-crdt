port module Headless exposing (main)

{-| Headless **memory/size/latency** harness, driven from Node (`run-mem.js`,
`run-merge.js`, `run-wire.js`, `run-bench.js`, `run-delta.js`, `run-scrub.js`).

For each requested workload it builds a document at scale `n` and reports structural
proxies for its footprint — the number of ops in the log, and the encoded byte size
(overall, and split by container). These proxies are deterministic (unlike a
GC-dependent heap reading) and attribute cost to a _structure_, so we can see which
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
  - `"deletes"` — a list of `n`, then every other element deleted, so half the log is
    `DeleteElem`. The delete-heavy shape, for pricing anything the fold does per delete
    (pair it with `mode:"scrub"`).
  - `"dict"` — a dict of `n` short text entries.
  - `"tree"` — an outline tree of `n` nodes.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
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


{-| The board's flat bundle: one `Ref` per field plus the reserved `.schema` (same shape
the demo uses).
-}
type alias BoardDoc =
    { title : Ref Board C.Settable String
    , todos : Ref Board (C.ListK C.Movable C.Nested Todo) (List Todo)
    , files : Ref Board (C.DictK C.RichK (List Span)) (Dict String (List Span))
    , outline : Ref Board (C.TreeK C.Nested ONode) (Tree.Forest ONode)
    , likes : Ref Board C.Counter Int
    , tags : Ref Board (C.ListK C.Fixed C.Settable String) (List String)
    , schema : C.Schema C.Nested Board
    }


boardDoc : BoardDoc
boardDoc =
    C.record Board BoardDoc
        |> C.field "title" .title C.text
        |> C.field "todos" .todos todosList
        |> C.field "files" .files filesDict
        |> C.field "outline" .outline outlineTree
        |> C.field "likes" .likes C.counter
        |> C.field "tags" .tags tagsList
        |> C.build


refs : BoardDoc
refs =
    boardDoc


{-| The container bundles. Each carries its element/value schema, so the edit and ref
call sites below don't repeat it: `todosList.index i`, `filesDict.key k`,
`outlineTree.node id`.
-}
todosList : C.Crdt (C.ListK C.Movable C.Nested Todo) (List Todo) { index : Int -> Ref r (C.ListK mv C.Nested Todo) (List Todo) -> Ref r C.Nested Todo }
todosList =
    C.movableList todoDoc


filesDict : C.Crdt (C.DictK C.RichK (List Span)) (Dict String (List Span)) { key : String -> Ref r (C.DictK C.RichK (List Span)) (Dict String (List Span)) -> Ref r C.RichK (List Span) }
filesDict =
    C.dict C.richText


outlineTree : C.Crdt (C.TreeK C.Nested ONode) (Tree.Forest ONode) { node : Id.OpId -> Ref r (C.TreeK C.Nested ONode) (Tree.Forest ONode) -> Ref r C.Nested ONode }
outlineTree =
    C.tree onodeDoc


tagsList : C.Crdt (C.ListK C.Fixed C.Settable String) (List String) { index : Int -> Ref r (C.ListK mv C.Settable String) (List String) -> Ref r C.Settable String }
tagsList =
    C.list C.text


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


type alias ONodeDoc =
    { text : Ref ONode C.Settable String
    , schema : C.Schema C.Nested ONode
    }


onodeDoc : ONodeDoc
onodeDoc =
    C.record ONode ONodeDoc
        |> C.field "text" .text C.text
        |> C.build


init : Doc Board
init =
    C.init (Id.replica "bench") boardDoc.schema


ok : Doc Board -> Result Edit.EditError (Doc Board) -> Doc Board
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
            Edit.setKey refs.files name [] doc |> ok doc

        fileRef =
            refs.files |> filesDict.key name
    in
    Edit.setRich fileRef s d1 |> ok d1


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
        |> List.foldl (\i doc -> Edit.set refs.title (String.left i full) doc |> ok doc) init


buildList : Int -> Doc Board
buildList n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Edit.append refs.todos (Todo (word i) False) doc |> ok doc)
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
                ( Edit.move refs.todos from to doc |> ok doc, s2 )
            )
            ( list0, 1 )
        |> Tuple.first


{-| Delete-heavy: build a list of `n` records, then delete **every other one** — so half
the ops in the log are `DeleteElem`, each addressing an element by id.

This is the workload that costs something under the pending-ops gate
(`design-docs/15-pending-ops.md`): a delete is one of the three gated actions, so every
one of them pays a target walk plus an `Rga.get` for its subject on each fold. Registers,
text and inserts skip the check syntactically, so `list`/`text`/`dict` can't see it —
only this workload and `scrub` (which re-folds the whole log) can.

-}
buildDeletes : Int -> Doc Board
buildDeletes n =
    let
        list0 =
            buildList n
    in
    -- delete from the back, so each index is still valid as the list shrinks
    List.range 1 (n // 2)
        |> List.foldl
            (\i doc -> Edit.remove refs.todos (n - (2 * i)) doc |> ok doc)
            list0


buildFlat : Int -> Doc Board
buildFlat n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Edit.append refs.tags (word i) doc |> ok doc)
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
            (\i doc -> Edit.setKey refs.files ("k" ++ String.fromInt i) [] doc |> ok doc)
            init


buildTree : Int -> Doc Board
buildTree n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Edit.addChild refs.outline onodeDoc (ONode (word i)) Nothing doc |> ok doc)
            init


{-| The realistic demo-like doc, scaled by `n`: `n` todos, `n/10` files each with a
short sentence, `n` outline nodes, a title, and some likes.
-}
buildDemo : Int -> Doc Board
buildDemo n =
    let
        withTitle d =
            Edit.set refs.title "Benchmark board" d |> ok d

        withTodos d =
            List.range 1 n
                |> List.foldl (\i acc -> Edit.append refs.todos (Todo (sentence i 4) False) acc |> ok acc) d

        withFiles d =
            List.range 1 (max 1 (n // 10))
                |> List.foldl (\i acc -> typeFile ("file" ++ String.fromInt i ++ ".md") (sentence i 12) acc) d

        withTree d =
            List.range 1 n
                |> List.foldl (\i acc -> Edit.addChild refs.outline onodeDoc (ONode (word i)) Nothing acc |> ok acc) d

        withLikes d =
            List.range 1 (max 1 (n // 5))
                |> List.foldl (\_ acc -> Edit.increment refs.likes 1 acc |> ok acc) d
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

        "deletes" ->
            buildDeletes n

        "flat" ->
            buildFlat n

        "dict" ->
            buildDict n

        "tree" ->
            buildTree n

        _ ->
            buildDemo n


{-| One more _typical_ edit on top of a doc — the realistic steady-state action whose
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
                    refs.files |> filesDict.key "doc.md"

                current =
                    Doc.read doc |> Result.toMaybe |> Maybe.andThen (\b -> Dict.get "doc.md" b.files) |> Maybe.map plain |> Maybe.withDefault ""
            in
            Edit.setRich fileRef (current ++ " more") doc |> ok doc

        "typing" ->
            -- one more keystroke: append a single char to the title (1-char run op)
            let
                current =
                    Doc.read doc |> Result.toMaybe |> Maybe.map .title |> Maybe.withDefault ""
            in
            Edit.set refs.title (current ++ "x") doc |> ok doc

        "list" ->
            Edit.append refs.todos (Todo "one more" False) doc |> ok doc

        "dict" ->
            Edit.setKey refs.files "newkey" [] doc |> ok doc

        "tree" ->
            Edit.addChild refs.outline onodeDoc (ONode "one more") Nothing doc |> ok doc

        _ ->
            Edit.append refs.todos (Todo "one more" False) doc |> ok doc


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


{-| A command. `mode` selects the harness: "" = build/retain (mem), "merge", "delta",
"wire", "build" (latency of constructing the doc), "read" (latency of `iters` cached reads),
"fresh" (the same reads re-folded from base), "scrub" (`readAt` mid-history).
-}
type alias Command =
    { workload : String, n : Int, retain : Bool, reset : Bool, roundtrip : Bool, mode : String, iters : Int }


type alias Model =
    { retained : List (Doc Board) }


{-| The wire payloads for a workload at size `n`: the full-document encoding, the delta
(`encodeSince`) of exactly one more typical edit on top, and a **snapshot** of the compacted
document.

The snapshot is the only payload that carries the `Node` **state** encoding rather than the
op encoding, so it is the only one that moves when a container's element representation
changes (`design-docs/16-typed-sequence-content.md`). It is also a real path, not a
curiosity: it is what a peer behind our compaction boundary is sent to catch up
(`Crdt.Doc.encodeFrom` / the snapshot branch of `decodeInto`).

-}
wirePayloads : String -> Int -> { full : String, delta : String, snapshot : String }
wirePayloads workload n =
    let
        doc =
            build workload n

        before =
            Doc.version doc

        edited =
            oneMoreEdit workload doc
    in
    { full = Doc.encode doc |> JE.encode 0
    , delta = Doc.encodeSince before edited |> JE.encode 0
    , snapshot = Doc.encodeFrom (Doc.version doc) doc |> JE.encode 0
    }


{-| A doc as it would arrive over the wire: encode then decode into a _fresh_ replica.
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
                |> (\d -> Edit.set refs.title "peer edit" d |> ok d)
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


{-| History-scrub latency: `iters` `readAt` reads at a **mid-history** version of a size-`n`
doc, returning a forced checksum.

Unlike `readBench` (served from the maintained cache) this is the one path that **re-folds
the op log from the base** on every call — `checkout` the frontier's ancestors, then
materialize. It is therefore where any per-op cost added to the fold shows up, and the
reason it has its own mode: pair it with the `deletes` workload to price the pending-ops
gate (`design-docs/15-pending-ops.md`).

Scrubbing to the middle rather than the end also means half the log is _excluded_, which
is what a scrubber drag actually does.

-}
scrubBench : String -> Int -> Int -> Int
scrubBench workload n iters =
    let
        doc =
            build workload n

        at =
            Doc.versionAt (Doc.historyLength doc // 2) doc

        once acc =
            case Doc.readAt at doc of
                Ok b ->
                    acc + List.length b.todos + Dict.size b.files + List.length b.outline

                Err _ ->
                    acc
    in
    List.range 1 iters |> List.foldl (\_ acc -> once acc) 0


{-| The un-cached counterpart to `readBench`: `iters` reads of the **head** version via
`readAt`, which re-folds the entire op log from the base instead of reading the maintained
cache. Same content as `readBench`, so the two are directly comparable — that ratio is the
Phase-2 go/no-go gate `run.js` prints (cached must stay ~flat in `n` while fresh grows).

`scrubBench` measures the same machinery at a mid-history version, where half the log is
excluded; this one folds all of it.

-}
freshReadBench : String -> Int -> Int -> Int
freshReadBench workload n iters =
    let
        doc =
            build workload n

        head =
            Doc.version doc

        once acc =
            case Doc.readAt head doc of
                Ok b ->
                    acc + List.length b.todos + Dict.size b.files + List.length b.outline

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
            case Doc.read doc of
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

                else if cmd.mode == "fresh" then
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
                            , ( "checksum", JE.int (freshReadBench cmd.workload cmd.n iters) )
                            ]
                        )
                    )

                else if cmd.mode == "scrub" then
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
                            , ( "checksum", JE.int (scrubBench cmd.workload cmd.n iters) )
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
                        payloads =
                            wirePayloads cmd.workload cmd.n
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "full", JE.string payloads.full )
                            , ( "delta", JE.string payloads.delta )
                            , ( "snapshot", JE.string payloads.snapshot )
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
