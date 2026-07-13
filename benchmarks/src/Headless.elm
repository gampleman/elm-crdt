port module Headless exposing (main)

{-| Headless **memory/size** harness, driven from Node (`run-mem.js`).

For each requested workload it builds a document at scale `n` and reports structural
proxies for its footprint — the number of ops in the log, and the encoded byte size
(overall, and split by container). These proxies are deterministic (unlike a
GC-dependent heap reading) and attribute cost to a *structure*, so we can see which
one dominates. `run-mem.js` additionally brackets the calls with
`process.memoryUsage()` to get an absolute heap figure.

Workloads:

  - `"demo"` — a realistic demo-like doc: todos + a dict of rich-text files + an
    outline tree + settings (title/status/likes), scaled by `n`.
  - `"text"` — one rich-text file with `n` typed characters (the hypothesized
    memory hog: one `Node` per character).
  - `"list"` — a movable list of `n` small records.
  - `"dict"` — a dict of `n` short text entries.
  - `"tree"` — an outline tree of `n` nodes.

Each reply is a JSON stat record `{ ops, bytes, textBytes, otherBytes }`, so the
harness can tabulate size-per-op and the text share.

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.RichText exposing (MarkValue(..), Span)
import Crdt.Schema as S exposing (Crdt)
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
    }


type alias Todo =
    { text : String, done : Bool }


type alias ONode =
    { text : String }


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


type alias TodoRefs =
    { text : Ref Todo S.Settable String, done : Ref Todo S.Settable Bool }


todoDoc : Ref.RecordRefs Todo TodoRefs
todoDoc =
    Ref.record Todo TodoRefs
        |> Ref.field "text" .text S.text
        |> Ref.field "done" .done S.bool
        |> Ref.build


type alias ONodeRefs =
    { text : Ref ONode S.Settable String }


onodeDoc : Ref.RecordRefs ONode ONodeRefs
onodeDoc =
    Ref.record ONode ONodeRefs
        |> Ref.field "text" .text S.text
        |> Ref.build


init : OpDoc Board
init =
    OpDoc.init (Id.replica "bench") boardDoc.schema


ok : OpDoc Board -> Result OpDoc.Error (OpDoc Board) -> OpDoc Board
ok fb =
    Result.withDefault fb



-- WORKLOADS ------------------------------------------------------------------


{-| Type `s` into a fresh file `name` via the demo's editor path (setRich diffs into
per-character insert ops), so the char-node cost is realistic.
-}
typeFile : String -> String -> OpDoc Board -> OpDoc Board
typeFile name s doc =
    let
        d1 =
            Ref.setKey S.richText name [] refs.files doc |> ok doc

        fileRef =
            refs.files |> Ref.key name S.richText
    in
    Ref.setRich fileRef s d1 |> ok d1


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


buildText : Int -> OpDoc Board
buildText n =
    -- one file with ~n characters typed
    typeFile "doc.md" (sentence 0 (max 1 (n // 5))) init


buildList : Int -> OpDoc Board
buildList n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Ref.append todoDoc.schema (Todo (word i) False) refs.todos doc |> ok doc)
            init


buildDict : Int -> OpDoc Board
buildDict n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Ref.setKey S.richText ("k" ++ String.fromInt i) [] refs.files doc |> ok doc)
            init


buildTree : Int -> OpDoc Board
buildTree n =
    List.range 1 n
        |> List.foldl
            (\i doc -> Ref.addChild onodeDoc.schema (ONode (word i)) Nothing refs.outline doc |> ok doc)
            init


{-| The realistic demo-like doc, scaled by `n`: `n` todos, `n/10` files each with a
short sentence, `n` outline nodes, a title, and some likes.
-}
buildDemo : Int -> OpDoc Board
buildDemo n =
    let
        withTitle d =
            Ref.set refs.title "Benchmark board" d |> ok d

        withTodos d =
            List.range 1 n
                |> List.foldl (\i acc -> Ref.append todoDoc.schema (Todo (sentence i 4) False) refs.todos acc |> ok acc) d

        withFiles d =
            List.range 1 (max 1 (n // 10))
                |> List.foldl (\i acc -> typeFile ("file" ++ String.fromInt i ++ ".md") (sentence i 12) acc) d

        withTree d =
            List.range 1 n
                |> List.foldl (\i acc -> Ref.addChild onodeDoc.schema (ONode (word i)) Nothing refs.outline acc |> ok acc) d

        withLikes d =
            List.range 1 (max 1 (n // 5))
                |> List.foldl (\_ acc -> Ref.increment refs.likes 1 acc |> ok acc) d
    in
    init |> withTitle |> withTodos |> withFiles |> withTree |> withLikes


build : String -> Int -> OpDoc Board
build workload n =
    case workload of
        "text" ->
            buildText n

        "list" ->
            buildList n

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
oneMoreEdit : String -> OpDoc Board -> OpDoc Board
oneMoreEdit workload doc =
    case workload of
        "text" ->
            -- type a few more characters into the existing file
            let
                fileRef =
                    refs.files |> Ref.key "doc.md" S.richText

                current =
                    OpDoc.read doc |> Result.toMaybe |> Maybe.andThen (\b -> Dict.get "doc.md" b.files) |> Maybe.map plain |> Maybe.withDefault ""
            in
            Ref.setRich fileRef (current ++ " more") doc |> ok doc

        "list" ->
            Ref.append todoDoc.schema (Todo "one more" False) refs.todos doc |> ok doc

        "dict" ->
            Ref.setKey S.richText "newkey" [] refs.files doc |> ok doc

        "tree" ->
            Ref.addChild onodeDoc.schema (ONode "one more") Nothing refs.outline doc |> ok doc

        _ ->
            Ref.append todoDoc.schema (Todo "one more" False) refs.todos doc |> ok doc


{-| Plain text of a rich-text span list, for `oneMoreEdit` on "text".
-}
plain : List Span -> String
plain spans =
    List.map .text spans |> String.concat



-- STATS ----------------------------------------------------------------------


{-| The encoded byte size of a document (its full wire form — a good proxy for the
information content, and correlated with heap since every op/element is retained).
-}
encodedBytes : OpDoc Board -> Int
encodedBytes doc =
    OpDoc.encode doc |> JE.encode 0 |> String.length


{-| A command: build `(workload, n)`. When `retain` is true, the built doc is kept
alive in the model (so its Elm-side heap is held in the shared V8 process and shows
up in `process.memoryUsage()`); `reset` clears the retained list first. This is how
the JS harness gets a real absolute heap figure for the live document — the doc
itself never crosses the port, so it must be retained on the Elm side.
-}
type alias Command =
    { workload : String, n : Int, retain : Bool, reset : Bool, roundtrip : Bool, mode : String, iters : Int }


type alias Model =
    { retained : List (OpDoc Board) }


{-| The wire payloads for a workload at size `n`: the full-document encoding, and the
delta (`encodeSince`) of exactly one more typical edit on top. The JS harness gzips
both to compare custom-format headroom against gzipped JSON — separately for the big
full doc and the tiny steady-state delta (where gzip's cold dictionary hurts).
-}
wirePayloads : String -> Int -> ( String, String )
wirePayloads workload n =
    let
        doc =
            build workload n

        before =
            OpDoc.version doc

        edited =
            oneMoreEdit workload doc

        full =
            OpDoc.encode doc |> JE.encode 0

        delta =
            OpDoc.encodeSince before edited |> JE.encode 0
    in
    ( full, delta )


{-| A doc as it would arrive over the wire: encode then decode into a *fresh* replica.
This is the case that matters for the replica-interning question — `opIdDecoder`
mints a fresh `ReplicaId` per op, where a locally-built doc shares one reference.
-}
received : OpDoc Board -> OpDoc Board
received doc =
    OpDoc.decodeInto (OpDoc.encode doc) (OpDoc.init (Id.replica "recv") boardDoc.schema)
        |> Result.withDefault doc


{-| Merge-timing setup for a workload at size `n`: a `local` doc, and a `peer` doc
that shares `local`'s history plus one extra edit on a DIFFERENT part of the tree — so
the merge integrates a small remote delta into a large doc. This is the exact shape the
demo hits on every incoming message, and the case incremental merge should speed up
(and where referential stability matters — the untouched containers should survive).
-}
mergeSetup : String -> Int -> ( OpDoc Board, OpDoc Board )
mergeSetup workload n =
    let
        local =
            build workload n

        -- a peer at the same history, then one edit to the TITLE (a container the
        -- workload's bulk did not touch), so most of the tree is unchanged by the merge.
        peer =
            OpDoc.decodeInto (OpDoc.encode local) (OpDoc.init (Id.replica "peer") boardDoc.schema)
                |> Result.withDefault local
                |> (\d -> Ref.set refs.title "peer edit" d |> ok d)
    in
    ( local, peer )


{-| Merge `peer` into `local` `iters` times (each from the same immutable inputs, so the
work is identical and repeatable) and return a checksum that FORCES each merge's
materialization (`opCount` reads the store; `encodedBytes` walks the merged tree). JS
times the whole batch. Higher `iters` amortizes port overhead.
-}
mergeBench : String -> Int -> Int -> Int
mergeBench workload n iters =
    let
        ( local, peer ) =
            mergeSetup workload n

        once acc =
            let
                merged =
                    OpDoc.merge local peer
            in
            -- Elm is eager, so `merge` fully computes the merged doc's `cached` (it walks
            -- it via `Node.maxCounter`); `opCount` just reads the store so the result
            -- can't be elided. NOT `encodedBytes` — a full JSON encode per iteration
            -- would swamp the merge cost we're trying to measure.
            acc + OpDoc.opCount merged
    in
    List.range 1 iters |> List.foldl (\_ acc -> once acc) 0


main : Program () Model Command
main =
    Platform.worker
        { init = \_ -> ( { retained = [] }, Cmd.none )
        , update =
            \cmd model ->
                if cmd.mode == "merge" then
                    -- merge-timing mode: run `iters` merges of a small remote delta into
                    -- a size-`n` doc and return a forced checksum. JS times the batch.
                    let
                        iters =
                            max 1 cmd.iters

                        checksum =
                            mergeBench cmd.workload cmd.n iters
                    in
                    ( model
                    , done
                        (JE.object
                            [ ( "workload", JE.string cmd.workload )
                            , ( "n", JE.int cmd.n )
                            , ( "iters", JE.int iters )
                            , ( "checksum", JE.int checksum )
                            ]
                        )
                    )

                else if cmd.mode == "wire" then
                    -- wire-size mode: hand back the JSON strings for JS to gzip. No
                    -- retention; this measures serialized size, not heap.
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
                            , ( "ops", JE.int (OpDoc.opCount doc) )
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
