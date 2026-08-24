module Helpers.Edits exposing
    ( Item
    , Op(..)
    , Sample
    , Step(..)
    , Todo
    , apply
    , deliver
    , fuzzSchedule
    , fuzzScript
    , init
    , render
    , run
    , runFrom
    , runSchedule
    , syncAll
    )

{-| **One fuzzable edit language over one document that uses every container.**

The property modules in this suite each define their own `Op` type, `applyOp` and `run` —
several near-identical fuzzers, each covering whatever slice of the schema its own subject
needed. The cost is not the duplication but the _narrowness_: convergence is fuzzed over
blocks, `move` over movable lists, and nothing is fuzzed over a document that holds a
counter and a tree and rich text at once — which is the shape of every real document, and
the only shape in which cross-container bugs (a clock advanced from the wrong subtree, a
cache invalidated for one container but read through another) can appear at all.

So: one schema with **every** container kind (text, register, counter, fixed list, movable
list of records, dict, movable tree, rich text, op-set), one `Op` per interesting edit, and
`render` collapsing the entire read model to a single comparable `String` so a property is
one `Expect.equal`.

Two levels of generation:

  - `fuzzScript` — a list of `Op`s applied to one replica. For properties about a single
    document (compaction preserves the read, the cache stays consistent).
  - `fuzzSchedule` — an interleaving of edits **and partial syncs** across N replicas
    (`Step`). This is what actually finds convergence bugs: replicas that diverge, sync
    halfway, edit again on top of a partial view, and only then reconcile. A schedule
    subsumes "two replicas each make a batch of edits, then merge", which is all the
    per-module fuzzers can express.

Every index an `Op` carries is **clamped against the document it is applied to**, so a
random script is a run of meaningful edits rather than mostly-`EditError` no-ops. `apply`
is total: an edit that fails to resolve leaves the document alone, exactly as an app doing
`Result.withDefault doc` would, and a property asserting over `render` covers "the edit
silently did nothing" anyway.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id exposing (OpId)
import Crdt.RichText exposing (MarkValue(..), Span)
import Crdt.Tree as Tree
import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)



-- DOCUMENT -------------------------------------------------------------------


{-| Deliberately one of everything: each field is a different container kind, and
`todos`/`outline` nest a record inside a sequence, so the nested-merge paths are covered
too.
-}
type alias Sample =
    { title : String
    , votes : Int
    , flag : Bool
    , todos : List Todo
    , tags : List String
    , meta : Dict String String
    , outline : Tree.Forest Item
    , body : List Span
    , high : Int
    }


type alias Todo =
    { text : String, done : Bool }


type alias Item =
    { label : String }


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


type alias ItemDoc =
    { label : Ref Item C.Settable String
    , schema : C.Schema C.Nested Item
    }


itemDoc : ItemDoc
itemDoc =
    C.record Item ItemDoc
        |> C.field "label" .label C.text
        |> C.build


type alias DocRefs =
    { title : Ref Sample C.Settable String
    , votes : Ref Sample C.Counter Int
    , flag : Ref Sample C.Settable Bool
    , todos : Ref Sample (C.ListK C.Movable C.Nested Todo) (List Todo)
    , tags : Ref Sample (C.ListK C.Fixed C.Settable String) (List String)
    , meta : Ref Sample (C.DictK C.Settable String) (Dict String String)
    , outline : Ref Sample (C.TreeK C.Nested Item) (Tree.Forest Item)
    , body : Ref Sample C.RichK (List Span)
    , high : Ref Sample (C.OpSetK C.Settable Int) Int
    , schema : C.Schema C.Nested Sample
    }


{-| Container bundles are kept as top-level values so their element accessors
(`.index` / `.key` / `.node`) are available for composing sub-refs with `C.at`.
-}
todosList :
    C.Crdt
        (C.ListK C.Movable C.Nested Todo)
        (List Todo)
        { index :
            Int
            -> Ref Sample (C.ListK C.Movable C.Nested Todo) (List Todo)
            -> Ref Sample C.Nested Todo
        }
todosList =
    C.movableList todoDoc


tagsList :
    C.Crdt
        (C.ListK C.Fixed C.Settable String)
        (List String)
        { index :
            Int
            -> Ref Sample (C.ListK C.Fixed C.Settable String) (List String)
            -> Ref Sample C.Settable String
        }
tagsList =
    C.list C.text


metaDict :
    C.Crdt
        (C.DictK C.Settable String)
        (Dict String String)
        { key :
            String
            -> Ref Sample (C.DictK C.Settable String) (Dict String String)
            -> Ref Sample C.Settable String
        }
metaDict =
    C.dict C.text


outlineTree :
    C.Crdt
        (C.TreeK C.Nested Item)
        (Tree.Forest Item)
        { node :
            OpId
            -> Ref Sample (C.TreeK C.Nested Item) (Tree.Forest Item)
            -> Ref Sample C.Nested Item
        }
outlineTree =
    C.tree itemDoc


{-| A max-register: the highest value anyone contributed. Enough to exercise the op-set
fold, contribution keys and `retract` without inventing a second user-defined CRDT.
-}
highScore : C.Crdt (C.OpSetK C.Settable Int) Int {}
highScore =
    C.opSet { contribution = C.int, fold = List.maximum >> Maybe.withDefault 0 }


docBundle : DocRefs
docBundle =
    C.record Sample DocRefs
        |> C.field "title" .title C.text
        |> C.field "votes" .votes C.counter
        |> C.field "flag" .flag C.bool
        |> C.field "todos" .todos todosList
        |> C.field "tags" .tags tagsList
        |> C.field "meta" .meta metaDict
        |> C.field "outline" .outline outlineTree
        |> C.field "body" .body C.richText
        |> C.field "high" .high highScore
        |> C.build


refs : DocRefs
refs =
    docBundle


docSchema : C.Schema C.Nested Sample
docSchema =
    docBundle.schema


init : String -> Doc Sample
init name =
    Doc.init (Id.replica name) docSchema


read : Doc Sample -> Result String Sample
read doc =
    Doc.read doc |> Result.mapError (\_ -> "READ-FAILED")



-- RENDER ---------------------------------------------------------------------


{-| The **whole** read model as one string, so a property compares two documents with a
single `Expect.equal` and a failure prints something a human can localize.

Everything observable goes in. A field left out of this string is a field whose
convergence these properties do not actually check.

-}
render : Doc Sample -> String
render doc =
    case read doc of
        Err e ->
            e

        Ok v ->
            [ "title=" ++ v.title
            , "votes=" ++ String.fromInt v.votes
            , "flag=" ++ flag v.flag
            , "todos=[" ++ (v.todos |> List.map todo |> String.join ",") ++ "]"
            , "tags=[" ++ String.join "," v.tags ++ "]"
            , "meta={" ++ (Dict.toList v.meta |> List.map pair |> String.join ",") ++ "}"
            , "outline=" ++ shape v.outline
            , "body=" ++ spans v.body
            , "high=" ++ String.fromInt v.high
            ]
                |> String.join " "


flag : Bool -> String
flag b =
    if b then
        "t"

    else
        "f"


todo : Todo -> String
todo t =
    t.text
        ++ (if t.done then
                "!"

            else
                ""
           )


pair : ( String, String ) -> String
pair ( k, v ) =
    k ++ ":" ++ v


shape : Tree.Forest Item -> String
shape forest =
    forest
        |> List.map
            (\item ->
                let
                    label =
                        Tree.itemValue item |> .label
                in
                case Tree.itemChildren item of
                    [] ->
                        label

                    kids ->
                        label ++ "[" ++ shape kids ++ "]"
            )
        |> String.join " "


spans : List Span -> String
spans =
    List.map
        (\s ->
            s.text
                ++ "{"
                ++ (Dict.toList s.marks |> List.map mark |> String.join ",")
                ++ "}"
        )
        >> String.join "|"


mark : ( String, MarkValue ) -> String
mark ( name, value ) =
    case value of
        Flag ->
            name

        Value v ->
            name ++ "=" ++ v



-- THE EDIT LANGUAGE ----------------------------------------------------------


{-| One user-level edit. `Int` arguments are positions, clamped on apply, so no
constructor can be "invalid" — only a no-op against an empty container.
-}
type Op
    = SetTitle String
    | Vote Int
    | Flip
      -- movable list of records
    | AddTodo String
    | RemoveTodo Int
    | MoveTodo Int Int
    | SetTodoText Int String
    | ToggleTodo Int
      -- fixed list
    | AddTag String
    | RemoveTag Int
    | SetTag Int String
      -- dict
    | SetMeta String String
    | RemoveMeta String
      -- movable tree
    | AddRoot String
    | AddUnder Int String
    | MoveNode Int Int
    | RemoveNode Int
    | SetNodeLabel Int String
      -- rich text
    | SetBody String
    | Mark Int Int String
    | Unmark Int Int String
    | SplitBlock Int Int
    | MergeBlock Int
    | SetBlockType Int String
    | Indent Int
    | Outdent Int
      -- op-set (user-defined CRDT)
    | Contribute Int
    | ContributeThenRetract Int


{-| Small pools of names and values, so concurrent replicas routinely pick the **same**
key, tag or mark — which is what makes their edits genuinely conflict instead of merging
trivially side by side.
-}
fuzzOp : Fuzzer Op
fuzzOp =
    Fuzz.oneOf
        [ Fuzz.map SetTitle word
        , Fuzz.map Vote (Fuzz.intRange -3 3)
        , Fuzz.constant Flip
        , Fuzz.map AddTodo word
        , Fuzz.map RemoveTodo smallIndex
        , Fuzz.map2 MoveTodo smallIndex smallIndex
        , Fuzz.map2 SetTodoText smallIndex word
        , Fuzz.map ToggleTodo smallIndex
        , Fuzz.map AddTag word
        , Fuzz.map RemoveTag smallIndex
        , Fuzz.map2 SetTag smallIndex word
        , Fuzz.map2 SetMeta key word
        , Fuzz.map RemoveMeta key
        , Fuzz.map AddRoot word
        , Fuzz.map2 AddUnder smallIndex word
        , Fuzz.map2 MoveNode smallIndex smallIndex
        , Fuzz.map RemoveNode smallIndex
        , Fuzz.map2 SetNodeLabel smallIndex word
        , Fuzz.map SetBody sentence
        , Fuzz.map3 Mark offset offset markName
        , Fuzz.map3 Unmark offset offset markName
        , Fuzz.map2 SplitBlock smallIndex offset
        , Fuzz.map MergeBlock smallIndex
        , Fuzz.map2 SetBlockType smallIndex blockType
        , Fuzz.map Indent smallIndex
        , Fuzz.map Outdent smallIndex
        , Fuzz.map Contribute (Fuzz.intRange 0 20)
        , Fuzz.map ContributeThenRetract (Fuzz.intRange 0 20)
        ]


word : Fuzzer String
word =
    Fuzz.oneOfValues [ "a", "b", "cd", "" ]


sentence : Fuzzer String
sentence =
    Fuzz.oneOfValues [ "", "x", "hello", "the quick fox", "one two" ]


key : Fuzzer String
key =
    Fuzz.oneOfValues [ "k1", "k2", "k3" ]


markName : Fuzzer String
markName =
    Fuzz.oneOfValues [ "bold", "italic", "comment" ]


blockType : Fuzzer String
blockType =
    Fuzz.oneOfValues [ "", "h1", "blockquote", "ul" ]


smallIndex : Fuzzer Int
smallIndex =
    Fuzz.intRange 0 4


offset : Fuzzer Int
offset =
    Fuzz.intRange 0 8


{-| A script of up to 12 edits. Long enough to build real structure (a list with items to
move, text with blocks to split), short enough that shrinking reports something legible.
-}
fuzzScript : Fuzzer (List Op)
fuzzScript =
    fuzzScriptUpTo 12


fuzzScriptUpTo : Int -> Fuzzer (List Op)
fuzzScriptUpTo n =
    Fuzz.listOfLengthBetween 0 n fuzzOp



-- APPLY ----------------------------------------------------------------------


{-| Apply one `Op`. Total: a failed edit leaves the document unchanged.
-}
apply : Op -> Doc Sample -> Doc Sample
apply op doc =
    case op of
        SetTitle s ->
            ok doc (Edit.set refs.title s doc)

        Vote n ->
            ok doc (Edit.increment refs.votes n doc)

        Flip ->
            ok doc (Edit.over refs.flag not doc)

        AddTodo label ->
            ok doc (Edit.append refs.todos { text = label, done = False } doc)

        RemoveTodo i ->
            withIndex (todoCount doc) i doc (\j -> Edit.remove refs.todos j doc)

        MoveTodo from to ->
            withIndex (todoCount doc)
                from
                doc
                (\j -> Edit.move refs.todos j (modBy (todoCount doc) to) doc)

        SetTodoText i s ->
            withIndex (todoCount doc)
                i
                doc
                (\j -> Edit.set (todosList.index j refs.todos |> C.at todoDoc.text) s doc)

        ToggleTodo i ->
            withIndex (todoCount doc)
                i
                doc
                (\j -> Edit.over (todosList.index j refs.todos |> C.at todoDoc.done) not doc)

        AddTag t ->
            ok doc (Edit.append refs.tags t doc)

        RemoveTag i ->
            withIndex (tagCount doc) i doc (\j -> Edit.remove refs.tags j doc)

        SetTag i s ->
            withIndex (tagCount doc) i doc (\j -> Edit.set (tagsList.index j refs.tags) s doc)

        SetMeta k v ->
            ok doc (Edit.setKey refs.meta k v doc)

        RemoveMeta k ->
            ok doc (Edit.removeKey refs.meta k doc)

        AddRoot label ->
            ok doc (Edit.addChild refs.outline itemDoc { label = label } Nothing doc)

        AddUnder i label ->
            withNode i doc (\nodeId -> Edit.addChild refs.outline itemDoc { label = label } (Just nodeId) doc)

        MoveNode i j ->
            -- Move node `i` under node `j`, both clamped into the current forest. A
            -- concurrent schedule routinely makes this a cycle, which is the interesting
            -- case: the tree must drop one of the moves rather than lose the subtree.
            withNode i doc (\childId -> Edit.moveInto refs.outline childId (nodeIdAt j doc) doc)

        RemoveNode i ->
            withNode i doc (\nodeId -> Edit.removeNode refs.outline nodeId doc)

        SetNodeLabel i label ->
            withNode i
                doc
                (\nodeId ->
                    Edit.set (outlineTree.node nodeId refs.outline |> C.at itemDoc.label) label doc
                )

        SetBody s ->
            ok doc (Edit.setRich refs.body s doc)

        Mark from to name ->
            withRange from to doc (\lo hi -> Edit.mark refs.body lo hi name Flag doc)

        Unmark from to name ->
            withRange from to doc (\lo hi -> Edit.unmark refs.body lo hi name doc)

        SplitBlock i charOffset ->
            withIndex (blockCount doc) i doc (\j -> Edit.splitBlock refs.body j charOffset doc)

        MergeBlock i ->
            -- Block 0 has no predecessor to merge into, so bias onto a real boundary.
            withIndex (blockCount doc - 1) i doc (\j -> Edit.mergeBlock refs.body (j + 1) doc)

        SetBlockType i t ->
            withIndex (blockCount doc)
                i
                doc
                (\j -> Edit.setBlockType refs.body j (nonEmpty t) doc)

        Indent i ->
            withIndex (blockCount doc) i doc (\j -> Edit.indentBlock refs.body j doc)

        Outdent i ->
            withIndex (blockCount doc) i doc (\j -> Edit.outdentBlock refs.body j doc)

        Contribute n ->
            case Edit.contribute refs.high C.int n doc of
                Ok ( _, doc1 ) ->
                    doc1

                Err _ ->
                    doc

        ContributeThenRetract n ->
            -- A contribution is keyed by its own op id, which only `contribute` hands
            -- back, so a fuzzed script can only retract one it made itself. (Retracting a
            -- *peer's* contribution is example-tested in `ExtensibilityTests`.) Still
            -- worth generating: the pair travels the wire together and has to fold away
            -- against a concurrent contribution of the same value.
            case Edit.contribute refs.high C.int n doc of
                Ok ( contributionKey, doc1 ) ->
                    ok doc1 (Edit.retract refs.high contributionKey doc1)

                Err _ ->
                    doc


run : List Op -> Doc Sample -> Doc Sample
run ops doc =
    List.foldl apply doc ops


{-| Build a fresh replica named `name` and run a script on it.
-}
runFrom : String -> List Op -> Doc Sample
runFrom name ops =
    run ops (init name)



-- APPLY HELPERS --------------------------------------------------------------


ok : Doc Sample -> Result Edit.EditError (Doc Sample) -> Doc Sample
ok fallback =
    Result.withDefault fallback


nonEmpty : String -> Maybe String
nonEmpty s =
    if s == "" then
        Nothing

    else
        Just s


{-| Run `f` with an index wrapped into `0..count-1`, or do nothing when there is nothing
to address.
-}
withIndex : Int -> Int -> Doc Sample -> (Int -> Result Edit.EditError (Doc Sample)) -> Doc Sample
withIndex count i doc f =
    if count <= 0 then
        doc

    else
        ok doc (f (modBy count i))


withNode : Int -> Doc Sample -> (OpId -> Result Edit.EditError (Doc Sample)) -> Doc Sample
withNode i doc f =
    case nodeIdAt i doc of
        Just nodeId ->
            ok doc (f nodeId)

        Nothing ->
            doc


{-| Run `f` over a non-empty half-open character range clamped into the body.
-}
withRange : Int -> Int -> Doc Sample -> (Int -> Int -> Result Edit.EditError (Doc Sample)) -> Doc Sample
withRange from to doc f =
    let
        len =
            bodyLength doc
    in
    if len <= 0 then
        doc

    else
        let
            a =
                modBy len from

            b =
                modBy len to
        in
        ok doc (f (min a b) (min len (max a b + 1)))


{-| Node ids in document order, so a fuzzed index names a stable node.
-}
nodeIds : Doc Sample -> List OpId
nodeIds doc =
    case read doc of
        Ok v ->
            flattenIds v.outline

        Err _ ->
            []


flattenIds : Tree.Forest Item -> List OpId
flattenIds forest =
    forest
        |> List.concatMap (\item -> Tree.itemId item :: flattenIds (Tree.itemChildren item))


nodeIdAt : Int -> Doc Sample -> Maybe OpId
nodeIdAt i doc =
    let
        ids =
            nodeIds doc
    in
    if List.isEmpty ids then
        Nothing

    else
        ids |> List.drop (modBy (List.length ids) i) |> List.head


todoCount : Doc Sample -> Int
todoCount doc =
    read doc |> Result.map (.todos >> List.length) |> Result.withDefault 0


tagCount : Doc Sample -> Int
tagCount doc =
    read doc |> Result.map (.tags >> List.length) |> Result.withDefault 0


bodyLength : Doc Sample -> Int
bodyLength doc =
    read doc
        |> Result.map (.body >> List.map (.text >> String.length) >> List.sum)
        |> Result.withDefault 0


blockCount : Doc Sample -> Int
blockCount doc =
    Edit.readBlocks refs.body doc |> Result.map List.length |> Result.withDefault 0



-- SCHEDULES ------------------------------------------------------------------


{-| One step of a concurrent history: a replica edits, or a replica **receives** another's
current state.

`Deliver from to` is a real wire delivery (`encodeSince` the receiver's version, then
`decodeInto`), so a schedule exercises the delta path and partial knowledge — a replica can
edit on top of half of another's history and only reconcile the rest later.

`Compact i` garbage-collects replica `i` at the **stable frontier** across every replica in
the schedule — the cut they have all delivered past, which is the library's own
multi-replica-safe GC policy (`Doc.stableFrontier`, regime 2 in `design-docs/04-gc.md`).
Compacting at the replica's _own_ frontier instead would step outside that contract as soon
as any peer holds concurrent work, so a schedule property built on it would only re-derive a
documented limitation.

`fuzzSchedule` does not generate `Compact`: it is a `Step` so that a property can inject
compaction at a chosen point of an otherwise random schedule — see
`CompactionPropertyTests`.

-}
type Step
    = Edits Int Op
    | Deliver Int Int
    | Compact Int


{-| A schedule over `n` replicas: interleaved edits and deliveries, biased towards editing
(a schedule that is mostly deliveries never builds divergence worth reconciling).
-}
fuzzSchedule : Int -> Fuzzer (List Step)
fuzzSchedule n =
    let
        replica =
            Fuzz.intRange 0 (n - 1)
    in
    Fuzz.listOfLengthBetween 0
        20
        (Fuzz.frequency
            [ ( 3, Fuzz.map2 Edits replica fuzzOp )
            , ( 1, Fuzz.map2 Deliver replica replica )
            ]
        )


{-| Run a schedule over `n` replicas named `r0..r(n-1)`, returned in that order.
-}
runSchedule : Int -> List Step -> List (Doc Sample)
runSchedule n steps =
    List.range 0 (n - 1)
        |> List.map (\i -> init ("r" ++ String.fromInt i))
        |> (\replicas -> List.foldl step replicas steps)


step : Step -> List (Doc Sample) -> List (Doc Sample)
step s replicas =
    case s of
        Edits i op ->
            updateAt i (apply op) replicas

        Deliver from to ->
            if from == to then
                replicas

            else
                case at from replicas of
                    Just source ->
                        updateAt to (deliver source) replicas

                    Nothing ->
                        replicas

        Compact i ->
            let
                versions =
                    List.map Doc.version replicas
            in
            updateAt i (\d -> Doc.compact (Doc.stableFrontier versions d) d) replicas


{-| Deliver `source`'s state to `target` over the wire, as a delta against what the target
already knows. (`encodeSince` falls back to a snapshot when the target is behind a
compaction boundary, which is exactly what a schedule containing a `compact` needs.)
-}
deliver : Doc Sample -> Doc Sample -> Doc Sample
deliver source target =
    Doc.decodeInto (Doc.encodeSince (Doc.version target) source) target
        |> Result.withDefault target


{-| Sync every replica with every other until they have all seen everything: two passes of
a full mesh, enough for any delivery graph over the given list.
-}
syncAll : List (Doc Sample) -> List (Doc Sample)
syncAll replicas =
    mesh (mesh replicas)


mesh : List (Doc Sample) -> List (Doc Sample)
mesh docs =
    docs
        |> List.indexedMap
            (\i target ->
                docs
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( j, source ) acc ->
                            if i == j then
                                acc

                            else
                                deliver source acc
                        )
                        target
            )


at : Int -> List a -> Maybe a
at i xs =
    List.drop i xs |> List.head


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f =
    List.indexedMap
        (\j x ->
            if i == j then
                f x

            else
                x
        )
