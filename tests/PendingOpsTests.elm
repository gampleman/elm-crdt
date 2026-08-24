module PendingOpsTests exposing (suite)

{-| **Ops delivered ahead of the insert they name.** A causal order guarantees this can't
happen — an op's `deps` include that insert, and Lamport ids put every dep first — so these
tests deliberately break the guarantee, which is what a peer cannot do but its
_infrastructure_ can: a relay that answers delta queries out of an op table and filters by
author, a truncated persisted log, a payload that lost part of itself.

Four in-place mutators (`Rga.delete`, `Rga.updateElement`, `MoveList.updateValue`,
`Tree.updateValue`) evaporate when their subject is absent, so before
`OpLog.applyOpsWithPending` the two failure modes were:

  - a **delete** silently didn't happen — and once the withheld insert arrived, the deleted
    content came back and stayed;
  - a **nested edit** into a list item silently didn't happen — and it never got another
    chance, so the edit was lost for good ("my change didn't save").

Both are now deferred and retried, so delivery order stops mattering here as it already
doesn't everywhere else. Each test asserts the deferred op **lands** once its subject
arrives, and that `Doc.cacheConsistent` holds throughout — the incrementally-maintained
cache must equal a full `materialize`, including while ops are held back, or live state and
history-scrubbing would disagree about the same document.

Deltas are produced with `encodeSince` per edit and then delivered in the wrong order,
which is exactly the shape a filtering relay produces.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Json.Encode as JE
import Test exposing (Test, describe, test)



-- SCHEMA: a fixed list and a movable list of nested records --------------------


type alias Item =
    { label : String, done : Bool }


type alias ItemDoc =
    { label : Ref Item C.Settable String
    , done : Ref Item C.Settable Bool
    , schema : C.Schema C.Nested Item
    }


itemDoc : ItemDoc
itemDoc =
    C.record Item ItemDoc
        |> C.field "label" .label C.text
        |> C.field "done" .done C.bool
        |> C.build


type alias Board =
    { fixed : List Item
    , movable : List Item
    }


type alias BoardDoc =
    { fixed : Ref Board (C.ListK C.Fixed C.Nested Item) (List Item)
    , movable : Ref Board (C.ListK C.Movable C.Nested Item) (List Item)
    , schema : C.Schema C.Nested Board
    }


fixedList : C.Crdt (C.ListK C.Fixed C.Nested Item) (List Item) { index : Int -> Ref r (C.ListK mv C.Nested Item) (List Item) -> Ref r C.Nested Item }
fixedList =
    C.list itemDoc


movableList : C.Crdt (C.ListK C.Movable C.Nested Item) (List Item) { index : Int -> Ref r (C.ListK mv C.Nested Item) (List Item) -> Ref r C.Nested Item }
movableList =
    C.movableList itemDoc


refs : BoardDoc
refs =
    C.record Board BoardDoc
        |> C.field "fixed" .fixed fixedList
        |> C.field "movable" .movable movableList
        |> C.build


item : String -> Item
item label =
    { label = label, done = False }



-- HELPERS ---------------------------------------------------------------------


init : String -> Doc Board
init name =
    Doc.init (Id.replica name) refs.schema


{-| Run an edit, keeping the document on failure (the tests assert on reads, so a dropped
edit shows up as a mismatch rather than as a silent pass).
-}
edit : (Doc Board -> Result e (Doc Board)) -> Doc Board -> Doc Board
edit f doc =
    f doc |> Result.withDefault doc


{-| Ingest a wire payload the way an application's incoming-message handler does.
-}
apply : JE.Value -> Doc Board -> Doc Board
apply payload doc =
    Doc.decodeInto payload doc |> Result.withDefault doc


{-| A fresh replica caught up to `from` in full — the peer that then receives the
out-of-order deltas.
-}
peerOf : String -> Doc Board -> Doc Board
peerOf name from =
    apply (Doc.encode from) (init name)


{-| The labels/flags a `Board` reads as, so an expectation names what the user would see.
-}
boardOf : Doc Board -> List ( String, Bool )
boardOf doc =
    case Doc.read doc of
        Ok board ->
            (board.fixed ++ board.movable) |> List.map (\i -> ( i.label, i.done ))

        Err _ ->
            [ ( "READ FAILED", False ) ]


consistent : Doc Board -> Expect.Expectation
consistent doc =
    -- the invariant the pending set exists to protect: the incremental cache still equals
    -- a full re-materialize, so `readAt`/scrub and `read` cannot disagree.
    Doc.cacheConsistent doc |> Expect.equal True



-- A SCRIPT WITH ONE WITHHELD INSERT -------------------------------------------


{-| Build the source history `A, then B, then edit B, then delete B` on a fixed list,
and hand back the three deltas after A plus the finished document. `insertB` is the one a
relay withholds; `editB` and `deleteB` are the ops that name the element it creates.
-}
type alias Script =
    { peerBase : Doc Board
    , insertB : JE.Value
    , editB : JE.Value
    , deleteB : JE.Value
    , final : Doc Board
    }


fixedScript : Script
fixedScript =
    let
        a0 =
            init "alice" |> edit (Edit.append refs.fixed (item "A"))

        a1 =
            a0 |> edit (Edit.append refs.fixed (item "B"))

        a2 =
            a1 |> edit (Edit.set (refs.fixed |> fixedList.index 1 |> C.at itemDoc.label) "beta")

        a3 =
            a2 |> edit (Edit.remove refs.fixed 1)
    in
    { peerBase = peerOf "bob" a0
    , insertB = Doc.encodeSince (Doc.version a0) a1
    , editB = Doc.encodeSince (Doc.version a1) a2
    , deleteB = Doc.encodeSince (Doc.version a2) a3
    , final = a3
    }


{-| The same shape on a **movable** list, where the delete needs no repair (a movable list
records deleted ids in a grow-only set) but the nested edit does. Stops after the edit.
-}
movableScript : Script
movableScript =
    let
        a0 =
            init "alice" |> edit (Edit.append refs.movable (item "A"))

        a1 =
            a0 |> edit (Edit.append refs.movable (item "B"))

        a2 =
            a1 |> edit (Edit.set (refs.movable |> movableList.index 1 |> C.at itemDoc.done) True)
    in
    { peerBase = peerOf "bob" a0
    , insertB = Doc.encodeSince (Doc.version a0) a1
    , editB = Doc.encodeSince (Doc.version a1) a2
    , deleteB = JE.null
    , final = a2
    }


suite : Test
suite =
    describe "ops delivered before the insert they name"
        [ test "an orphan delete is held back, then wins once the insert arrives" <|
            \_ ->
                -- The loud failure: with the delete dropped as a no-op, `B` reappeared the
                -- moment its insert showed up — and stayed, for every replica downstream.
                let
                    s =
                        fixedScript

                    orphaned =
                        s.peerBase |> apply s.deleteB

                    healed =
                        orphaned |> apply s.insertB
                in
                Expect.all
                    [ \_ -> boardOf orphaned |> Expect.equalLists [ ( "A", False ) ]
                    , \_ -> consistent orphaned
                    , \_ -> boardOf healed |> Expect.equalLists (boardOf s.final)
                    , \_ -> boardOf healed |> Expect.equalLists [ ( "A", False ) ]
                    , \_ -> consistent healed
                    ]
                    ()
        , test "an orphan nested edit is held back, then applies once the insert arrives" <|
            \_ ->
                -- The quiet failure, and the real reason to fix this: the edit into `B`
                -- used to vanish with nothing to retry it, so `B` arrived with its seeded
                -- label and the user's change was gone.
                let
                    s =
                        fixedScript

                    healed =
                        s.peerBase |> apply s.editB |> apply s.insertB
                in
                Expect.all
                    [ \_ -> boardOf healed |> Expect.equalLists [ ( "A", False ), ( "beta", False ) ]
                    , \_ -> consistent healed
                    ]
                    ()
        , test "a nested edit into a movable-list item survives the same reordering" <|
            \_ ->
                -- `MoveList.updateValue` has the same in-place hole as `Rga.updateElement`,
                -- reached through the same `IntoElem` target step.
                let
                    s =
                        movableScript

                    healed =
                        s.peerBase |> apply s.editB |> apply s.insertB
                in
                Expect.all
                    [ \_ -> boardOf healed |> Expect.equalLists [ ( "A", False ), ( "B", True ) ]
                    , \_ -> boardOf healed |> Expect.equalLists (boardOf s.final)
                    , \_ -> consistent healed
                    ]
                    ()
        , test "delivery order of the three deltas doesn't change the result" <|
            \_ ->
                -- The property the whole op log is built on, now extended to the ops that
                -- used to be order-sensitive. Every permutation must read alike.
                let
                    s =
                        fixedScript

                    deliver order =
                        List.foldl apply s.peerBase order |> boardOf

                    expected =
                        boardOf s.final
                in
                Expect.all
                    [ \_ -> deliver [ s.insertB, s.editB, s.deleteB ] |> Expect.equalLists expected
                    , \_ -> deliver [ s.deleteB, s.editB, s.insertB ] |> Expect.equalLists expected
                    , \_ -> deliver [ s.editB, s.deleteB, s.insertB ] |> Expect.equalLists expected
                    , \_ -> deliver [ s.deleteB, s.insertB, s.editB ] |> Expect.equalLists expected
                    ]
                    ()
        , test "re-delivering the withheld insert does not resurrect the deleted element" <|
            \_ ->
                -- Retrying a held-back op only converges if applying an insert twice is a
                -- no-op — which is why `Rga.put` keeps an existing tombstone.
                let
                    s =
                        fixedScript

                    healed =
                        s.peerBase
                            |> apply s.deleteB
                            |> apply s.insertB
                            |> apply s.insertB
                            |> apply s.deleteB
                in
                Expect.all
                    [ \_ -> boardOf healed |> Expect.equalLists [ ( "A", False ) ]
                    , \_ -> consistent healed
                    ]
                    ()
        , test "compaction keeps a still-pending op instead of folding it away" <|
            \_ ->
                -- `compact` folds ops below the cut into `base`; folding one that couldn't
                -- apply would discard it permanently, so it stays in the store and is still
                -- there to retry.
                let
                    s =
                        fixedScript

                    pendingThenCompacted =
                        s.peerBase
                            |> apply s.editB
                            |> (\d -> Doc.compact (Doc.version d) d)

                    healed =
                        pendingThenCompacted |> apply s.insertB
                in
                Expect.all
                    [ \_ -> consistent pendingThenCompacted
                    , \_ -> boardOf healed |> Expect.equalLists [ ( "A", False ), ( "beta", False ) ]
                    , \_ -> consistent healed
                    ]
                    ()
        , test "forking a document with a pending op recomputes what is pending" <|
            \_ ->
                -- A checkout re-folds from `base`, so the branch derives its own pending set
                -- rather than inheriting one computed against a different op set.
                let
                    s =
                        fixedScript

                    branch =
                        s.peerBase
                            |> apply s.editB
                            |> (\d -> Doc.fork (Id.replica "branch") d)
                            |> apply s.insertB
                in
                Expect.all
                    [ \_ -> boardOf branch |> Expect.equalLists [ ( "A", False ), ( "beta", False ) ]
                    , \_ -> consistent branch
                    ]
                    ()
        ]
