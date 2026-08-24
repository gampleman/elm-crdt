module TreePropertyTests exposing (suite)

{-| **The movable tree's structural invariants, at the level the read path breaks them.**

Tree edits are already fuzzed for convergence (`Helpers.Edits` drives `AddRoot`/`AddUnder`/
`MoveNode`/`RemoveNode`, and every schedule property compares `render`). But `render`
compares two documents to _each other_: two replicas that both drop a node, or both loop
forever, agree perfectly. Convergence is the wrong oracle for the claims `Crdt.Tree` makes
on its own —

> a skipped move leaves its node at its previous parent (**nothing vanishes**); the
> resolved structure is **acyclic** whatever the move-set says; a deleted node takes
> **exactly** its subtree; and the read is a pure function of the grow-only representation.

— and three of those are only visible from _inside_ the tree, so this module tests
`Crdt.Tree.Internal` directly. That is also where the ops land: `OpLog.treeMove` is a
three-line wrapper choosing `Tree.move` (a creation, which carries a payload seed) or
`Tree.moveOnly` (a re-parent, which does not), and `DeleteElem` on a tree is `Tree.delete`.
A script of those three calls _is_ a fold of tree ops.

Two groups, because there are two kinds of input and they justify different claims:

  - **An arbitrary tree** (`Helpers.fuzzTreeValue`: move-set, payload table and tombstones
    generated independently, ids from a pool of three so parent cycles are common). This
    comes off the wire, so the read must be **total** — terminate, and produce a forest,
    even though nothing in the representation had to be well-formed. It is also where the
    hand-optimized read paths get checked against the slow ones they duplicate: `toForest`
    builds a child index in one pass and must agree with `roots`/`childrenOf`, and
    `lastChildPos` folds the resolved map once and must agree with `childrenOf` +
    `siblingPos`. Nothing but a property pins those together.
  - **A scripted tree** — a fuzzed list of concrete moves and deletes, which is what a
    replica actually holds. Only here can the _semantic_ claims be stated, because they
    quantify over the ops: what was created is what is readable, deleting takes a subtree,
    and any delivery order reads the same.

-}

import Crdt.Frac as Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.Tree.Internal as Tree exposing (Tree)
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Helpers exposing (fuzzTreeValue)
import Set exposing (Set)
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "the movable tree"
        [ describe "an arbitrary move-set still reads as a forest"
            [ fuzz fuzzTreeValue "the read terminates and every node appears once" <|
                \tree ->
                    -- Termination is the claim. `resolve` folds moves from an empty map,
                    -- skipping any that would make a node its own ancestor, so the resolved
                    -- map is inductively acyclic — which is the *only* reason `toForest`'s
                    -- recursive walk is guaranteed to stop. A cycle here would not be a
                    -- wrong answer; it would be a hung tab, from a message a peer sent.
                    -- A repeated id is the same bug seen from the other side (a diamond in
                    -- the resolved parent map), so it is the assertion.
                    let
                        ids =
                            forestIds (Tree.toForest Just tree)
                    in
                    List.length ids |> Expect.equal (Set.size (keysOf ids))
            , fuzz fuzzTreeValue "every node in the forest is live and has a payload" <|
                \tree ->
                    -- The three components are generated independently, so a move naming a
                    -- node with no payload, and a payload for a node that no move places,
                    -- are both routine. Either one surfacing as an item would mean the read
                    -- invented a node.
                    Tree.toForest Just tree
                        |> forestIds
                        |> List.filter (\id -> Tree.get id tree == Nothing)
                        |> List.map Id.opIdToString
                        |> Expect.equalLists []
            , fuzz fuzzTreeValue "the forest's shape is what `roots`/`childrenOf` report" <|
                \tree ->
                    -- Drift check. `toForest` buckets every node under its parent in one
                    -- pass (O(N log N)); `roots`/`childrenOf` re-scan the resolved map per
                    -- parent (the older, obvious version) and are what `Ref`-based edits
                    -- resolve indices through. They duplicate the admission rule — live,
                    -- payload present, no tombstoned ancestor — so they can disagree, and
                    -- then a write lands at a different index than the one the user clicked.
                    Expect.equalLists (childListMismatches tree) []
            , fuzz fuzzTreeValue "every node's forest parent is its resolved parent" <|
                \tree ->
                    -- The other direction of the same shape: `parentOf` is how a nested edit
                    -- climbs back out, so it has to name the parent the read displayed.
                    Tree.toForest Just tree
                        |> forestParents Nothing
                        |> List.filter (\( child, parent ) -> Tree.parentOf child tree /= parent)
                        |> List.map (Tuple.first >> Id.opIdToString)
                        |> Expect.equalLists []
            , fuzz fuzzTreeValue "every node's resolved parent chain terminates" <|
                \tree ->
                    -- Acyclicity is not a nicety here, it is load-bearing, and the forest
                    -- walk above does not actually establish it: a cycle can never be
                    -- *reached* from a root (every member's parent is another member), so
                    -- `toForest` skips it silently. Two read paths climb *up* the same map
                    -- and would not — `wouldCycle`'s own ancestor walk, which carries a
                    -- defensive `seen` set, and `ancestorTombstoned`, which carries nothing
                    -- and relies entirely on `resolve` having skipped the cycle-forming
                    -- moves. So this asserts it over every node any move names, readable or
                    -- not, with the climb bounded: an unbounded one would hang the run
                    -- instead of failing it.
                    Expect.equalLists (nonTerminatingClimbs tree) []
            , fuzz fuzzTreeValue "`lastChildPos` is the position of the last sibling" <|
                \tree ->
                    -- The append hot path. It resolves the move-set once and takes the max
                    -- `pos` directly, instead of `childrenOf` + `siblingPos` (two resolves) —
                    -- so it re-implements the admission rule a third time. If it reads high,
                    -- appends pile up beyond the end; if it reads low, `Frac.between` is
                    -- handed a gap that is already occupied.
                    --
                    -- Compared *by `Frac.compare`*, not structurally: `Frac [5]` and
                    -- `Frac [5,0]` are the same position with different digits, and which one
                    -- comes back depends on which sibling was visited first.
                    Expect.equalLists (lastChildMismatches tree) []
            ]
        , describe "a script of moves and deletes"
            [ fuzz2 fuzzScript order "the forest is a function of the op set, not its order" <|
                \script keys ->
                    -- The tree is derived at read time by re-folding the move-set sorted by
                    -- `moveOp`, and all three components are grow-only — which is *why*
                    -- `merge` can be a plain union and a late-arriving move needs no undo.
                    -- Stated as a property: the same ops in any order read the same. (Each
                    -- op here already carries its own seed/no-seed decision, exactly as the
                    -- emitting replica made it, so permuting delivery cannot change which
                    -- move is a creation.)
                    Expect.equal
                        (forestOf (build (permute keys script)))
                        (forestOf (build script))
            , fuzz fuzzScript "the read paths still agree on a tree with real siblings" <|
                \script ->
                    -- The same two drift checks as above, over the *populous* input. Both
                    -- sources are needed and neither subsumes the other: an arbitrary
                    -- move-set covers representations no replica would build, but with ids
                    -- drawn from a pool of three it hardly ever lands two payload-bearing
                    -- siblings under one live parent — and a sibling *group* is the only
                    -- place a comparison can be backwards. (Verified: inverting
                    -- `lastChildPos`'s max to a min passes the arbitrary fuzzer and fails
                    -- this one.)
                    Expect.equalLists (childListMismatches (build script) ++ lastChildMismatches (build script)) []
            , fuzz fuzzMoves "a cycle-forming move is skipped — the node does not vanish" <|
                \moves ->
                    -- The one place this tree drops an op on purpose. Skipping is safe only
                    -- because the node keeps its *previous* position; if a skip could leave a
                    -- node unplaced, concurrent re-parents would delete data. With no
                    -- deletes in the script, "readable" must mean exactly "created".
                    Expect.equalLists
                        (build moves |> forestOf |> forestIds |> List.map Id.opIdToString |> List.sort)
                        (createdIds moves)
            , test "two nodes swapped under each other keep both, one relation" <|
                \_ ->
                    -- The concrete shape of the above: A moved under B while B moved under A.
                    -- Sorting by `moveOp` makes the later move the one that loses, on every
                    -- replica. Written out because the property can only say "nothing
                    -- vanished", not "and the result is the sensible one".
                    let
                        tree =
                            build
                                [ MoveOp { moveOp = mv 1, child = node 0, parent = Nothing, pos = pos 1, seed = Just "a" }
                                , MoveOp { moveOp = mv 2, child = node 1, parent = Nothing, pos = pos 2, seed = Just "b" }
                                , MoveOp { moveOp = mv 3, child = node 0, parent = Just (node 1), pos = pos 1, seed = Nothing }
                                , MoveOp { moveOp = mv 4, child = node 1, parent = Just (node 0), pos = pos 1, seed = Nothing }
                                ]
                    in
                    Expect.all
                        [ \t -> Tree.roots t |> Expect.equalLists [ node 1 ]
                        , \t -> Tree.childrenOf (node 1) t |> Expect.equalLists [ node 0 ]
                        , \t -> Tree.childrenOf (node 0) t |> Expect.equalLists []
                        ]
                        tree
            , test "the winner is decided by `moveOp` order, not the move-set's key order" <|
                \_ ->
                    -- `resolve` re-sorts the move-set's values with `compareOpId` before
                    -- folding, even though the `Dict` already hands them over in key order.
                    -- That is not redundant: the key is `Id.opIdToString`, so the two orders
                    -- agree for counters 1–9 and then diverge — `"10@m"` sorts before
                    -- `"2@m"`. This scenario straddles that boundary on purpose (the swap
                    -- above, with single-digit counters, converges the same way either
                    -- way), because "the tree is right until the tenth op" is the shape of
                    -- bug a suite of small fixtures cannot see.
                    let
                        tree =
                            build
                                [ MoveOp { moveOp = mv 9, child = node 0, parent = Nothing, pos = pos 1, seed = Just "a" }
                                , MoveOp { moveOp = mv 10, child = node 1, parent = Nothing, pos = pos 2, seed = Just "b" }
                                , MoveOp { moveOp = mv 11, child = node 0, parent = Just (node 1), pos = pos 1, seed = Nothing }
                                , MoveOp { moveOp = mv 12, child = node 1, parent = Just (node 0), pos = pos 1, seed = Nothing }
                                ]
                    in
                    Expect.all
                        [ \t -> Tree.roots t |> Expect.equalLists [ node 1 ]
                        , \t -> Tree.childrenOf (node 1) t |> Expect.equalLists [ node 0 ]
                        ]
                        tree
            , fuzz fuzzMoves "deleting a node removes exactly its subtree" <|
                \moves ->
                    -- Delete-wins and recursive, but only *through the read*: the tombstone
                    -- names one node, and its descendants disappear because the walk never
                    -- reaches them. So an off-by-one in that reasoning is either an orphan
                    -- promoted to a root (a deleted node's children reappearing at the top
                    -- level, which is what `ancestorTombstoned` exists to prevent) or a
                    -- sibling taken down with it.
                    Expect.equalLists (deleteMismatches moves) []
            , fuzz fuzzMoves "a deleted node's subtree is inert to every read path" <|
                \moves ->
                    -- Dropping the subtree from `toForest` is not enough, because a `Ref`
                    -- holds a node's `OpId` and a peer can delete that node between the ref
                    -- being taken and being used. So `childrenOf` gets called on an id whose
                    -- ancestor is tombstoned as a matter of course, and if it answers with
                    -- children, an index resolves inside a subtree the read has already
                    -- dropped and the write lands where nobody can see it. That is what
                    -- `childIdsOf`'s ancestor climb is for — `toForest` gets the same rule
                    -- for free by never descending through a dead node, so the two paths are
                    -- only equivalent while the climb is there.
                    --
                    -- `get` is deliberately narrower and not asserted here: it checks the
                    -- node's own tombstone only, so a nested edit already in flight into a
                    -- doomed subtree still resolves rather than erroring.
                    Expect.equalLists (staleChildMismatches moves) []
            , fuzz fuzzScript "`maxCounter` covers every id in the tree" <|
                \script ->
                    -- Not cosmetic: it is how a merging replica advances its clock past what
                    -- it just received. An id it fails to see is a counter it can re-mint,
                    -- and a duplicated `OpId` is a node two ops disagree about forever.
                    let
                        tree =
                            build script
                    in
                    Tree.maxCounter (always 0) tree
                        |> Expect.atLeast (List.foldl (max << counterOf) 0 script)
            ]
        ]



-- READING A FOREST ------------------------------------------------------------


forestOf : Tree String -> Tree.Forest String
forestOf =
    Tree.toForest Just


forestIds : Tree.Forest a -> List OpId
forestIds =
    List.concatMap (\item -> Tree.itemId item :: forestIds (Tree.itemChildren item))


{-| Every `( node, its parent in the forest )` pair, from a walk of the nesting itself.
-}
forestParents : Maybe OpId -> Tree.Forest a -> List ( OpId, Maybe OpId )
forestParents parent =
    List.concatMap
        (\item ->
            ( Tree.itemId item, parent )
                :: forestParents (Just (Tree.itemId item)) (Tree.itemChildren item)
        )


keysOf : List OpId -> Set String
keysOf =
    List.map Id.opIdToString >> Set.fromList


{-| Every parent slot the tree has children under: the true root, plus every node the read
surfaced. Restricted to nodes _in the forest_ on purpose — `lastChildPos` and `childrenOf`
genuinely differ under a node whose ancestor is tombstoned (`lastChildPos` skips the
ancestor climb), and no caller ever appends there, because such a parent isn't readable.
-}
parentSlots : Tree a -> List (Maybe OpId)
parentSlots tree =
    Nothing :: List.map Just (forestIds (Tree.toForest Just tree))


childListMismatches : Tree a -> List String
childListMismatches tree =
    let
        forest =
            Tree.toForest Just tree

        atSlot slot =
            case slot of
                Nothing ->
                    ( Tree.roots tree, List.map Tree.itemId forest )

                Just id ->
                    ( Tree.childrenOf id tree
                    , itemAt id forest |> Maybe.map (Tree.itemChildren >> List.map Tree.itemId) |> Maybe.withDefault []
                    )
    in
    parentSlots tree
        |> List.filterMap
            (\slot ->
                let
                    ( viaChildrenOf, viaForest ) =
                        atSlot slot
                in
                if viaChildrenOf == viaForest then
                    Nothing

                else
                    Just ("childrenOf@" ++ slotName slot)
            )


lastChildMismatches : Tree a -> List String
lastChildMismatches tree =
    let
        childrenAt slot =
            case slot of
                Nothing ->
                    Tree.roots tree

                Just id ->
                    Tree.childrenOf id tree

        samePos a b =
            case ( a, b ) of
                ( Just x, Just y ) ->
                    Frac.compare x y == EQ

                _ ->
                    a == b
    in
    parentSlots tree
        |> List.filter
            (\slot ->
                childrenAt slot
                    |> List.reverse
                    |> List.head
                    |> Maybe.andThen (\lastChild -> Tree.siblingPos lastChild tree)
                    |> samePos (Tree.lastChildPos slot tree)
                    |> not
            )
        |> List.map (\slot -> "lastChildPos@" ++ slotName slot)


{-| Every node named by a move whose `parentOf` chain does not reach a root within one step
per move. The bound is what turns a cycle into a failure instead of a hang.
-}
nonTerminatingClimbs : Tree a -> List String
nonTerminatingClimbs tree =
    let
        limit =
            Dict.size (Tree.moves tree) + 1
    in
    Tree.moves tree
        |> Dict.values
        |> List.map .child
        |> List.filter (\child -> not (climbTerminates tree limit 0 child))
        |> List.map Id.opIdToString


climbTerminates : Tree a -> Int -> Int -> OpId -> Bool
climbTerminates tree limit steps id =
    if steps > limit then
        False

    else
        case Tree.parentOf id tree of
            Nothing ->
                True

            Just parent ->
                climbTerminates tree limit (steps + 1) parent


itemAt : OpId -> Tree.Forest a -> Maybe (Tree.Item a)
itemAt id forest =
    forest
        |> List.foldl
            (\item found ->
                if found /= Nothing then
                    found

                else if Tree.itemId item == id then
                    Just item

                else
                    itemAt id (Tree.itemChildren item)
            )
            Nothing


slotName : Maybe OpId -> String
slotName slot =
    slot |> Maybe.map Id.opIdToString |> Maybe.withDefault "<root>"



-- SCRIPTS ---------------------------------------------------------------------


{-| One tree op, already resolved the way `OpLog.treeMove`/`DeleteElem` hand them over: a
move carries a payload seed **iff** it is the creation of its node, and a delete names one
node. Concrete rather than an index-based instruction, so a script can be permuted without
its creations moving around.
-}
type Step
    = MoveOp { moveOp : OpId, child : OpId, parent : Maybe OpId, pos : Frac, seed : Maybe String }
    | DeleteOp OpId


build : List Step -> Tree String
build =
    List.foldl applyStep Tree.empty


applyStep : Step -> Tree String -> Tree String
applyStep step tree =
    case step of
        MoveOp m ->
            case m.seed of
                Just content ->
                    Tree.move m.moveOp m.child m.parent m.pos content tree

                Nothing ->
                    Tree.moveOnly m.moveOp m.child m.parent m.pos tree

        DeleteOp child ->
            Tree.delete child tree


{-| The ids a script _created_ — one per node that any move seeded, sorted, deduplicated.
With no deletes in the script this is exactly what the read must contain.
-}
createdIds : List Step -> List String
createdIds =
    List.filterMap
        (\step ->
            case step of
                MoveOp m ->
                    m.seed |> Maybe.map (\_ -> Id.opIdToString m.child)

                DeleteOp _ ->
                    Nothing
        )
        >> Set.fromList
        >> Set.toList


counterOf : Step -> Int
counterOf step =
    case step of
        MoveOp m ->
            max (Id.opIdCounter m.moveOp) (Id.opIdCounter m.child)

        DeleteOp child ->
            Id.opIdCounter child


{-| Delete each node the script produced, one at a time, and report every id whose removal
took the wrong set of nodes with it. The expected set is computed from the _undeleted_
forest, so it is independent of the code under test.
-}
deleteMismatches : List Step -> List String
deleteMismatches moves =
    let
        forest =
            forestOf (build moves)

        before =
            keysOf (forestIds forest)
    in
    forestIds forest
        |> List.filterMap
            (\victim ->
                let
                    subtree =
                        itemAt victim forest
                            |> Maybe.map (\item -> keysOf (forestIds [ item ]))
                            |> Maybe.withDefault Set.empty

                    after =
                        keysOf (forestIds (forestOf (build (moves ++ [ DeleteOp victim ]))))
                in
                if after == Set.diff before subtree then
                    Nothing

                else
                    Just (Id.opIdToString victim)
            )


{-| Delete each node the script produced and report every victim whose subtree still
answers `childrenOf` afterwards.
-}
staleChildMismatches : List Step -> List String
staleChildMismatches moves =
    let
        forest =
            forestOf (build moves)
    in
    forestIds forest
        |> List.filterMap
            (\victim ->
                let
                    after =
                        build (moves ++ [ DeleteOp victim ])

                    subtree =
                        itemAt victim forest
                            |> Maybe.map (\item -> forestIds [ item ])
                            |> Maybe.withDefault []
                in
                if List.all (\id -> Tree.childrenOf id after == []) subtree then
                    Nothing

                else
                    Just (Id.opIdToString victim)
            )



-- FUZZERS ---------------------------------------------------------------------


{-| A script over a **six-node pool**, so re-parents, reorders and cycles all collide
routinely. Move ops are minted in script order from one replica: a real move-set is sorted
by `moveOp`, and the sort is what decides which of two conflicting moves wins, so the
counters have to be meaningful rather than arbitrary.

Two things are resolved here rather than fuzzed, because a replica resolves them too, and
fuzzing them would generate ops no editor can emit while quietly falsifying the claims
above:

  - **A step's creation flag** — seeded iff the node has not been created earlier in the
    script, mirroring the edit layer's choice between "add a node" and "move an existing
    one". Deciding it once also lets the permutation property reorder a script without
    changing which move is a creation.
  - **A parent (and a delete target) is picked from the nodes already created.** You can
    only re-parent under a node you can see. Fuzzing the parent freely instead produces two
    shapes that are _legitimately_ unreadable — a node placed under an id that no move ever
    created (nothing walks down to it), and a creation whose parent is the node itself
    (cycle-forming, so skipped, so never placed at all) — and both would make "created ⇒
    readable" false for reasons that say nothing about the tree. Malformed structure is the
    first group's subject, where the claim is totality rather than semantics.

Cycles are still generated freely, which is the point: a _re-parent_ may name the child
itself or any of its descendants, since by then both are nodes the script can see.

-}
fuzzScript : Fuzzer (List Step)
fuzzScript =
    fuzzSteps (Fuzz.frequency [ ( 4, fuzzMoveStep ), ( 1, Fuzz.map DeleteSlot fuzzPick ) ])


{-| The same, with **no deletes** — needed by the properties that quantify over what
survives, since a tombstone would make "readable" and "created" legitimately differ.
-}
fuzzMoves : Fuzzer (List Step)
fuzzMoves =
    fuzzSteps fuzzMoveStep


{-| An instruction before its move op is minted, its creation flag decided, and its parent
resolved against what exists. `Int`s are picks, taken modulo the candidates.
-}
type Instruction
    = MoveSlot Int (Maybe Int) Int
    | DeleteSlot Int


fuzzMoveStep : Fuzzer Instruction
fuzzMoveStep =
    Fuzz.map3 MoveSlot (Fuzz.intRange 0 5) (Fuzz.maybe fuzzPick) (Fuzz.intRange 1 4)


fuzzPick : Fuzzer Int
fuzzPick =
    Fuzz.intRange 0 7


fuzzSteps : Fuzzer Instruction -> Fuzzer (List Step)
fuzzSteps instruction =
    Fuzz.listOfLengthBetween 0 14 instruction |> Fuzz.map mint


mint : List Instruction -> List Step
mint instructions =
    instructions
        |> List.foldl
            (\instruction ( created, acc ) ->
                case instruction of
                    MoveSlot childSlot parentPick posDigit ->
                        ( if List.member childSlot created then
                            created

                          else
                            created ++ [ childSlot ]
                        , MoveOp
                            { moveOp = mv (List.length acc + 1)
                            , child = node childSlot
                            , parent = parentPick |> Maybe.andThen (pickFrom created) |> Maybe.map node
                            , pos = pos posDigit
                            , seed =
                                if List.member childSlot created then
                                    Nothing

                                else
                                    Just ("n" ++ String.fromInt childSlot)
                            }
                            :: acc
                        )

                    DeleteSlot victimPick ->
                        case pickFrom created victimPick of
                            Just victim ->
                                ( created, DeleteOp (node victim) :: acc )

                            Nothing ->
                                -- nothing exists to delete yet
                                ( created, acc )
            )
            ( [], [] )
        |> Tuple.second
        |> List.reverse


pickFrom : List Int -> Int -> Maybe Int
pickFrom candidates i =
    if List.isEmpty candidates then
        Nothing

    else
        List.drop (modBy (List.length candidates) i) candidates |> List.head


node : Int -> OpId
node i =
    Id.opId (i + 1) (Id.replica "n")


mv : Int -> OpId
mv i =
    Id.opId i (Id.replica "m")


pos : Int -> Frac
pos digit =
    Frac.fromList [ digit * 50 ]



-- PERMUTATIONS ----------------------------------------------------------------


{-| Sort keys, used to permute a list whose length is only known at apply time (there is no
shuffle fuzzer). Ties keep their original order, so equal keys are the identity permutation
and distinct ones can express any order. Same trick as `tests/DeliveryOrderTests.elm`.
-}
order : Fuzzer (List Int)
order =
    Fuzz.listOfLengthBetween 0 14 (Fuzz.intRange 0 8)


permute : List Int -> List a -> List a
permute keys xs =
    let
        padded =
            keys ++ List.repeat (List.length xs) 0
    in
    List.map2 Tuple.pair padded xs
        |> List.indexedMap (\i ( k, x ) -> ( ( k, i ), x ))
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
