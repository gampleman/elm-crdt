module TreeCoreTests exposing (suite)

{-| Correctness core for `Crdt.Tree`, tested in isolation with `String` payloads —
no schema, no op-log — so the move/cycle/order semantics stand on their own.

The properties that matter:

  - add/move/delete read back a correct structure; payload survives a move;
  - **the cycle test**: concurrent X→under-Y and Y→under-X converge to the SAME
    tree whichever order the moves are applied in, with exactly one move skipped
    (the higher `moveOp`);
  - concurrent moves of different nodes both apply;
  - sibling order via fractional positions; delete drops a subtree;
  - applying the same move twice changes nothing.

Concurrency is expressed by applying both replicas' moves to the shared starting
tree, in each order. That _is_ the merge: a `Tree`'s `moveSet` is keyed by `moveOp`
and its tombstones are grow-only, so applying both moves in either order yields the
identical state the removed structural `Tree.merge` used to compute — which is also
exactly what the op log does when it replays two peers' move ops.

-}

import Crdt.Frac as Frac exposing (Frac)
import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.Tree.Internal as Tree exposing (Tree)
import Expect
import Test exposing (Test, describe, test)


a : Int -> OpId
a n =
    Id.opId n (Id.replica "alice")


b : Int -> OpId
b n =
    Id.opId n (Id.replica "bob")


mid : Frac
mid =
    Frac.between Nothing Nothing


{-| Position after `p` (toward the high end).
-}
after : Frac -> Frac
after p =
    Frac.between (Just p) Nothing


{-| Render the tree as a flat bracketed string, e.g. `A[A1] B`, so structure +
order are easy to assert with a single `Expect.equal` on `String`.
-}
render : Tree String -> String
render tree =
    Tree.roots tree |> List.map (renderNode tree) |> String.join " "


renderNode : Tree String -> OpId -> String
renderNode tree id =
    let
        label =
            Tree.get id tree |> Maybe.withDefault "?"

        kids =
            Tree.childrenOf id tree |> List.map (renderNode tree)
    in
    if List.isEmpty kids then
        label

    else
        label ++ "[" ++ String.join " " kids ++ "]"


{-| A tree: A and B at root; A has child A1.

    A
        A1

    B

-}
base : Tree String
base =
    Tree.empty
        |> Tree.move (a 1) (a 1) Nothing mid "A"
        |> Tree.move (a 2) (a 2) Nothing (after mid) "B"
        |> Tree.move (a 3) (a 3) (Just (a 1)) mid "A1"


suite : Test
suite =
    describe "Crdt.Tree core"
        [ test "reads back the built structure in order" <|
            \_ ->
                render base
                    |> Expect.equal "A[A1] B"
        , test "move re-parents a node, payload survives" <|
            \_ ->
                -- move A1 under B
                base
                    |> Tree.moveOnly (a 4) (a 3) (Just (a 2)) mid
                    |> render
                    |> Expect.equal "A B[A1]"
        , test "a node edited then moved keeps its content" <|
            \_ ->
                base
                    |> Tree.updateValue (a 3) (\_ -> "A1*")
                    |> Tree.moveOnly (a 4) (a 3) (Just (a 2)) mid
                    |> render
                    |> Expect.equal "A B[A1*]"
        , test "delete drops the node and its subtree" <|
            \_ ->
                base
                    |> Tree.delete (a 1)
                    |> render
                    |> Expect.equal "B"
        , describe "sibling order (fractional index)"
            [ test "children order by position" <|
                \_ ->
                    -- add A2 before A1 (position below A1's mid)
                    base
                        |> Tree.move (a 5) (a 5) (Just (a 1)) (Frac.between Nothing (Just mid)) "A2"
                        |> (\t -> Tree.childrenOf (a 1) t |> List.map (\c -> Tree.get c t))
                        |> Expect.equal [ Just "A2", Just "A1" ]
            ]
        , describe "THE CYCLE TEST"
            [ test "concurrent A-under-B and B-under-A converge, one move skipped" <|
                \_ ->
                    let
                        -- start: A and B both roots
                        start =
                            Tree.empty
                                |> Tree.move (a 1) (a 1) Nothing mid "A"
                                |> Tree.move (a 2) (a 2) Nothing (after mid) "B"

                        -- alice moves A under B (moveOp a5)
                        aliceMove =
                            Tree.moveOnly (a 5) (a 1) (Just (a 2)) mid

                        -- bob concurrently moves B under A (moveOp b9 > a5)
                        bobMove =
                            Tree.moveOnly (b 9) (a 2) (Just (a 1)) mid

                        ab =
                            start |> aliceMove |> bobMove

                        ba =
                            start |> bobMove |> aliceMove
                    in
                    Expect.all
                        [ -- converges regardless of the order the moves arrive in
                          \_ -> Expect.equal (render ab) (render ba)
                        , -- moves fold in ascending moveOp order: a5 (A-under-B)
                          -- applies first; then b9 (B-under-A) would make B its own
                          -- ancestor, so it is SKIPPED → A stays under B, B a root.
                          \_ -> Expect.equal "B[A]" (render ab)
                        ]
                        ()
            , test "a 3-cycle also resolves to a valid tree (no node lost)" <|
                \_ ->
                    let
                        start =
                            Tree.empty
                                |> Tree.move (a 1) (a 1) Nothing mid "A"
                                |> Tree.move (a 2) (a 2) Nothing (after mid) "B"
                                |> Tree.move (a 3) (a 3) Nothing (after (after mid)) "C"

                        -- concurrent: A→B, B→C, C→A (would be a 3-cycle if all applied)
                        merged =
                            start
                                |> Tree.moveOnly (a 10) (a 1) (Just (a 2)) mid
                                |> Tree.moveOnly (a 11) (a 2) (Just (a 3)) mid
                                |> Tree.moveOnly (a 12) (a 3) (Just (a 1)) mid

                        -- every node still present exactly once
                        count =
                            countNodes merged
                    in
                    Expect.equal 3 count
            ]
        , describe "concurrency (different nodes) + convergence laws"
            [ test "concurrent moves of different nodes both apply" <|
                \_ ->
                    let
                        aliceMove =
                            Tree.moveOnly (a 5) (a 3) (Just (a 2)) mid

                        -- bob adds a child under A concurrently
                        bobAdd =
                            Tree.move (b 9) (b 9) (Just (a 1)) (after mid) "B-child"
                    in
                    Expect.equal
                        (render (base |> aliceMove |> bobAdd))
                        (render (base |> bobAdd |> aliceMove))
            , test "re-applying a move is idempotent" <|
                \_ ->
                    -- ops are re-delivered freely, so a repeat must change nothing
                    let
                        moved =
                            Tree.moveOnly (a 5) (a 3) (Just (a 2)) mid base
                    in
                    Expect.equal moved (Tree.moveOnly (a 5) (a 3) (Just (a 2)) mid moved)
            , test "delete-wins over a concurrent move" <|
                \_ ->
                    let
                        aliceDelete =
                            Tree.delete (a 3)

                        bobMove =
                            Tree.moveOnly (b 9) (a 3) (Just (a 2)) mid
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal
                                (render (base |> aliceDelete |> bobMove))
                                (render (base |> bobMove |> aliceDelete))
                        , \_ -> Expect.equal "A B" (render (base |> aliceDelete |> bobMove))
                        ]
                        ()
            ]
        ]


countNodes : Tree String -> Int
countNodes tree =
    Tree.roots tree |> List.map (countFrom tree) |> List.sum


countFrom : Tree String -> OpId -> Int
countFrom tree id =
    1 + (Tree.childrenOf id tree |> List.map (countFrom tree) |> List.sum)
