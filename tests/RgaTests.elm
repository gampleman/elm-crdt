module RgaTests exposing (suite)

{-| RGA-specific correctness: deterministic ordering independent of the order ops
arrive in, tombstone semantics, and the classic concurrent-insert-at-same-origin case.

These used to be written against a state-level `Rga.merge`, which no longer exists —
documents converge by op-union plus replay (see `Crdt.OpLog`). The properties are
unchanged; they are now expressed the way the library actually works. Two replicas'
divergent states correspond to two sets of insert/delete ops, and merging them is
**applying the union of those ops** — so "merge order doesn't matter" becomes
"application order doesn't matter", asserted by applying each union both ways round.

-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Rga as Rga
import Expect
import Test exposing (Test, describe, test)


alice : String -> Int -> OpId
alice _ n =
    Id.opId n (Id.replica "alice")


bob : String -> Int -> OpId
bob _ n =
    Id.opId n (Id.replica "bob")


{-| A single-character text element keyed by an OpId, anchored as a right-child of
`origin` (i.e. "inserted after origin" — classic RGA semantics; `Nothing` = head).
-}
charEl : OpId -> Maybe OpId -> Char -> Rga.Element Node
charEl id origin c =
    Rga.element id origin Rga.Right (Node.reg (PString (String.fromChar c)) id) False


{-| Apply a batch of insert ops, in the given order. This is what the op-log fold
does (`Crdt.OpLog.applyOp` → `Rga.put` per `InsertElem`), so it is the real
counterpart of the state-level union these tests used to perform.
-}
inserts : List (Rga.Element Node) -> Rga.Rga Node
inserts =
    List.foldl Rga.put Rga.empty


textOf : Rga.Rga Node -> String
textOf rga =
    Rga.toList rga
        |> List.filterMap
            (\node ->
                case Node.asPrim node of
                    Just (PString s) ->
                        Just s

                    _ ->
                        Nothing
            )
        |> String.concat


{-| Assert that a set of insert ops reads the same whichever order it is applied in
(forwards and reversed), and return that reading.
-}
orderIndependent : List (Rga.Element Node) -> String -> Expect.Expectation
orderIndependent els expected =
    Expect.all
        [ \_ -> Expect.equal expected (textOf (inserts els))
        , \_ -> Expect.equal expected (textOf (inserts (List.reverse els)))
        ]
        ()


suite : Test
suite =
    -- Fixtures on purpose, and the properties they'd generalize to live next door in
    -- `tests/NodeFuzzTests.elm` — "the visible order is a pure function of the element set"
    -- (over an arbitrary permutation of arbitrary, including malformed, elements), "the
    -- ordering walk is total", and "re-applying the same elements changes nothing".
    --
    -- What's left here is what a fuzz test cannot state: each of these pins a specific
    -- VALUE, and the value is the claim. "YoHi" and "Xba" are the tie-break *direction* —
    -- a property can only say the two orders agree, not which one is right, so an inverted
    -- comparison passes it and fails these. The tombstone cases name the exact surviving
    -- text. And the last two are named hazards rather than invariants: delete-before-insert
    -- resurrects (an obligation on the op log, not a property of `Rga`), and the 20k chain
    -- is a fixed-size regression guard whose whole point is that N is large.
    describe "Rga"
        [ test "application order does not affect resulting order" <|
            \_ ->
                -- alice types "Hi", bob types "Yo", both at the head, concurrently.
                -- Merging is applying both sets of inserts; either order must read alike.
                orderIndependent
                    [ charEl (alice "" 1) Nothing 'H'
                    , charEl (alice "" 2) (Just (alice "" 1)) 'i'
                    , charEl (bob "" 1) Nothing 'Y'
                    , charEl (bob "" 2) (Just (bob "" 1)) 'o'
                    ]
                    "YoHi"
        , test "concurrent inserts at the same origin converge to a deterministic order" <|
            \_ ->
                -- both insert a char right after X, concurrently
                orderIndependent
                    [ charEl (alice "" 1) Nothing 'X'
                    , charEl (alice "" 5) (Just (alice "" 1)) 'a'
                    , charEl (bob "" 5) (Just (alice "" 1)) 'b'
                    ]
                    "Xba"
        , test "re-applying the same insert op is idempotent" <|
            \_ ->
                -- ops are re-delivered freely, so applying one twice must change nothing
                let
                    els =
                        [ charEl (alice "" 1) Nothing 'A'
                        , charEl (alice "" 2) (Just (alice "" 1)) 'B'
                        ]

                    once =
                        inserts els
                in
                Expect.equal once (inserts (els ++ els))
        , test "a delete tombstones rather than removing, and the tombstone holds" <|
            \_ ->
                -- one replica still sees 'A', the other deleted it. The union of their
                -- ops is [insert A, delete A]; in causal order the deletion wins, and
                -- re-delivering the delete changes nothing.
                Expect.all
                    [ \_ ->
                        inserts [ charEl (alice "" 1) Nothing 'A' ]
                            |> Rga.delete (alice "" 1)
                            |> textOf
                            |> Expect.equal ""
                    , \_ ->
                        inserts [ charEl (alice "" 1) Nothing 'A' ]
                            |> Rga.delete (alice "" 1)
                            |> Rga.delete (alice "" 1)
                            |> textOf
                            |> Expect.equal ""
                    ]
                    ()
        , test "a tombstone still anchors a later insert (it is not just dead weight)" <|
            \_ ->
                -- 'B' anchors after 'A'; deleting 'A' must not move or lose 'B'
                inserts
                    [ charEl (alice "" 1) Nothing 'A'
                    , charEl (alice "" 2) (Just (alice "" 1)) 'B'
                    ]
                    |> Rga.delete (alice "" 1)
                    |> textOf
                    |> Expect.equal "B"
        , test "delete applied BEFORE its insert does not tombstone (causal order is load-bearing)" <|
            \_ ->
                -- Pins the invariant the op log owes `Rga`: `Rga.delete` on an id it has
                -- never seen is a no-op, and a later `put` of that element carries
                -- `deleted = False`, so the element comes back. Nothing checks this inside
                -- `Rga` — durability of a tombstone rests entirely on ops being applied in
                -- causal order (`OpLog.causalOrder`, and `addedOpsInOrder` for the
                -- incremental paths). A payload delivering a delete without its insert
                -- therefore resurrects; see the note in `design-docs/04-gc.md`.
                Rga.empty
                    |> Rga.delete (alice "" 1)
                    |> Rga.put (charEl (alice "" 1) Nothing 'A')
                    |> textOf
                    |> Expect.equal "A"
        , test "long origin-chain orders without stack overflow (regression)" <|
            \_ ->
                -- A list built by appending forms a linear origin-chain of depth
                -- N. The ordering walk must be iterative, not N-deep recursion —
                -- this used to overflow the stack around a few thousand elements.
                let
                    n =
                        20000

                    chain =
                        List.range 1 n
                            |> List.map
                                (\i ->
                                    charEl (alice "" i)
                                        (if i == 1 then
                                            Nothing

                                         else
                                            Just (alice "" (i - 1))
                                        )
                                        'x'
                                )
                in
                Rga.fromElements chain
                    |> Rga.toList
                    |> List.length
                    |> Expect.equal n
        ]
