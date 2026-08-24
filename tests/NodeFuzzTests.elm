module NodeFuzzTests exposing (suite)

{-| Property tests over arbitrary — including malformed — `Node` values.

This suite replaces the old `NodeMergeTests`, which fuzzed the semilattice laws of a
state-level `Node.merge`. That merge was removed: documents converge by op-union plus
deterministic replay (`Crdt.OpLog`), so it had become a parallel specification nothing
executed, free to drift from the shipping path without failing a test.

The fuzzer it depended on was worth keeping, though — it is the only source of
_adversarial_ `Node` values in the suite, and a CRDT decodes state straight off the
network. So the same generator now points at the paths that do ship:

  - the JSON codec (`Crdt.Json`), which is where untrusted bytes become a `Node`;
  - the ordering walk (`Rga.toElementsInOrder`), which must terminate and stay total
    even when anchors form cycles — an earlier version hung the whole test runner on
    exactly this input;
  - `compactTombstones`, the GC rewrite, whose contract is that the visible value is
    unchanged.

-}

import Crdt.Id.Internal as Id
import Crdt.Json as Json
import Crdt.Node as Node exposing (Node)
import Crdt.Rga as Rga
import Dict
import Expect
import Fuzz
import Helpers exposing (fuzzElements, fuzzNode)
import Json.Decode as JD
import Test exposing (Test, describe, fuzz, fuzz2)


{-| Keep the first element carrying each id, so the input is a genuine element **set**.
`Rga.fromElements` resolves a repeated id by last-one-wins, which would otherwise make
"same set, different insertion order" false by construction.
-}
dedupById : List (Rga.Element Node) -> List (Rga.Element Node)
dedupById els =
    els
        |> List.foldl
            (\el ( seen, acc ) ->
                let
                    key =
                        Id.opIdToString el.id
                in
                if Dict.member key seen then
                    ( seen, acc )

                else
                    ( Dict.insert key () seen, el :: acc )
            )
            ( Dict.empty, [] )
        |> Tuple.second
        |> List.reverse


ids : Rga.Rga Node -> List String
ids rga =
    Rga.toElementsInOrder rga |> List.map (.id >> Id.opIdToString)


{-| Apply a batch of inserts the way the op-log fold does — one `Rga.put` per `InsertElem`
(`Crdt.OpLog.applyOp`) — rather than through `fromElements`, so arrival order is a real
variable and a re-delivery is a real second `put`.
-}
inserts : List (Rga.Element Node) -> Rga.Rga Node
inserts =
    List.foldl Rga.put Rga.empty


{-| Sort keys, used to permute a list whose length is only known at apply time (there is no
shuffle fuzzer). Ties keep their original relative order, so equal keys are the identity
permutation and distinct ones can express any order. Same convention as
`tests/DeliveryOrderTests.elm`.
-}
order : Fuzz.Fuzzer (List Int)
order =
    Fuzz.listOfLengthBetween 0 40 (Fuzz.intRange 0 12)


permute : List Int -> List a -> List a
permute keys xs =
    List.map2 Tuple.pair (keys ++ List.repeat (List.length xs) 0) xs
        |> List.indexedMap (\i ( k, x ) -> ( ( k, i ), x ))
        |> List.sortBy Tuple.first
        |> List.map Tuple.second


suite : Test
suite =
    describe "Node / Rga — properties over arbitrary (incl. malformed) values"
        [ fuzz fuzzNode "JSON round-trips losslessly" <|
            \node ->
                -- Nothing about ordering, presence or causality may be dropped on the
                -- wire, or convergence breaks (`Crdt.Json`'s module doc). Structural
                -- equality is the convergence oracle, so `==` is the right assertion.
                Json.encodeNode node
                    |> JD.decodeValue Json.nodeDecoder
                    |> Expect.equal (Ok node)
        , fuzz fuzzElements "the ordering walk is total: every element appears exactly once" <|
            \els ->
                -- Anchors may cycle, point at themselves, or dangle. The walk must
                -- still terminate (a hang here takes the runner down, not just the
                -- test) and must neither drop nor duplicate an element: dropping one
                -- loses data, duplicating one breaks `==` as a convergence oracle.
                let
                    rga =
                        Rga.fromElements (dedupById els)
                in
                List.length (Rga.toElementsInOrder rga)
                    |> Expect.equal (List.length (Rga.elements rga))
        , fuzz2 fuzzElements order "the visible order is a pure function of the element set" <|
            \els keys ->
                -- The invariant the whole design rests on: order is derived from the
                -- set, never from arrival order, so replicas that received the same
                -- inserts in different orders read alike.
                --
                -- Over an ARBITRARY permutation, not just the reversal: reversing is one
                -- permutation out of n!, and it is a symmetric one — a comparison that got
                -- its operands the wrong way round is invisible to it. Applied with
                -- `Rga.put`, which is what the op-log fold actually calls per `InsertElem`.
                let
                    set =
                        dedupById els
                in
                inserts (permute keys set)
                    |> ids
                    |> Expect.equal (ids (inserts set))
        , fuzz2 fuzzElements order "re-applying the same elements changes nothing" <|
            \els keys ->
                -- Ops are re-delivered freely — a peer gossips, a relay replays its table,
                -- a reconnect re-sends a whole store — so applying an element a replica
                -- already holds must be a no-op, in any order, including interleaved with
                -- the originals. `==` on the whole `Rga` rather than on the read: a
                -- duplicate hiding in the store would eventually surface through some other
                -- path even if this read happened to hide it.
                let
                    set =
                        dedupById els
                in
                inserts (set ++ permute keys set)
                    |> Expect.equal (inserts set)
        , fuzz fuzzElements "compactTombstones preserves the visible order and ids" <|
            \els ->
                -- The GC rewrite's contract: tombstones go, the visible value is
                -- byte-for-byte identical, and survivors keep their ids so cursors
                -- still resolve.
                let
                    rga =
                        Rga.fromElements (dedupById els)

                    liveIds =
                        Rga.toElementsInOrder rga
                            |> List.filter (not << .deleted)
                            |> List.map (.id >> Id.opIdToString)
                in
                Expect.all
                    [ \compacted -> Expect.equal liveIds (ids compacted)
                    , \compacted -> Expect.equal (Rga.toList rga) (Rga.toList compacted)
                    ]
                    (Rga.compactTombstones identity rga)
        , fuzz (Fuzz.pair fuzzNode fuzzNode) "maxCounter dominates every stamp a decoded node carries" <|
            \( x, y ) ->
                -- Clock safety: a replica advances past `maxCounter` of what it takes
                -- in, so it can never re-mint an id already in use. Monotonicity under
                -- nesting is the part that is easy to break by forgetting a case — a
                -- child's stamps must always count toward its parent's maximum.
                let
                    nested =
                        Node.mapFromEntries
                            (Dict.fromList
                                [ ( "x", Node.entry (Id.opId 0 (Id.replica "z")) True x )
                                , ( "y", Node.entry (Id.opId 0 (Id.replica "z")) True y )
                                ]
                            )
                in
                Node.maxCounter nested
                    |> Expect.atLeast (max (Node.maxCounter x) (Node.maxCounter y))
        ]
