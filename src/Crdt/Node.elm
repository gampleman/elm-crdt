module Crdt.Node exposing
    ( Node(..), Register, Prim(..), Entry, Increment, MovNode
    , reg, mapFromEntries, entry, seq, txt, counter, increment, mov
    , asPrim, asMap, presentEntries, asSeq, asTxt, asCounter, asMov
    , merge, maxCounter
    , restore
    , Element, RgaNode
    )

{-| The uniform replicated-state type that every CRDT document is made of, plus
its structural `merge`.

`Node` is a closed recursive union:

  - `Reg` — a last-write-wins register (a primitive leaf);
  - `Map` — a keyed collection used for **both** records and dicts. Each entry
    carries a **presence cell** (an LWW boolean with its own stamp) alongside its
    value, so that a removed key wins against a concurrent value edit by stamp.
    This is what makes dictionary key removal a well-behaved CRDT instead of an
    ambiguous set-vs-remove race;
  - `Seq` — a sequence, backed by `Crdt.Rga` at `Rga Node`;
  - `Txt` — collaborative text, also an `Rga Node` (of single-char registers);
  - `Cnt` — a PN-counter: a map of per-op signed contributions, summed to a value.

`merge` is monomorphic over `Node` and never touches the typed schema layer —
convergence correctness lives here and nowhere else.

@docs Node, Register, Prim, Entry, Increment, MovNode
@docs reg, mapFromEntries, entry, seq, txt, counter, increment, mov
@docs asPrim, asMap, presentEntries, asSeq, asTxt, asCounter, asMov
@docs merge, maxCounter
@docs restore
@docs Element, RgaNode

-}

import Crdt.Id as Id exposing (OpId)
import Crdt.MoveList as MoveList exposing (MoveList)
import Crdt.Rga as Rga exposing (Rga)
import Dict exposing (Dict)


{-| The replicated state.
-}
type Node
    = Reg Register
    | Map (Dict String Entry)
    | Seq RgaNode
    | Txt RgaNode
    | Cnt (Dict String Increment)
    | Mov MovNode


{-| A movable list (reorderable sequence) of `Node` content.
-}
type alias MovNode =
    MoveList Node


{-| One contribution to a counter: a signed `delta` tagged with the `OpId` of the
increment op that produced it. The counter's value is the sum of all deltas;
keying by `OpId` makes merge a `Dict.union` (each op is unique, so a shared key
carries an identical contribution) — idempotent, commutative, and a proper
PN-counter (concurrent `+1`/`+1` sum to 2, not LWW-collapse to 1).
-}
type alias Increment =
    { stamp : OpId
    , delta : Int
    }


{-| A map entry: a value plus an LWW presence cell. `present = False` is a key
tombstone; `stamp` is the last write to the presence bit.
-}
type alias Entry =
    { value : Node
    , present : Bool
    , stamp : OpId
    }


{-| An RGA whose elements carry `Node` content.
-}
type alias RgaNode =
    Rga Node


{-| An RGA element carrying `Node` content.
-}
type alias Element =
    Rga.Element Node


{-| A last-write-wins register: a primitive value tagged with the `OpId` that
last wrote it.
-}
type alias Register =
    { value : Prim
    , stamp : OpId
    }


{-| The primitive values a register can hold (the leaves of the JSON-like tree).
-}
type Prim
    = PNull
    | PBool Bool
    | PInt Int
    | PFloat Float
    | PString String



-- CONSTRUCTORS ---------------------------------------------------------------


{-| A register node from a primitive and the stamp that wrote it.
-}
reg : Prim -> OpId -> Node
reg value stamp =
    Reg { value = value, stamp = stamp }


{-| A map entry value with a presence stamp.
-}
entry : OpId -> Bool -> Node -> Entry
entry stamp present value =
    { value = value, present = present, stamp = stamp }


{-| A map from a dict of fully-formed entries.
-}
mapFromEntries : Dict String Entry -> Node
mapFromEntries =
    Map


{-| A sequence node.
-}
seq : RgaNode -> Node
seq =
    Seq


{-| A text node.
-}
txt : RgaNode -> Node
txt =
    Txt


{-| A counter node from its per-op contributions.
-}
counter : Dict String Increment -> Node
counter =
    Cnt


{-| A single counter contribution.
-}
increment : OpId -> Int -> Increment
increment stamp delta =
    { stamp = stamp, delta = delta }


{-| A movable-list node.
-}
mov : MovNode -> Node
mov =
    Mov



-- ACCESSORS ------------------------------------------------------------------


{-| Extract a primitive value, if this is a register.
-}
asPrim : Node -> Maybe Prim
asPrim node =
    case node of
        Reg r ->
            Just r.value

        _ ->
            Nothing


{-| Extract the raw entries, if this is a map.
-}
asMap : Node -> Maybe (Dict String Entry)
asMap node =
    case node of
        Map d ->
            Just d

        _ ->
            Nothing


{-| The present (non-tombstoned) key/value pairs of a map, in key order.
-}
presentEntries : Node -> List ( String, Node )
presentEntries node =
    case node of
        Map d ->
            Dict.toList d
                |> List.filter (\( _, e ) -> e.present)
                |> List.map (\( k, e ) -> ( k, e.value ))

        _ ->
            []


{-| Extract the array, if this is a sequence.
-}
asSeq : Node -> Maybe RgaNode
asSeq node =
    case node of
        Seq r ->
            Just r

        _ ->
            Nothing


{-| Extract the array, if this is text.
-}
asTxt : Node -> Maybe RgaNode
asTxt node =
    case node of
        Txt r ->
            Just r

        _ ->
            Nothing


{-| Extract the movable list, if this is one.
-}
asMov : Node -> Maybe MovNode
asMov node =
    case node of
        Mov ml ->
            Just ml

        _ ->
            Nothing


{-| The counter's current value: the sum of all its contributions. `Nothing` if
this node isn't a counter.
-}
asCounter : Node -> Maybe Int
asCounter node =
    case node of
        Cnt d ->
            Just (Dict.foldl (\_ inc acc -> acc + inc.delta) 0 d)

        _ ->
            Nothing



-- MERGE ----------------------------------------------------------------------


{-| Merge two nodes. Commutative, associative and idempotent.
-}
merge : Node -> Node -> Node
merge a b =
    case ( a, b ) of
        ( Reg ra, Reg rb ) ->
            Reg (mergeRegister ra rb)

        ( Map da, Map db ) ->
            Map (mergeMaps da db)

        ( Seq sa, Seq sb ) ->
            Seq (Rga.merge merge sa sb)

        ( Txt ta, Txt tb ) ->
            Txt (Rga.merge merge ta tb)

        ( Cnt ca, Cnt cb ) ->
            -- union of per-op contributions. In real use a shared key carries an
            -- identical contribution (one op mints one delta), so this just sums
            -- distinct contributions. On a key collision with differing deltas
            -- (only possible from corrupt/adversarial input) pick the larger delta
            -- deterministically, so merge stays commutative on ANY input.
            Cnt
                (Dict.merge
                    Dict.insert
                    (\k x y -> Dict.insert k (mergeIncrement x y))
                    Dict.insert
                    ca
                    cb
                    Dict.empty
                )

        ( Mov ma, Mov mb ) ->
            Mov (MoveList.merge merge ma mb)

        _ ->
            -- constructor mismatch: deterministic, order-independent tiebreak.
            -- Does not arise under a single shared schema.
            if rank a >= rank b then
                a

            else
                b



-- RESTORE --------------------------------------------------------------------


{-| Revert `current` to look like `old` (a checkpoint snapshot), as a fresh CRDT
edit rather than a merge.

`merge` cannot do this: `current` evolved from `old` by last-write-wins edits, so
every current value already carries a higher stamp and would always win — merging
the snapshot back is a no-op. `restore` instead re-asserts the snapshot's values
with **freshly minted, winning stamps**, so the revert propagates and converges
like any other edit. Keys/elements that were added after the snapshot are
tombstoned; keys absent from `current` but present in `old` are re-created.

New ids are minted from the context, which is threaded through and returned.

-}
restore : Id.Ctx -> Node -> Node -> ( Node, Id.Ctx )
restore ctx old current =
    case ( old, current ) of
        ( Reg ro, Reg rc ) ->
            if ro.value == rc.value then
                ( current, ctx )

            else
                let
                    ( stamp, ctx1 ) =
                        Id.nextId ctx
                in
                ( Reg { value = ro.value, stamp = stamp }, ctx1 )

        ( Map mo, Map mc ) ->
            restoreMap ctx mo mc

        ( Seq _, Seq _ ) ->
            restoreSequence ctx Seq old current

        ( Txt _, Txt _ ) ->
            restoreSequence ctx Txt old current

        ( Cnt co, Cnt cc ) ->
            -- revert a counter by adding one fresh contribution that cancels the
            -- difference, so it reads the old sum but stays a valid PN-counter.
            let
                diff =
                    sumIncrements co - sumIncrements cc
            in
            if diff == 0 then
                ( current, ctx )

            else
                let
                    ( stamp, ctx1 ) =
                        Id.nextId ctx
                in
                ( Cnt (Dict.insert (Id.opIdToString stamp) (increment stamp diff) cc), ctx1 )

        _ ->
            -- shape changed between versions (shouldn't happen under one schema):
            -- re-assert the old node wholesale with fresh stamps.
            reStamp ctx old


sumIncrements : Dict String Increment -> Int
sumIncrements =
    Dict.foldl (\_ inc acc -> acc + inc.delta) 0


restoreMap : Id.Ctx -> Dict String Entry -> Dict String Entry -> ( Node, Id.Ctx )
restoreMap ctx mo mc =
    let
        keys =
            (Dict.keys mo ++ Dict.keys mc)
                |> List.foldr
                    (\k acc ->
                        if List.member k acc then
                            acc

                        else
                            k :: acc
                    )
                    []

        step : String -> ( Dict String Entry, Id.Ctx ) -> ( Dict String Entry, Id.Ctx )
        step k ( acc, c ) =
            case ( Dict.get k mo, Dict.get k mc ) of
                ( Just eo, Just ec ) ->
                    -- present in both: restore the value; re-assert presence if
                    -- the key had been removed since the snapshot.
                    let
                        ( v, c1 ) =
                            restore c eo.value ec.value

                        ( present, stamp, c2 ) =
                            if eo.present /= ec.present then
                                let
                                    ( s, cc ) =
                                        Id.nextId c1
                                in
                                ( eo.present, s, cc )

                            else
                                ( ec.present, ec.stamp, c1 )
                    in
                    ( Dict.insert k { value = v, present = present, stamp = stamp } acc, c2 )

                ( Just eo, Nothing ) ->
                    -- key existed at the snapshot but not now: re-create it.
                    let
                        ( v, c1 ) =
                            reStamp c eo.value

                        ( s, c2 ) =
                            Id.nextId c1
                    in
                    ( Dict.insert k { value = v, present = eo.present, stamp = s } acc, c2 )

                ( Nothing, Just ec ) ->
                    -- key added after the snapshot: tombstone it.
                    let
                        ( s, c1 ) =
                            Id.nextId c
                    in
                    ( Dict.insert k { ec | present = False, stamp = s } acc, c1 )

                ( Nothing, Nothing ) ->
                    ( acc, c )

        ( entries, ctx1 ) =
            List.foldl step ( Dict.empty, ctx ) keys
    in
    ( Map entries, ctx1 )


{-| Restore a sequence/text node: tombstone every current element and re-insert
the snapshot's visible contents as a fresh chain of new elements. Convergent
because the new elements carry fresh ids and the tombstones are permanent.
`wrap` is `Seq` or `Txt`.
-}
restoreSequence : Id.Ctx -> (RgaNode -> Node) -> Node -> Node -> ( Node, Id.Ctx )
restoreSequence ctx wrap old current =
    let
        oldRga =
            rgaOf old

        currentRga =
            rgaOf current

        -- 1. tombstone everything currently visible
        tombstoned =
            List.foldl (\el acc -> Rga.delete el.id acc)
                currentRga
                (Rga.elements currentRga)

        -- 2. re-insert the snapshot's visible contents, deep-restamping each so
        -- nested ids are fresh too, chaining each after the previous one
        ( rebuilt, ctx1 ) =
            Rga.toList oldRga
                |> List.foldl
                    (\childOld ( acc, c, origin ) ->
                        let
                            ( child, c1 ) =
                                reStamp c childOld

                            ( acc1, c2 ) =
                                Rga.insertAfter c1 origin child acc
                        in
                        ( acc1, c2, Rga.lastVisibleId acc1 )
                    )
                    ( tombstoned, ctx, Nothing )
                |> (\( acc, c, _ ) -> ( acc, c ))
    in
    ( wrap rebuilt, ctx1 )


rgaOf : Node -> RgaNode
rgaOf node =
    case node of
        Seq r ->
            r

        Txt r ->
            r

        _ ->
            Rga.empty


{-| Deep-copy a node with entirely fresh ids/stamps, building new sequence
elements. Used to re-create content during a restore.
-}
reStamp : Id.Ctx -> Node -> ( Node, Id.Ctx )
reStamp ctx node =
    case node of
        Reg r ->
            let
                ( stamp, ctx1 ) =
                    Id.nextId ctx
            in
            ( Reg { r | stamp = stamp }, ctx1 )

        Map entries ->
            let
                ( newEntries, ctx1 ) =
                    Dict.foldl
                        (\k e ( acc, c ) ->
                            let
                                ( v, c1 ) =
                                    reStamp c e.value

                                ( s, c2 ) =
                                    Id.nextId c1
                            in
                            ( Dict.insert k { value = v, present = e.present, stamp = s } acc, c2 )
                        )
                        ( Dict.empty, ctx )
                        entries
            in
            ( Map newEntries, ctx1 )

        Seq r ->
            reStampRga ctx Seq r

        Txt r ->
            reStampRga ctx Txt r

        Cnt d ->
            -- re-stamp each contribution with a fresh id; the sum is preserved.
            let
                ( newCnt, ctx1 ) =
                    Dict.foldl
                        (\_ inc ( acc, c ) ->
                            let
                                ( s, c1 ) =
                                    Id.nextId c
                            in
                            ( Dict.insert (Id.opIdToString s) (increment s inc.delta) acc, c1 )
                        )
                        ( Dict.empty, ctx )
                        d
            in
            ( Cnt newCnt, ctx1 )

        Mov ml ->
            -- rebuild a fresh movable list from the old visible order, minting a
            -- new valueId + cell per item (content deep-restamped too).
            let
                ( rebuilt, ctx1, _ ) =
                    MoveList.toList ml
                        |> List.foldl
                            (\childOld ( acc, c, afterCell ) ->
                                let
                                    ( child, c1 ) =
                                        reStamp c childOld

                                    ( vid, c2 ) =
                                        Id.nextId c1
                                in
                                ( MoveList.insert vid afterCell child acc, c2, Just vid )
                            )
                            ( MoveList.empty, ctx, Nothing )
            in
            ( Mov rebuilt, ctx1 )


reStampRga : Id.Ctx -> (RgaNode -> Node) -> RgaNode -> ( Node, Id.Ctx )
reStampRga ctx wrap rga =
    let
        ( rebuilt, ctx1 ) =
            Rga.toList rga
                |> List.foldl
                    (\childOld ( acc, c, origin ) ->
                        let
                            ( child, c1 ) =
                                reStamp c childOld

                            ( acc1, c2 ) =
                                Rga.insertAfter c1 origin child acc
                        in
                        ( acc1, c2, Rga.lastVisibleId acc1 )
                    )
                    ( Rga.empty, ctx, Nothing )
                |> (\( acc, c, _ ) -> ( acc, c ))
    in
    ( wrap rebuilt, ctx1 )


mergeMaps : Dict String Entry -> Dict String Entry -> Dict String Entry
mergeMaps da db =
    Dict.merge
        Dict.insert
        (\k ea eb -> Dict.insert k (mergeEntry ea eb))
        Dict.insert
        da
        db
        Dict.empty


mergeEntry : Entry -> Entry -> Entry
mergeEntry a b =
    let
        -- the value always merges recursively, regardless of presence
        mergedValue =
            merge a.value b.value

        -- presence is an LWW boolean by stamp; on equal stamps, remove wins
        -- (present = AND), which keeps the tiebreak commutative & associative.
        ( present, stamp ) =
            case Id.compareOpId a.stamp b.stamp of
                GT ->
                    ( a.present, a.stamp )

                LT ->
                    ( b.present, b.stamp )

                EQ ->
                    ( a.present && b.present, a.stamp )
    in
    { value = mergedValue, present = present, stamp = stamp }


mergeRegister : Register -> Register -> Register
mergeRegister a b =
    case Id.compareOpId a.stamp b.stamp of
        GT ->
            a

        LT ->
            b

        EQ ->
            -- same stamp: pick the larger value so the result is independent of
            -- argument order. Equal values make this a no-op.
            if comparePrim a.value b.value == LT then
                b

            else
                a


{-| Two contributions for the same op id should be identical; if they disagree
(corrupt input), pick the larger delta so merge is commutative.
-}
mergeIncrement : Increment -> Increment -> Increment
mergeIncrement a b =
    if a.delta >= b.delta then
        a

    else
        b


rank : Node -> Int
rank node =
    case node of
        Reg _ ->
            0

        Map _ ->
            1

        Seq _ ->
            2

        Txt _ ->
            3

        Cnt _ ->
            4

        Mov _ ->
            5



-- PRIM ORDER -----------------------------------------------------------------


{-| A total order over primitives, used as the equal-stamp tiebreak in
`mergeRegister`.
-}
comparePrim : Prim -> Prim -> Order
comparePrim a b =
    case ( a, b ) of
        ( PNull, PNull ) ->
            EQ

        ( PBool x, PBool y ) ->
            compare (boolToInt x) (boolToInt y)

        ( PInt x, PInt y ) ->
            compare x y

        ( PFloat x, PFloat y ) ->
            compare x y

        ( PString x, PString y ) ->
            compare x y

        _ ->
            compare (primRank a) (primRank b)


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0


primRank : Prim -> Int
primRank p =
    case p of
        PNull ->
            0

        PBool _ ->
            1

        PInt _ ->
            2

        PFloat _ ->
            3

        PString _ ->
            4



-- CLOCK CATCH-UP -------------------------------------------------------------


{-| The largest Lamport counter referenced anywhere in the tree, used to advance
a replica's clock after a merge so it never re-mints a seen id.
-}
maxCounter : Node -> Int
maxCounter node =
    case node of
        Reg r ->
            Id.opIdCounter r.stamp

        Map d ->
            Dict.foldl
                (\_ e acc -> max acc (max (Id.opIdCounter e.stamp) (maxCounter e.value)))
                0
                d

        Seq rga ->
            rgaMaxCounter rga

        Txt rga ->
            rgaMaxCounter rga

        Cnt d ->
            Dict.foldl (\_ inc acc -> max acc (Id.opIdCounter inc.stamp)) 0 d

        Mov ml ->
            MoveList.maxCounter maxCounter ml


rgaMaxCounter : RgaNode -> Int
rgaMaxCounter rga =
    List.foldl
        (\el acc -> max acc (maxCounter el.content))
        (Rga.maxCounter rga)
        (Rga.elements rga)
