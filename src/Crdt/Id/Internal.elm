module Crdt.Id.Internal exposing
    ( ReplicaId, replica, toString
    , OpId, opId, opIdCounter, opIdReplica, compareOpId, opIdToString
    , Ctx, ctx, nextId, observe, ctxReplica
    )

{-| Identity and causality primitives.

Every replica has a stable `ReplicaId` (a user-supplied string). Edits mint
`OpId`s — a Lamport counter paired with the originating `ReplicaId`. The pair is
a total order, which we use both to give sequence elements stable identity and
to break ties in last-write-wins registers.


# Replicas

@docs ReplicaId, replica, toString


# Operation ids

@docs OpId, opId, opIdCounter, opIdReplica, compareOpId, opIdToString


# Clocks

@docs Ctx, ctx, nextId, observe, ctxReplica

-}


{-| A stable per-replica identifier. Two browser tabs, two `ReplicaId`s.
-}
type ReplicaId
    = ReplicaId String


{-| Build a `ReplicaId` from a string (e.g. a random per-tab token).
-}
replica : String -> ReplicaId
replica =
    ReplicaId


{-| The underlying string, for display.
-}
toString : ReplicaId -> String
toString (ReplicaId s) =
    s


compareReplica : ReplicaId -> ReplicaId -> Order
compareReplica (ReplicaId a) (ReplicaId b) =
    compare a b


{-| A globally-unique operation id: a Lamport counter plus its replica. Ordered
lexicographically by `(counter, replica)`.
-}
type OpId
    = OpId Int ReplicaId


{-| Construct an `OpId` from a counter and replica.
-}
opId : Int -> ReplicaId -> OpId
opId =
    OpId


{-| The Lamport counter component.
-}
opIdCounter : OpId -> Int
opIdCounter (OpId c _) =
    c


{-| The replica component.
-}
opIdReplica : OpId -> ReplicaId
opIdReplica (OpId _ r) =
    r


{-| Total order on `OpId`s: by counter, then by replica id as a tiebreak. This
is the LWW winner rule and the RGA sibling-ordering rule.
-}
compareOpId : OpId -> OpId -> Order
compareOpId (OpId c1 r1) (OpId c2 r2) =
    case compare c1 c2 of
        EQ ->
            compareReplica r1 r2

        other ->
            other


{-| A stable string form, used as a `Dict` key for collections (so structural
equality is a sound convergence oracle).
-}
opIdToString : OpId -> String
opIdToString (OpId c (ReplicaId r)) =
    String.fromInt c ++ "@" ++ r


{-| A Lamport clock value.
-}
type alias Clock =
    Int


{-| The mutable context threaded through constructors and edits: which replica
we are, and the next counter value to hand out.
-}
type Ctx
    = Ctx ReplicaId Clock


{-| Build a fresh context for a replica (clock starts at 0).
-}
ctx : ReplicaId -> Ctx
ctx r =
    Ctx r 0


{-| The replica owning this context.
-}
ctxReplica : Ctx -> ReplicaId
ctxReplica (Ctx r _) =
    r


{-| Mint a fresh `OpId` and advance the clock. The new id uses the _incremented_
counter so ids are strictly increasing per replica.
-}
nextId : Ctx -> ( OpId, Ctx )
nextId (Ctx r c) =
    let
        next =
            c + 1
    in
    ( OpId next r, Ctx r next )


{-| Advance the clock past a counter value seen during a merge, so subsequent
local edits never collide with ids minted elsewhere. Lamport's rule.
-}
observe : Int -> Ctx -> Ctx
observe seen (Ctx r c) =
    Ctx r (max c seen)
