module Crdt.Id exposing
    ( ReplicaId, replica, toString
    , OpId, opIdCounter, opIdReplica, compareOpId, opIdToString
    )

{-| The two identity types the library hands back to you.

Every replica — every independent copy of a document, such as one browser tab — has a
stable `ReplicaId`, a string you choose (typically a random per-tab token). You pass one
to `Crdt.init` so the library knows which replica it is editing on behalf of.

Every edit the library records is stamped with an `OpId`, a globally-unique operation
id. You never mint these yourself, but they surface as **stable handles** to things
inside a document: the id of an item in a `Crdt.Tree`, or the identity a `Crdt.Cursor`
is pinned to. An `OpId` can be compared and turned into a string, which is all you need
to use one as a dictionary key or to look a value up.


# Replicas

@docs ReplicaId, replica, toString


# Operation ids

@docs OpId, opIdCounter, opIdReplica, compareOpId, opIdToString

-}

import Crdt.Id.Internal as I


{-| A stable per-replica identifier. Two browser tabs, two `ReplicaId`s. Build one with
`replica`.
-}
type alias ReplicaId =
    I.ReplicaId


{-| Build a `ReplicaId` from a string. Use something unique per replica — a random
per-tab token, a device id, a signed-in user id.

    Crdt.init (Crdt.Id.replica "alice-tab-1") schema

-}
replica : String -> ReplicaId
replica =
    I.replica


{-| The underlying string of a `ReplicaId`, e.g. to show who made a change.
-}
toString : ReplicaId -> String
toString =
    I.toString


{-| A globally-unique id for a single recorded operation. You get these back as handles
to things inside a document (tree item ids, cursor anchors); the library mints them, you
read and compare them.
-}
type alias OpId =
    I.OpId


{-| The counter part of an `OpId`.
-}
opIdCounter : OpId -> Int
opIdCounter =
    I.opIdCounter


{-| The replica that minted an `OpId`.
-}
opIdReplica : OpId -> ReplicaId
opIdReplica =
    I.opIdReplica


{-| A total order on `OpId`s — by counter, then replica as a tiebreak. Handy if you keep
your own sorted structure keyed by id.
-}
compareOpId : OpId -> OpId -> Order
compareOpId =
    I.compareOpId


{-| A stable string form of an `OpId`, suitable as a `Dict` key (`"3@alice"`).
-}
opIdToString : OpId -> String
opIdToString =
    I.opIdToString
