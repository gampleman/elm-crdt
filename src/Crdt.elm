module Crdt exposing
    ( Doc, Dict, Error
    , init, read, merge
    , encode, decode
    , errorToString, dictToList
    , opIds, maxCounter
    )

{-| Pure-Elm CRDTs: compose records, lists, dicts, text and last-write-wins
registers into JSON-like documents that converge under concurrent editing.

A document is described once by a `Crdt.Schema.Crdt a` schema, edited through
`Crdt.Edit`, synced by exchanging JSON (`encode`/`decode`) and `merge`-ing, and
read back into your typed `a` with `read`. The library is transport-agnostic:
it only ever produces and consumes `Json.Value` — see the demo for WebSocket
wiring.

    import Crdt
    import Crdt.Schema as S
    import Crdt.Edit as E
    import Crdt.Path as Path

    schema = S.record Todo |> S.field "text" .text S.text |> S.build

    doc = Crdt.init (Crdt.Id.replica "alice") schema

    -- after edits + a merge with a peer:
    Crdt.read schema (Crdt.merge doc peerDoc)

@docs Doc, Dict, Error
@docs init, read, merge
@docs encode, decode
@docs errorToString, dictToList
@docs opIds, maxCounter

-}

import Crdt.Id as Id exposing (OpId, ReplicaId)
import Crdt.Internal as I
import Crdt.Json as Json
import Crdt.Node as Node exposing (Node)
import Crdt.Rga as Rga
import Crdt.Schema as Schema exposing (Crdt)
import Dict
import Json.Decode as JD
import Json.Encode as JE


{-| A replica's document. Holds the replicated state, the local clock, and
local (non-replicated) history.
-}
type alias Doc =
    I.Doc


{-| Re-export of `Dict` so demos can name the dict type without importing core
`Dict` separately.
-}
type alias Dict k v =
    Dict.Dict k v


{-| A schema read error.
-}
type alias Error =
    Schema.Error


{-| Render a read error.
-}
errorToString : Error -> String
errorToString =
    Schema.errorToString


{-| Stable ordered listing of a dict's entries (re-exported for rendering).
-}
dictToList : Dict k v -> List ( k, v )
dictToList =
    Dict.toList



-- LIFECYCLE ------------------------------------------------------------------


{-| Create an empty document for a replica, with the structure described by a
schema.
-}
init : ReplicaId -> Crdt a -> Doc
init replica schema =
    let
        ( root, ctx ) =
            Schema.emptyNode schema (Id.ctx replica)
    in
    I.make root ctx


{-| Read the typed value out of a document through its schema.
-}
read : Crdt a -> Doc -> Result Error a
read schema doc =
    Schema.decodeNode schema (I.root doc)


{-| Merge another replica's document into this one. Commutative, associative and
idempotent on the replicated state. Advances the local clock past anything seen
in the incoming state so future edits never re-mint a seen id. History is local,
so it is preserved (not merged).
-}
merge : Doc -> Doc -> Doc
merge local incoming =
    let
        mergedRoot =
            Node.merge (I.root local) (I.root incoming)

        seen =
            max (Node.maxCounter (I.root incoming)) (Node.maxCounter mergedRoot)

        newCtx =
            Id.observe seen (I.ctx local)
    in
    I.withRootNoHistory mergedRoot newCtx local



-- WIRE -----------------------------------------------------------------------


{-| Serialize the full replicated state to JSON for transport. (History is local
and is not encoded.)
-}
encode : Doc -> JE.Value
encode doc =
    Json.encodeNode (I.root doc)


{-| Decode a document from JSON received from a peer. The given `ReplicaId` is
this replica's id; the decoded document's clock is advanced past everything in
the payload.
-}
decode : ReplicaId -> JE.Value -> Result String Doc
decode replica value =
    JD.decodeValue Json.nodeDecoder value
        |> Result.mapError JD.errorToString
        |> Result.map
            (\root ->
                let
                    ctx =
                        Id.observe (Node.maxCounter root) (Id.ctx replica)
                in
                I.make root ctx
            )



-- INTROSPECTION (used by tests / debugging) ----------------------------------


{-| The largest Lamport counter referenced anywhere in the document.
-}
maxCounter : Doc -> Int
maxCounter doc =
    Node.maxCounter (I.root doc)


{-| Every freshly-_minted_ `OpId` in the document: register stamps, map-entry
presence stamps, and sequence element ids. Element _origins_ are deliberately
excluded — they are references to other elements' ids, so they repeat by design.
Used by tests to assert that minted ids are globally unique.
-}
opIds : Doc -> List OpId
opIds doc =
    collectOpIds (I.root doc)


collectOpIds : Node -> List OpId
collectOpIds node =
    case node of
        Node.Reg r ->
            [ r.stamp ]

        Node.Map entries ->
            Dict.foldl (\_ e acc -> e.stamp :: collectOpIds e.value ++ acc) [] entries

        Node.Seq rga ->
            collectRgaOpIds rga

        Node.Txt rga ->
            collectRgaOpIds rga

        Node.Cnt contributions ->
            Dict.foldl (\_ inc acc -> inc.stamp :: acc) [] contributions


collectRgaOpIds : Node.RgaNode -> List OpId
collectRgaOpIds rga =
    Rga.elements rga
        |> List.concatMap (\el -> el.id :: collectOpIds el.content)
