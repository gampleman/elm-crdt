module Crdt.Presence exposing
    ( Presence, Codec
    , init, setLocal, updateLocal, local, peers, merge
    , encode, decode
    , codec, field, optional, buildCodec
    , string, bool, int
    , FieldCodec
    )

{-| Presence (a.k.a. awareness): ephemeral per-peer state — who is online, their
display name/color, which field they are editing — kept on a channel **separate**
from the CRDT document. It is never merged into the document.

Each peer owns one slot keyed by its `ReplicaId`. A slot carries a logical
sequence number; merging keeps the higher-sequence slot per peer (last-write-wins
per peer). This is a deliberately tiny, self-contained CRDT: a LWW-map keyed by
replica.

State is described by a small `Codec` (a cut-down record codec) so it can be
serialized to JSON for the wire.

@docs Presence, Codec
@docs init, setLocal, updateLocal, local, peers, merge
@docs encode, decode
@docs codec, field, optional, buildCodec
@docs string, bool, int
@docs FieldCodec

-}

import Crdt.Id as Id exposing (ReplicaId)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


{-| The awareness table for a peer: its own id + the codec, plus the latest known
slot for every peer (including itself).
-}
type Presence a
    = Presence
        { me : ReplicaId
        , codec : Codec a
        , slots : Dict String (Slot a)
        }


type alias Slot a =
    { seq : Int
    , value : a
    }



-- LIFECYCLE ------------------------------------------------------------------


{-| A fresh, empty presence table for a replica.
-}
init : ReplicaId -> Codec a -> Presence a
init me c =
    Presence { me = me, codec = c, slots = Dict.empty }


{-| Set the local peer's state, bumping its sequence number.
-}
setLocal : a -> Presence a -> Presence a
setLocal value (Presence p) =
    let
        key =
            Id.toString p.me

        seq =
            Dict.get key p.slots |> Maybe.map (\s -> s.seq + 1) |> Maybe.withDefault 0
    in
    Presence { p | slots = Dict.insert key { seq = seq, value = value } p.slots }


{-| Update the local peer's state, if any, bumping its sequence number.
-}
updateLocal : (a -> a) -> Presence a -> Presence a
updateLocal f pres =
    case local pres of
        Just value ->
            setLocal (f value) pres

        Nothing ->
            pres


{-| The local peer's current state, if set.
-}
local : Presence a -> Maybe a
local (Presence p) =
    Dict.get (Id.toString p.me) p.slots |> Maybe.map .value


{-| Every known peer and its state (including self), in replica-id order.
-}
peers : Presence a -> List ( ReplicaId, a )
peers (Presence p) =
    Dict.toList p.slots
        |> List.map (\( k, slot ) -> ( Id.replica k, slot.value ))


{-| Merge another peer's awareness table into this one: per replica, keep the
slot with the higher sequence number (LWW per peer).
-}
merge : Presence a -> Presence a -> Presence a
merge (Presence a) (Presence b) =
    Presence
        { a
            | slots =
                Dict.merge
                    Dict.insert
                    (\k sa sb ->
                        Dict.insert k
                            (if sa.seq >= sb.seq then
                                sa

                             else
                                sb
                            )
                    )
                    Dict.insert
                    a.slots
                    b.slots
                    Dict.empty
        }



-- WIRE -----------------------------------------------------------------------


{-| Serialize the awareness table to JSON for the wire.
-}
encode : Presence a -> JE.Value
encode (Presence p) =
    JE.dict identity
        (\slot ->
            JE.object
                [ ( "seq", JE.int slot.seq )
                , ( "value", encodeValue p.codec slot.value )
                ]
        )
        p.slots


{-| Decode an awareness table received from a peer. Callers merge the result
into their own table with `merge`.
-}
decode : Codec a -> JE.Value -> Result String (Presence a)
decode c value =
    JD.decodeValue (presenceDecoder c) value
        |> Result.mapError JD.errorToString


presenceDecoder : Codec a -> Decoder (Presence a)
presenceDecoder ((Codec cc) as c) =
    JD.dict
        (JD.map2 Slot
            (JD.field "seq" JD.int)
            (JD.field "value" cc.decoder)
        )
        |> JD.map
            (\slots ->
                -- a decoded table has no inherent "me"; callers merge it into
                -- their own table, so a placeholder me is fine.
                Presence { me = Id.replica "", codec = c, slots = slots }
            )



-- CODEC ----------------------------------------------------------------------


{-| Describes how a presence value serializes. A cut-down record codec.
-}
type Codec a
    = Codec
        { encoder : a -> List ( String, JE.Value )
        , decoder : Decoder a
        }


{-| A single field's codec.
-}
type FieldCodec a
    = FieldCodec
        { encode : a -> JE.Value
        , decode : Decoder a
        }


{-| In-progress presence codec.
-}
type CodecBuilder full a
    = CodecBuilder
        { encoder : full -> List ( String, JE.Value )
        , decoder : Decoder a
        }


encodeValue : Codec a -> a -> JE.Value
encodeValue (Codec c) value =
    JE.object (c.encoder value)


{-| Begin a presence codec from a constructor.
-}
codec : (a -> b) -> CodecBuilder full (a -> b)
codec ctor =
    CodecBuilder
        { encoder = \_ -> []
        , decoder = JD.succeed ctor
        }


{-| A required field.
-}
field : String -> (full -> a) -> FieldCodec a -> CodecBuilder full (a -> b) -> CodecBuilder full b
field name getter (FieldCodec fc) (CodecBuilder cb) =
    CodecBuilder
        { encoder = \full -> ( name, fc.encode (getter full) ) :: cb.encoder full
        , decoder = JD.map2 (\f a -> f a) cb.decoder (JD.field name fc.decode)
        }


{-| An optional field (encoded as null when absent).
-}
optional : String -> (full -> Maybe a) -> FieldCodec a -> CodecBuilder full (Maybe a -> b) -> CodecBuilder full b
optional name getter (FieldCodec fc) (CodecBuilder cb) =
    CodecBuilder
        { encoder =
            \full ->
                ( name
                , case getter full of
                    Just a ->
                        fc.encode a

                    Nothing ->
                        JE.null
                )
                    :: cb.encoder full
        , decoder = JD.map2 (\f a -> f a) cb.decoder (JD.field name (JD.nullable fc.decode))
        }


{-| Finish a presence codec.
-}
buildCodec : CodecBuilder a a -> Codec a
buildCodec (CodecBuilder cb) =
    Codec { encoder = cb.encoder, decoder = cb.decoder }



-- FIELD CODECS ---------------------------------------------------------------


{-| A string field.
-}
string : FieldCodec String
string =
    FieldCodec { encode = JE.string, decode = JD.string }


{-| A bool field.
-}
bool : FieldCodec Bool
bool =
    FieldCodec { encode = JE.bool, decode = JD.bool }


{-| An int field.
-}
int : FieldCodec Int
int =
    FieldCodec { encode = JE.int, decode = JD.int }
