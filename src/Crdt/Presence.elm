module Crdt.Presence exposing
    ( Presence, Codec
    , init, setLocal, updateLocal, local, peers, merge, remove
    , encode, decode
    , codec, field, optional, buildCodec
    , string, bool, int, cursor, range, custom
    , FieldCodec
    )

{-| **Presence** is the "who's here and what are they doing" layer of a collaborative
app — each person's name and colour, whether they are online, where their cursor is —
as opposed to the document they are editing together.

It is kept deliberately separate from your `Crdt.Doc`, because it behaves differently:
presence is **throwaway**. You want the newest value and nothing else — when someone
moves their cursor you only care where it is _now_, and when they close the tab their
marker should just disappear. There is no history to preserve and nothing to reconcile
character by character, so presence has its own tiny, self-contained mechanism rather
than going through the document's CRDT machinery.

Each peer owns one slot in a shared table, keyed by its `Crdt.Id.ReplicaId`. When two
tables merge, the newer value wins for each peer — simple last-writer-wins, per peer.


## Describing your presence value

You first describe the shape of one peer's presence with a small `Codec`, so it can be
sent as JSON. It reads like a compact record builder:

    type alias Peer =
        { name : String
        , color : String
        , caret : Maybe Cursor
        }

    peerCodec : Presence.Codec Peer
    peerCodec =
        Presence.codec Peer
            |> Presence.field "name" .name Presence.string
            |> Presence.field "color" .color Presence.string
            |> Presence.optional "caret" .caret Presence.cursor
            |> Presence.buildCodec


## Using it

Keep one `Presence Peer` in your model alongside your document. Set your own state as it
changes, broadcast the table to peers exactly as you broadcast document updates, merge
what comes back, and render everyone with `peers`:

    model.presence
        |> Presence.setLocal { name = "Alice", color = "#e11", caret = Just cursor }
        |> Presence.encode

    -- send this JSON to peers
    -- on receiving a peer's table:
    case Presence.decode peerCodec json of
        Ok theirs ->
            { model | presence = Presence.merge model.presence theirs }

        Err _ ->
            model


# The presence table

@docs Presence, Codec


# Setting and reading state

@docs init, setLocal, updateLocal, local, peers, merge, remove


# Sending it over the wire

@docs encode, decode


# Describing a presence value

@docs codec, field, optional, buildCodec


# Field types

@docs string, bool, int, cursor, range, custom
@docs FieldCodec

-}

import Crdt.Cursor as Cursor exposing (Cursor, Range)
import Crdt.Id as Id exposing (ReplicaId)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


{-| A table of everyone's presence state: this replica's own id and codec, plus the
latest known value for every peer (including itself). Opaque — build one with `init`.
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


{-| A fresh, empty presence table for a replica. Pass the same `ReplicaId` you gave
`Crdt.init` and the `Codec` describing your presence value.
-}
init : ReplicaId -> Codec a -> Presence a
init me c =
    Presence { me = me, codec = c, slots = Dict.empty }


{-| Set this replica's own presence state (replacing any previous value). Call it
whenever your local state changes — the user renamed themselves, moved their cursor —
and then broadcast the table.
-}
setLocal : a -> Presence a -> Presence a
setLocal value (Presence p) =
    let
        key =
            Id.replicaToString p.me

        seq =
            Dict.get key p.slots |> Maybe.map (\s -> s.seq + 1) |> Maybe.withDefault 0
    in
    Presence { p | slots = Dict.insert key { seq = seq, value = value } p.slots }


{-| Update this replica's own state with a function, if it has been set. Handy for
changing one part — moving the cursor while keeping name and colour:

    Presence.updateLocal (\me -> { me | caret = Just cursor }) model.presence

-}
updateLocal : (a -> a) -> Presence a -> Presence a
updateLocal f pres =
    case local pres of
        Just value ->
            setLocal (f value) pres

        Nothing ->
            pres


{-| This replica's own current state, if it has been set.
-}
local : Presence a -> Maybe a
local (Presence p) =
    Dict.get (Id.replicaToString p.me) p.slots |> Maybe.map .value


{-| Every known peer and its state, including yourself, ordered by replica id. This is
what you fold over to render collaborators (filter out your own id if you don't want to
draw yourself).
-}
peers : Presence a -> List ( ReplicaId, a )
peers (Presence p) =
    Dict.toList p.slots
        |> List.map (\( k, slot ) -> ( Id.replica k, slot.value ))


{-| Merge a table received from a peer into this one, keeping the newer value for each
replica. Like document `merge`, the result is the same no matter what order tables
arrive in.
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


{-| Drop a peer from the table, e.g. when they disconnect. Because presence is
throwaway there is nothing left behind — the peer simply vanishes, and reappears if it
broadcasts again later.
-}
remove : ReplicaId -> Presence a -> Presence a
remove rid (Presence p) =
    Presence { p | slots = Dict.remove (Id.replicaToString rid) p.slots }



-- WIRE -----------------------------------------------------------------------


{-| Serialize the whole presence table to JSON, to broadcast to peers.
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


{-| Decode a presence table received from a peer. Merge the result into your own with
`merge`.
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


{-| Describes how one peer's presence value is turned into JSON and back. Build one with
`codec`/`field`/`buildCodec`. Opaque.
-}
type Codec a
    = Codec
        { encoder : a -> List ( String, JE.Value )
        , decoder : Decoder a
        }


{-| Describes one field of a presence value (see [`string`](#string) / [`bool`](#bool) /
[`int`](#int) / [`custom`](#custom)).
-}
type FieldCodec a
    = FieldCodec
        { encode : a -> JE.Value
        , decode : Decoder a
        }


{-| A presence codec under construction, between `codec` and `buildCodec`. You won't
name this type directly.
-}
type CodecBuilder full a
    = CodecBuilder
        { encoder : full -> List ( String, JE.Value )
        , decoder : Decoder a
        }


encodeValue : Codec a -> a -> JE.Value
encodeValue (Codec c) value =
    JE.object (c.encoder value)


{-| Start describing a presence value, given the constructor of your record (just like
`Crdt.record`). Follow it with a `field`/`optional` per record field and finish with
`buildCodec`.
-}
codec : (a -> b) -> CodecBuilder full (a -> b)
codec ctor =
    CodecBuilder
        { encoder = \_ -> []
        , decoder = JD.succeed ctor
        }


{-| Add a required field: its JSON name, a getter, and its field type.
-}
field : String -> (full -> a) -> FieldCodec a -> CodecBuilder full (a -> b) -> CodecBuilder full b
field name getter (FieldCodec fc) (CodecBuilder cb) =
    CodecBuilder
        { encoder = \full -> ( name, fc.encode (getter full) ) :: cb.encoder full
        , decoder = JD.map2 (\f a -> f a) cb.decoder (JD.field name fc.decode)
        }


{-| Add an optional field, read as a `Maybe` and sent as `null` when absent — good for
things not always present, like a cursor only shown while editing.
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


{-| Finish building a presence codec.
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


{-| A boolean field.
-}
bool : FieldCodec Bool
bool =
    FieldCodec { encode = JE.bool, decode = JD.bool }


{-| An integer field.
-}
int : FieldCodec Int
int =
    FieldCodec { encode = JE.int, decode = JD.int }


{-| A `Crdt.Cursor` field — a peer's caret position. The common case for sharing where
everyone is typing, so it is built in:

    Presence.codec Peer
        |> Presence.optional "caret" .caret Presence.cursor
        |> Presence.buildCodec

-}
cursor : FieldCodec Cursor
cursor =
    FieldCodec { encode = Cursor.encode, decode = Cursor.decoder }


{-| A `Crdt.Cursor.Range` field — a peer's text **selection** (its two ends). Like
`cursor`, but for a highlighted range rather than a single caret.
-}
range : FieldCodec Range
range =
    FieldCodec { encode = Cursor.encodeRange, decode = Cursor.rangeDecoder }


{-| A field with your own JSON encoder and decoder — for anything the built-in field
types don't cover:

    Presence.custom encodeColor colorDecoder

-}
custom : (a -> JE.Value) -> Decoder a -> FieldCodec a
custom enc dec =
    FieldCodec { encode = enc, decode = dec }
