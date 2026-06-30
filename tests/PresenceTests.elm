module PresenceTests exposing (suite)

{-| Presence (awareness) is ephemeral per-peer state, kept on a separate channel
from the document. It is last-write-wins per peer and never merged into the doc.
-}

import Crdt.Id as Id
import Crdt.Presence as Presence
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)


type alias Cursor =
    { name : String
    , editing : Maybe String
    }


codec : Presence.Codec Cursor
codec =
    Presence.codec Cursor
        |> Presence.field "name" .name Presence.string
        |> Presence.optional "editing" .editing Presence.string
        |> Presence.buildCodec


{-| Mirrors the demo's `Peer`: a required field plus an OPTIONAL field with a
custom JSON codec (the caret carries a `Crdt.Cursor` this way).
-}
type alias Peer =
    { name : String
    , mark : Maybe (List Int)
    }


peerCodec : Presence.Codec Peer
peerCodec =
    Presence.codec Peer
        |> Presence.field "name" .name Presence.string
        |> Presence.optional "mark" .mark (Presence.custom (JE.list JE.int) (JD.list JD.int))
        |> Presence.buildCodec


alice : Id.ReplicaId
alice =
    Id.replica "alice"


bob : Id.ReplicaId
bob =
    Id.replica "bob"


suite : Test
suite =
    describe "Presence"
        [ test "local state is readable after setLocal" <|
            \_ ->
                Presence.init alice codec
                    |> Presence.setLocal (Cursor "Alice" Nothing)
                    |> Presence.local
                    |> Expect.equal (Just (Cursor "Alice" Nothing))
        , test "merging two peers lists both" <|
            \_ ->
                let
                    a =
                        Presence.init alice codec |> Presence.setLocal (Cursor "Alice" (Just "title"))

                    b =
                        Presence.init bob codec |> Presence.setLocal (Cursor "Bob" Nothing)

                    merged =
                        Presence.merge a b
                in
                Presence.peers merged
                    |> List.length
                    |> Expect.equal 2
        , test "presence is LWW per peer: newer state replaces older" <|
            \_ ->
                let
                    a1 =
                        Presence.init alice codec |> Presence.setLocal (Cursor "Alice" Nothing)

                    a2 =
                        a1 |> Presence.updateLocal (\c -> { c | editing = Just "notes" })
                in
                Presence.local (Presence.merge a1 a2)
                    |> Expect.equal (Just (Cursor "Alice" (Just "notes")))
        , test "presence encode/decode roundtrips" <|
            \_ ->
                let
                    a =
                        Presence.init alice codec |> Presence.setLocal (Cursor "Alice" (Just "title"))
                in
                case Presence.decode codec (Presence.encode a) of
                    Ok decoded ->
                        Expect.equal (Presence.peers a) (Presence.peers decoded)

                    Err e ->
                        Expect.fail ("presence decode failed: " ++ e)
        , test "optional custom field round-trips when present (the caret path)" <|
            \_ ->
                let
                    a =
                        Presence.init alice peerCodec
                            |> Presence.setLocal (Peer "Alice" (Just [ 3, 1, 4 ]))
                in
                case Presence.decode peerCodec (Presence.encode a) of
                    Ok decoded ->
                        Presence.peers decoded
                            |> List.map Tuple.second
                            |> Expect.equal [ Peer "Alice" (Just [ 3, 1, 4 ]) ]

                    Err e ->
                        Expect.fail ("decode failed: " ++ e)
        , test "optional custom field round-trips when absent (Nothing)" <|
            \_ ->
                let
                    a =
                        Presence.init alice peerCodec
                            |> Presence.setLocal (Peer "Alice" Nothing)
                in
                case Presence.decode peerCodec (Presence.encode a) of
                    Ok decoded ->
                        Presence.peers decoded
                            |> List.map Tuple.second
                            |> Expect.equal [ Peer "Alice" Nothing ]

                    Err e ->
                        Expect.fail ("decode failed: " ++ e)
        , test "remove drops a peer (e.g. on disconnect)" <|
            \_ ->
                let
                    a =
                        Presence.init alice codec |> Presence.setLocal (Cursor "Alice" Nothing)

                    b =
                        Presence.init bob codec |> Presence.setLocal (Cursor "Bob" Nothing)

                    merged =
                        Presence.merge a b

                    afterLeave =
                        Presence.remove bob merged
                in
                Expect.all
                    [ \_ -> Presence.peers merged |> List.length |> Expect.equal 2
                    , \_ -> Presence.peers afterLeave |> List.map Tuple.first |> Expect.equal [ alice ]
                    ]
                    ()
        , test "a peer WITH a custom field merges alongside a peer WITHOUT one" <|
            \_ ->
                let
                    a =
                        Presence.init alice peerCodec |> Presence.setLocal (Peer "Alice" (Just [ 1 ]))

                    b =
                        Presence.init bob peerCodec |> Presence.setLocal (Peer "Bob" Nothing)

                    -- simulate the wire: each decodes the other's broadcast
                    merged =
                        case Presence.decode peerCodec (Presence.encode b) of
                            Ok bDecoded ->
                                Presence.merge a bDecoded

                            Err _ ->
                                a
                in
                Presence.peers merged |> List.length |> Expect.equal 2
        ]
