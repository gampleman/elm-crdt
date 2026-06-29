module PresenceTests exposing (suite)

{-| Presence (awareness) is ephemeral per-peer state, kept on a separate channel
from the document. It is last-write-wins per peer and never merged into the doc.
-}

import Crdt.Id as Id
import Crdt.Presence as Presence
import Expect
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
        ]
