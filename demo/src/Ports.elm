port module Ports exposing (connection, incoming, outgoing)

{-| The library is pure Elm and knows nothing about the network. All transport
lives here in the demo: we serialize documents/presence to `Json.Value` and
ship them over a WebSocket (see `index.js` + `server/relay.js`).
-}

import Json.Decode as JD
import Json.Encode as JE


{-| Send an envelope (`{kind, payload}`) out to the relay, which broadcasts it
to every other connected peer.
-}
port outgoing : JE.Value -> Cmd msg


{-| Receive an envelope forwarded by the relay from another peer.
-}
port incoming : (JD.Value -> msg) -> Sub msg


{-| WebSocket open/close events, so the UI can show online/offline and flush
queued state on reconnect.
-}
port connection : (Bool -> msg) -> Sub msg
