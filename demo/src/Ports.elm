port module Ports exposing
    ( connection
    , incoming
    , outgoing
    , renderRichText
    , richTextInput
    )

{-| The library is pure Elm and knows nothing about the network. All transport
lives here in the demo: we serialize documents/presence to `Json.Value` and
ship them over a WebSocket (see `index.js` + `server/relay.js`).

The rich-text ports bridge Elm to a ProseMirror editor (TipTap) hosted in a
`<crdt-richtext>` custom element: `renderRichText` pushes the current spans down
for the editor to display, and `richTextInput` carries user edit intents back up.

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


{-| Push the current rich-text document (as spans) to the ProseMirror editor so it
can reconcile its view. The editor flags the resulting transaction so it is not
echoed back through `richTextInput` (loop avoidance).
-}
port renderRichText : JE.Value -> Cmd msg


{-| An edit intent from the ProseMirror editor: a `{ text, marks }` snapshot of the
editor's current single paragraph. Elm diffs it against the CRDT and emits the
minimal ops, so concurrent edits still merge.
-}
port richTextInput : (JD.Value -> msg) -> Sub msg
