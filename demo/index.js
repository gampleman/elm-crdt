// Boots the Elm app and wires its ports to a WebSocket connection to the relay.
//
// Each browser tab is a real, independent replica. A random replicaId/name/color
// is generated per tab so two tabs collaborate as two distinct peers.

// registers the <crdt-richtext> custom element (TipTap/ProseMirror editor)
import "./editor/crdt-richtext.js";

// Relay port defaults to 8080 but can be overridden with `?relayPort=NNNN` — used by
// the e2e tests to run an isolated relay that stray tabs on the default port can't
// reach (so a leftover tab's document never leaks into a test replica).
const RELAY_PORT = new URLSearchParams(location.search).get("relayPort") || "8080";
const RELAY_URL =
  (location.protocol === "https:" ? "wss://" : "ws://") +
  (location.hostname || "localhost") +
  ":" +
  RELAY_PORT;

const COLORS = ["#e84393", "#0984e3", "#00b894", "#fdcb6e", "#6c5ce7", "#e17055"];
const ANIMALS = ["otter", "lynx", "heron", "marten", "shrike", "vole"];

function pick(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}

// Persist this tab's identity across reloads so it stays the same replica.
function tabIdentity() {
  let id = sessionStorage.getItem("crdt-replica");
  if (!id) {
    id = Math.random().toString(36).slice(2, 8);
    sessionStorage.setItem("crdt-replica", id);
  }
  return {
    replicaId: id,
    name: pick(ANIMALS) + "-" + id.slice(0, 3),
    color: pick(COLORS),
  };
}

const identity = tabIdentity();

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: identity,
});

// --- Rich-text editor bridge ------------------------------------------------
//
// Elm owns the CRDT; the <crdt-richtext> element is a ProseMirror view. We connect
// the two ports to it:
//   • renderRichText (Elm -> JS): set the element's `docSpans` so it reconciles.
//   • richTextInput  (JS -> Elm): forward the element's `richtext-input` events.
//
// A render only needs applying if the editor is currently mounted (i.e. the editor
// tab is showing). If it isn't, the Elm view will hand it the current spans as a
// property the moment it mounts, so there's nothing to cache here.
app.ports.renderRichText.subscribe((payload) => {
  const el = document.querySelector("crdt-richtext");
  if (el) el.docBlocks = payload.blocks;
});

// Edits bubble from the element as CustomEvents; forward their detail to Elm.
document.addEventListener("richtext-input", (event) => {
  app.ports.richTextInput.send(event.detail);
});

// Note: no mount observer is needed — the Elm view sets `docBlocks` as a property on
// the <crdt-richtext> element on every render (including the first, when the editor
// tab mounts), so a freshly-mounted editor is always handed the current blocks.

let socket = null;
let reconnectTimer = null;

function connect() {
  socket = new WebSocket(RELAY_URL);

  socket.addEventListener("open", () => {
    app.ports.connection.send(true);
  });

  socket.addEventListener("close", () => {
    app.ports.connection.send(false);
    // simple auto-reconnect; the CRDT merge makes catch-up safe
    clearTimeout(reconnectTimer);
    reconnectTimer = setTimeout(connect, 1000);
  });

  socket.addEventListener("message", (event) => {
    // The relay sends text frames, but be robust to a Blob (binary frame) too,
    // which some setups produce — resolve it to text before parsing.
    const handle = (text) => {
      try {
        app.ports.incoming.send(JSON.parse(text));
      } catch (e) {
        console.warn("bad message", e);
      }
    };
    if (event.data instanceof Blob) {
      event.data.text().then(handle);
    } else {
      handle(event.data);
    }
  });
}

// Elm -> network. Messages sent while offline are simply dropped on the floor;
// on reconnect the demo re-broadcasts full state (ConnectionChanged True), so
// peers always converge.
app.ports.outgoing.subscribe((envelope) => {
  if (socket && socket.readyState === WebSocket.OPEN) {
    // stamp every message with our replica id so the relay can announce our
    // departure (a `left` message) when this socket closes
    socket.send(JSON.stringify({ ...envelope, from: identity.replicaId }));
  }
});

connect();
