// Boots the Elm app and wires its ports to a WebSocket connection to the relay.
//
// Each browser tab is a real, independent replica. A random replicaId/name/color
// is generated per tab so two tabs collaborate as two distinct peers.

// registers the <crdt-richtext> custom element (TipTap/ProseMirror editor)
import "./editor/crdt-richtext.js";

// Where the WebSocket relay lives. Resolution order:
//   1. `?relayPort=NNNN` in the URL — always wins (the e2e tests spin up an isolated
//      relay on a random port so a stray tab on the default port can't leak into a test).
//   2. A relay URL baked in at build time via esbuild
//      `--define:RELAY_URL_BUILD='"wss://…"'` — this is how the GitHub Pages build points
//      at the hosted (Render) relay, since Pages is static and can't run the relay itself.
//   3. Same host, port 8080 (local dev: `npm start` runs the relay there).
// `RELAY_URL_BUILD` defaults to "" so the reference resolves when esbuild doesn't define it.
const buildRelayUrl = typeof RELAY_URL_BUILD === "string" ? RELAY_URL_BUILD : "";

const relayPortOverride = new URLSearchParams(location.search).get("relayPort");
const sameHostRelay = (port) =>
  (location.protocol === "https:" ? "wss://" : "ws://") +
  (location.hostname || "localhost") +
  ":" +
  port;
const relayUrl = relayPortOverride
  ? sameHostRelay(relayPortOverride)
  : buildRelayUrl || sameHostRelay("8080");

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
  // Auto-compaction bound: the op log is compacted to the stable frontier once it exceeds
  // this. `?historyCap=` overrides the 1000 default (tests use a tiny cap to trigger it).
  const capParam = new URLSearchParams(location.search).get("historyCap");
  const historyCap = capParam ? Math.max(1, parseInt(capParam, 10)) : 1000;
  return {
    replicaId: id,
    name: pick(ANIMALS) + "-" + id.slice(0, 3),
    color: pick(COLORS),
    historyCap,
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

// The editor's caret (a document-wide char offset) bubbles up on every selection/doc
// change; forward it so Elm can mint a stable cursor and broadcast it as presence.
document.addEventListener("richtext-caret", (event) => {
  app.ports.richTextCaret.send(event.detail);
});

// Note: no mount observer is needed — the Elm view sets `docBlocks` as a property on
// the <crdt-richtext> element on every render (including the first, when the editor
// tab mounts), so a freshly-mounted editor is always handed the current blocks.

let socket = null;
let reconnectTimer = null;

function connect() {
  socket = new WebSocket(relayUrl);

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
