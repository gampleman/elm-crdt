// Boots the Elm app and wires its ports to a WebSocket connection to the relay.
//
// Each browser tab is a real, independent replica. A random replicaId/name/color
// is generated per tab so two tabs collaborate as two distinct peers.

const RELAY_URL =
  (location.protocol === "https:" ? "wss://" : "ws://") +
  (location.hostname || "localhost") +
  ":8080";

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

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: tabIdentity(),
});

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
    try {
      app.ports.incoming.send(JSON.parse(event.data));
    } catch (e) {
      console.warn("bad message", e);
    }
  });
}

// Elm -> network. Messages sent while offline are simply dropped on the floor;
// on reconnect the demo re-broadcasts full state (ConnectionChanged True), so
// peers always converge.
app.ports.outgoing.subscribe((envelope) => {
  if (socket && socket.readyState === WebSocket.OPEN) {
    socket.send(JSON.stringify(envelope));
  }
});

connect();
