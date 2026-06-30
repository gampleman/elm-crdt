// Tiny WebSocket relay for the elm-crdt demo.
//
// It does NOT understand CRDTs at all — it is a dumb broadcast hub. Each client
// sends opaque JSON envelopes ({kind, payload}); the relay forwards every
// message to all *other* connected clients. Convergence is guaranteed by the
// CRDT merge on each client, not by the server, so message order and delivery
// duplication don't matter.

const http = require("http");
const { WebSocketServer } = require("ws");

const PORT = process.env.PORT || 8080;

const server = http.createServer((req, res) => {
  res.writeHead(200, { "content-type": "text/plain" });
  res.end("elm-crdt relay is running. Connect a WebSocket to this port.\n");
});

const wss = new WebSocketServer({ server });

function broadcastExcept(sender, payload) {
  for (const client of wss.clients) {
    if (client !== sender && client.readyState === client.OPEN) {
      client.send(payload);
    }
  }
}

wss.on("connection", (socket) => {
  console.log(`peer connected (${wss.clients.size} total)`);

  socket.on("message", (data, isBinary) => {
    // Remember which replica this socket belongs to, so we can announce its
    // departure on close. The client stamps every envelope with `from`.
    if (!isBinary) {
      try {
        const env = JSON.parse(data);
        if (env && typeof env.from === "string") socket.replicaId = env.from;
      } catch (_) {
        /* opaque to us; just forward */
      }
    }
    broadcastExcept(socket, data);
  });

  socket.on("close", () => {
    console.log(`peer disconnected (${wss.clients.size} total)`);
    // Tell everyone this peer is gone so they can drop it from presence. (A
    // closed tab can't send this itself, but the relay sees the socket close.)
    if (socket.replicaId) {
      broadcastExcept(
        socket,
        JSON.stringify({ kind: "left", from: socket.replicaId, payload: { replica: socket.replicaId } })
      );
    }
  });
});

server.listen(PORT, () => {
  console.log(`elm-crdt relay listening on ws://localhost:${PORT}`);
});
