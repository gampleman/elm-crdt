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

wss.on("connection", (socket) => {
  console.log(`peer connected (${wss.clients.size} total)`);

  socket.on("message", (data, isBinary) => {
    // Broadcast verbatim to everyone except the sender.
    for (const client of wss.clients) {
      if (client !== socket && client.readyState === client.OPEN) {
        client.send(data, { binary: isBinary });
      }
    }
  });

  socket.on("close", () => {
    console.log(`peer disconnected (${wss.clients.size} total)`);
  });
});

server.listen(PORT, () => {
  console.log(`elm-crdt relay listening on ws://localhost:${PORT}`);
});
