// Wire-size benchmark: is a custom binary format worth it, or does gzip(JSON)
// already capture most of the win? (See ROADMAP "compact binary".)
//
// For each workload + size, the Elm worker hands back three JSON payloads — a full
// document (`Doc.encode`), the delta of one more typical edit (`Doc.encodeSince`), and a
// compacted SNAPSHOT (`Doc.encodeFrom`) — and we measure raw vs gzipped bytes for each.
// Deltas are the interesting case for a binary format: a tiny payload gives gzip a cold
// dictionary, so that's where a custom format could plausibly beat gzip(JSON). Full docs
// are where gzip is strongest (lots of repeated keys/replica strings to crush).
//
// The snapshot is the only payload carrying the `Node` STATE encoding rather than the op
// encoding, so it is the only one that moves when a container's element representation
// changes (design-docs/16). It is what a peer behind our compaction boundary is sent.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
//   node run-wire.js

const zlib = require("zlib");
const { Elm } = require("./headless.js");
const app = Elm.Headless.init();

let resolveDone = null;
app.ports.done.subscribe((r) => {
  if (resolveDone) resolveDone(r);
});

function wire(workload, n) {
  return new Promise((resolve) => {
    resolveDone = resolve;
    app.ports.command.send({
      workload,
      n,
      retain: false,
      reset: false,
      roundtrip: false,
      mode: "wire",
      iters: 0,
    });
  });
}

function gz(s) {
  // level 9 = best; matches what a serious deployment would use.
  return zlib.gzipSync(Buffer.from(s, "utf8"), { level: 9 }).length;
}

const SIZES = (process.env.SIZES || "100,400").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,tree").split(",");

(async () => {
  console.log("# Full document — raw JSON vs gzip(JSON)\n");
  console.log(["workload", "n", "raw", "gzip", "gzip%", "ratio"].join("\t"));
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      const { full } = await wire(workload, n);
      const raw = Buffer.byteLength(full, "utf8");
      const g = gz(full);
      console.log(
        [workload, n, raw, g, ((g / raw) * 100).toFixed(0) + "%", (raw / g).toFixed(1) + "×"].join("\t")
      );
    }
  }

  console.log("\n# Compacted snapshot (Node STATE encoding) — raw JSON vs gzip(JSON)\n");
  console.log(["workload", "n", "raw", "gzip", "gzip%", "ratio"].join("\t"));
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      const { snapshot } = await wire(workload, n);
      const raw = Buffer.byteLength(snapshot, "utf8");
      const g = gz(snapshot);
      console.log(
        [workload, n, raw, g, ((g / raw) * 100).toFixed(0) + "%", (raw / g).toFixed(1) + "×"].join("\t")
      );
    }
  }

  console.log("\n# One-edit delta — raw JSON vs gzip(JSON)\n");
  console.log(["workload", "n", "raw", "gzip", "gzip%", "note"].join("\t"));
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      const { delta } = await wire(workload, n);
      const raw = Buffer.byteLength(delta, "utf8");
      const g = gz(delta);
      // on tiny payloads gzip often EXPANDS (header + no dictionary warmup)
      const note = g >= raw ? "gzip ≥ raw (too small to help)" : "";
      console.log(
        [workload, n, raw, g, ((g / raw) * 100).toFixed(0) + "%", note].join("\t")
      );
    }
  }

  console.log(
    "\nInterpretation: if gzip(JSON) full-doc ratio is already high (small gzip%), a\n" +
      "custom format has little headroom on full docs. Deltas are where to look — if the\n" +
      "raw delta is small and gzip barely helps (or expands), a compact binary delta could\n" +
      "win. Compare these against the packed-format ceiling (run-packed.js)."
  );
})();
