// Memory / size benchmark runner (see docs/02-oplog.md, "measure first").
//
// Two kinds of number per workload + size:
//   • structural PROXIES from Elm — op count and encoded byte size. Deterministic,
//     and attributable to a structure (compare the per-container workloads).
//   • an absolute HEAP figure — process.memoryUsage().heapUsed while the Elm worker
//     RETAINS N built docs in its model (so the live-document heap lives in the
//     shared V8 process). GC-noisy, so run with --expose-gc and we force GC.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js
//   node --expose-gc run-mem.js

const { Elm } = require("./headless.js");
const app = Elm.Headless.init();

let resolveDone = null;
app.ports.done.subscribe((stat) => {
  if (resolveDone) resolveDone(stat);
});

// Build (workload, n). retain=keep the doc alive in Elm's model; reset=drop any
// previously-retained docs first.
function send(workload, n, { retain = false, reset = false, roundtrip = false } = {}) {
  return new Promise((resolve) => {
    resolveDone = resolve;
    app.ports.command.send({ workload, n, retain, reset, roundtrip });
  });
}

function gc() {
  if (global.gc) {
    global.gc();
    global.gc();
  }
}

const SIZES = (process.env.SIZES || "100,400,1000").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,dict,tree").split(",");
const HEAP_COPIES = Number(process.env.HEAP_COPIES || 20);

async function proxies() {
  console.log("# Structural proxies (deterministic)\n");
  console.log(["workload", "n", "ops", "bytes", "bytes/op"].join("\t"));
  const rows = {};
  for (const workload of WORKLOADS) {
    rows[workload] = [];
    for (const n of SIZES) {
      const s = await send(workload, n, { reset: true }); // don't retain here
      const bpo = (s.bytes / Math.max(1, s.ops)).toFixed(1);
      rows[workload].push(s);
      console.log([workload, n, s.ops, s.bytes, bpo].join("\t"));
    }
  }
  return rows;
}

// Retain `count` docs of (workload, n) in Elm's model, measuring the heap growth.
// The doc lives in the Elm side (shared V8 heap), so process.memoryUsage() sees it.
// `roundtrip` retains a decoded-from-wire copy (fresh ReplicaId per op) instead of
// a locally-built one (shared ReplicaId reference) — the interning-relevant case.
async function heapRetained(workload, n, count, roundtrip = false) {
  await send(workload, 1, { reset: true }); // clear retained
  gc();
  const before = process.memoryUsage().heapUsed;
  for (let i = 0; i < count; i++) {
    await send(workload, n, { retain: true, roundtrip });
  }
  gc();
  const after = process.memoryUsage().heapUsed;
  await send(workload, 1, { reset: true }); // release
  return (after - before) / count;
}

(async () => {
  const rows = await proxies();

  console.log("\n# Interpretation\n");
  const big = SIZES[SIZES.length - 1];
  for (const w of WORKLOADS) {
    const s = rows[w][rows[w].length - 1];
    console.log(`${w}(n=${big}):\t${s.ops} ops\t${s.bytes} bytes\t${(s.bytes / Math.max(1, s.ops)).toFixed(1)} b/op`);
  }

  if (global.gc) {
    console.log("\n# Heap retained per live doc — built (shared replica ref) vs received (decoded, fresh replica per op)\n");
    console.log(["workload", "n", "built(bytes)", "received(bytes)", "delta%"].join("\t"));
    for (const workload of WORKLOADS) {
      const built = await heapRetained(workload, big, HEAP_COPIES, false);
      const recv = await heapRetained(workload, big, HEAP_COPIES, true);
      const deltaPct = (((recv - built) / built) * 100).toFixed(0);
      console.log([workload, big, Math.round(built), Math.round(recv), deltaPct].join("\t"));
    }
  } else {
    console.log("\n(run with `node --expose-gc run-mem.js` for heap figures)");
  }
})();
