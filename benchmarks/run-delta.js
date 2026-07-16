// Delta-ingest benchmark — the demo's real per-incoming-message path.
//
// Measures `Doc.decodeInto` of a one-edit `encodeSince` delta into a size-`n` doc. Unlike
// full-doc `merge` (run-merge.js), which unions two N-sized op stores and is inherently
// O(N), this is the case that SHOULD cost O(delta): the wire payload carries one op. If it
// still scales with `n`, the cost is merge *bookkeeping* (clock catch-up scanning the whole
// tree, added-ops scan over the whole store), not the fold.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js
//   node run-delta.js
//
// Env: SIZES=100,400,1000,2000  WORKLOADS=demo,text,list,dict,tree  ITERS=200

const { Elm } = require("./headless.js");
const app = Elm.Headless.init();

let resolveDone = null;
app.ports.done.subscribe((stat) => {
  if (resolveDone) resolveDone(stat);
});

function deltaBench(workload, n, iters) {
  return new Promise((resolve) => {
    resolveDone = resolve;
    app.ports.command.send({
      workload,
      n,
      iters,
      mode: "delta",
      retain: false,
      reset: false,
      roundtrip: false,
    });
  });
}

const SIZES = (process.env.SIZES || "100,400,1000,2000").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,dict,tree").split(",");
const ITERS = Number(process.env.ITERS || 200);

async function main() {
  console.log(`# Delta-ingest timing — ${ITERS} decodes/measurement, ms per delta decode\n`);
  console.log(["workload", "n", "ms/delta"].join("\t"));

  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      // warm up (JIT) with a small batch, discard
      await deltaBench(workload, n, Math.max(5, Math.floor(ITERS / 10)));

      const t0 = process.hrtime.bigint();
      await deltaBench(workload, n, ITERS);
      const t1 = process.hrtime.bigint();

      const msPerDelta = Number(t1 - t0) / 1e6 / ITERS;
      console.log([workload, n, msPerDelta.toFixed(3)].join("\t"));
    }
  }
  process.exit(0);
}

main();
