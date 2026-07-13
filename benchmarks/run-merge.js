// Merge-timing benchmark (see docs/12-referential-stability-and-diff.md).
//
// Measures how long `OpDoc.merge` takes to integrate a small remote delta into a
// size-`n` document. This is the every-incoming-message path in the demo, and the case
// that incremental merge (Part A) should speed up: today `merge` re-materializes the
// whole tree from base on every merge, so cost scales with the WHOLE doc, not the delta.
//
// Elm can't self-time, so the Elm worker runs `iters` identical merges internally and
// returns a forced checksum; we time the batch here with process.hrtime and divide.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js
//   node run-merge.js
//
// Env: SIZES=100,400,1000  WORKLOADS=demo,text,list,dict,tree  ITERS=200

const { Elm } = require("./headless.js");
const app = Elm.Headless.init();

let resolveDone = null;
app.ports.done.subscribe((stat) => {
  if (resolveDone) resolveDone(stat);
});

function mergeBench(workload, n, iters) {
  return new Promise((resolve) => {
    resolveDone = resolve;
    app.ports.command.send({
      workload,
      n,
      iters,
      mode: "merge",
      retain: false,
      reset: false,
      roundtrip: false,
    });
  });
}

const SIZES = (process.env.SIZES || "100,400,1000").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,dict,tree").split(",");
const ITERS = Number(process.env.ITERS || 200);

async function main() {
  console.log(`# Merge timing — ${ITERS} merges/measurement, ms per merge\n`);
  console.log(["workload", "n", "ms/merge"].join("\t"));

  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      // warm up (JIT) with a small batch, discard
      await mergeBench(workload, n, Math.max(5, Math.floor(ITERS / 10)));

      const t0 = process.hrtime.bigint();
      await mergeBench(workload, n, ITERS);
      const t1 = process.hrtime.bigint();

      const msPerMerge = Number(t1 - t0) / 1e6 / ITERS;
      console.log([workload, n, msPerMerge.toFixed(3)].join("\t"));
    }
  }
  process.exit(0);
}

main();
