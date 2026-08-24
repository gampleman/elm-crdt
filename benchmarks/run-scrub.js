// History-scrub benchmark (see design-docs/15-pending-ops.md).
//
// Measures `Doc.readAt` at a mid-history version — the one read path that does NOT come
// from the maintained cache but **re-folds the op log from the base** (checkout the
// frontier's ancestors, then materialize). So this is where any cost added *per op in the
// fold* shows up, and it's the path a scrubber drag hits once per frame.
//
// The workload that matters here is `deletes` (half the log is `DeleteElem`): a delete is
// one of the three actions gated by the pending-ops precheck, so it pays a target walk plus
// a lookup for its subject on every fold. Registers/text/inserts skip the check
// syntactically and can't show a difference.
//
// Elm can't self-time, so the worker does `iters` reads internally and returns a forced
// checksum; we time the batch here and divide.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
//   node run-scrub.js
//
// Env: SIZES=100,400,1000  WORKLOADS=deletes,demo,list,text  ITERS=50

const { Elm } = require("./headless.js");
const app = Elm.Headless.init();

let resolveDone = null;
app.ports.done.subscribe((stat) => {
  if (resolveDone) resolveDone(stat);
});

function send(mode, workload, n, iters) {
  return new Promise((resolve) => {
    resolveDone = resolve;
    app.ports.command.send({
      workload,
      n,
      iters: iters || 0,
      mode,
      retain: false,
      reset: false,
      roundtrip: false,
    });
  });
}

const SIZES = (process.env.SIZES || "100,400,1000").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "deletes,demo,list,text").split(",");
const ITERS = Number(process.env.ITERS || 50);

async function ms(fn) {
  const t0 = process.hrtime.bigint();
  await fn();
  const t1 = process.hrtime.bigint();
  return Number(t1 - t0) / 1e6;
}

async function main() {
  console.log(`# History scrub (readAt, mid-history) — ${ITERS} reads/measurement, ms per read\n`);
  console.log(["workload", "n", "build(ms)", "ms/readAt"].join("\t"));

  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      // warm up (JIT), discard
      await send("build", workload, n);
      await send("scrub", workload, n, Math.max(5, Math.floor(ITERS / 10)));

      // The worker builds the doc once per invocation, *outside* its read loop — and for a
      // delete-heavy doc that build is the dominant term (visible-index resolution in
      // `remove` is the known O(n²) tail), so subtract one build, exactly as run-bench.js
      // does for its read column. Without this the build/ITERS term swamps the fold.
      const build = await ms(() => send("build", workload, n));
      const batch = await ms(() => send("scrub", workload, n, ITERS));

      const msPerRead = Math.max(0, (batch - build) / ITERS);
      console.log([workload, n, build.toFixed(3), msPerRead.toFixed(4)].join("\t"));
    }
  }
  process.exit(0);
}

main();
