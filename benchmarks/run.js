// Phase 2 go/no-go benchmark runner (see design-docs/02-oplog.md).
//
// Drives the Elm `Headless` worker from Node, timing the cached read path
// (`Doc.read`, served from the maintained cache) against a full re-materialization of the
// same content (`Doc.readAt` at the head version, which re-folds every op from the base —
// `readAt` has no cache fast path, so this really does fold).
//
// NOTE on what this shows today. The original Phase-2 gate expected "cached ~flat in N,
// fresh grows with N". Measured now, BOTH columns grow linearly, and the ratio is only
// ~1.2–2×: `read` re-runs the whole typed schema decode on every call, and at these sizes
// that decode dominates the fold it avoids. So this table prices *the fold the cache saves*
// (≈20–60% of a read, most visible on `deletes`), not an asymptotic difference. The decode
// cost itself is `run-bench.js`'s read column; the fold in isolation is `run-scrub.js`.
//
//   cd benchmarks
//   npm run build          # or: ../node_modules/.bin/elm make src/Headless.elm --output headless.js
//   node run.js
//
// Env: SIZES=50,100,200,400  WORKLOADS=demo  ITERS=100  TRIALS=3

const { performance } = require("perf_hooks");

// Elm's compiled IIFE assigns `Elm` to `this`, which for a CommonJS module is
// `module.exports`. So require() returns the Elm object directly.
const { Elm } = require("./headless.js");

const app = Elm.Headless.init();

const SIZES = (process.env.SIZES || "50,100,200,400").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo").split(",");
const ITERS = Number(process.env.ITERS || 100);
const TRIALS = Number(process.env.TRIALS || 3);

// Each command resolves on the next `done` message.
let resolveDone = null;
app.ports.done.subscribe((stat) => {
  if (resolveDone) resolveDone(stat);
});

// The port expects the whole `Command` record — a partial object fails to decode and
// crashes the worker, so send every field.
function time(mode, workload, n, iters) {
  return new Promise((resolve) => {
    const t0 = performance.now();
    resolveDone = () => resolve(performance.now() - t0);
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

async function best(mode, workload, n, iters) {
  let min = Infinity;
  for (let i = 0; i < TRIALS; i++) min = Math.min(min, await time(mode, workload, n, iters));
  return min;
}

(async () => {
  console.log(`# Cached vs re-folded reads — ${ITERS} reads/measurement\n`);
  console.log("workload\tN\tbuild(ms)\tcached-reads(ms)\tfresh-reads(ms)\tspeedup");
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      // Subtract the shared build cost so the columns isolate the read loop.
      const build = await best("build", workload, n);
      const cached = await best("read", workload, n, ITERS);
      const fresh = await best("fresh", workload, n, ITERS);
      const cr = Math.max(0.01, cached - build);
      const fr = Math.max(0.01, fresh - build);
      console.log(
        `${workload}\t${n}\t${build.toFixed(2)}\t\t${cr.toFixed(2)}\t\t${fr.toFixed(2)}\t\t${(fr / cr).toFixed(0)}x`
      );
    }
  }
  process.exit(0);
})();
