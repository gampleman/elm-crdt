// Latency benchmark: build + read, ms per operation.
//
//   build: time constructing a size-`n` document (one build per port round-trip,
//          averaged over REPEAT invocations). This is the "apply N edits" cost.
//   read:  the Elm worker does `iters` reads of a size-`n` doc internally and returns
//          a forced checksum; we time the batch and divide. This is the read-model
//          materialization cost (served from the maintained cache).
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
//   node run-bench.js
//
// Env: SIZES=100,400,1000  WORKLOADS=demo,text,list,dict,tree  ITERS=100  REPEAT=30

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
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,dict,tree").split(",");
const ITERS = Number(process.env.ITERS || 100);
const REPEAT = Number(process.env.REPEAT || 30);

// Median wall-clock (ms) of `fn` over `n` runs — median resists GC/JIT outliers
// better than mean for these coarse port round-trips.
async function medianMs(runs, fn) {
  const samples = [];
  for (let i = 0; i < runs; i++) {
    const t0 = process.hrtime.bigint();
    await fn();
    const t1 = process.hrtime.bigint();
    samples.push(Number(t1 - t0) / 1e6);
  }
  samples.sort((a, b) => a - b);
  return samples[Math.floor(samples.length / 2)];
}

async function main() {
  console.log(`# Latency — build (whole doc) and read (${ITERS} reads, ms/read)\n`);
  console.log(["workload", "n", "build(ms)", "read(ms/read)"].join("\t"));

  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      // warm up JIT
      await send("build", workload, n);
      await send("read", workload, n, ITERS);

      const build = await medianMs(REPEAT, () => send("build", workload, n));

      // read: batch of ITERS internal reads, timed and divided; the build inside is
      // shared per invocation, so subtract a build to isolate the read loop.
      const readBatch = await medianMs(Math.max(5, Math.floor(REPEAT / 3)), () =>
        send("read", workload, n, ITERS)
      );
      const readPer = Math.max(0, (readBatch - build) / ITERS);

      console.log(
        [workload, n, build.toFixed(3), readPer.toFixed(4)].join("\t")
      );
    }
  }
  process.exit(0);
}

main();
