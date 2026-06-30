// Phase 2 go/no-go benchmark runner (see docs/02-oplog.md).
//
// Drives the Elm `Headless` worker from Node, timing the cached read path vs a
// full re-materialization at several document sizes. Prints a table; the gate is
// that "cached" stays ~flat in N while "fresh" grows with N.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js
//   node run.js

const { performance } = require("perf_hooks");

// Elm's compiled IIFE assigns `Elm` to `this`, which for a CommonJS module is
// `module.exports`. So require() returns the Elm object directly.
const { Elm } = require("./headless.js");

const app = Elm.Headless.init();

const SIZES = [50, 100, 200, 400];
const TRIALS = 3;

// Each command resolves on the next `done` message.
let resolveDone = null;
app.ports.done.subscribe((checksum) => {
  if (resolveDone) resolveDone(checksum);
});

function time(mode, n) {
  return new Promise((resolve) => {
    const t0 = performance.now();
    resolveDone = () => resolve(performance.now() - t0);
    app.ports.command.send({ mode, n });
  });
}

async function best(mode, n) {
  let min = Infinity;
  for (let i = 0; i < TRIALS; i++) min = Math.min(min, await time(mode, n));
  return min;
}

(async () => {
  // Subtract the shared build cost so the columns isolate the 100-read loop.
  console.log("N\tbuild(ms)\tcached-reads(ms)\tfresh-reads(ms)\tspeedup");
  for (const n of SIZES) {
    const build = await best("build", n);
    const cached = await best("cached", n);
    const fresh = await best("fresh", n);
    const cr = Math.max(0.01, cached - build);
    const fr = Math.max(0.01, fresh - build);
    console.log(
      `${n}\t${build.toFixed(2)}\t\t${cr.toFixed(2)}\t\t${fr.toFixed(2)}\t\t${(fr / cr).toFixed(0)}x`
    );
  }
  process.exit(0);
})();
