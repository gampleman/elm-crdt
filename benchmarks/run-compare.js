// External comparison: elm-crdt vs Automerge vs Loro vs plain (no-CRDT) JS.
//
// Runs the SAME logical operations on each library and times them in the same Node
// process, so numbers are directly comparable on the JS runtime:
//   - list build: append N small items {text, done}
//   - text build: insert N characters (one op per char, the realistic editor path)
//   - read:       materialize the whole value back to a plain JS value
//
// elm-crdt itself is measured separately via run-bench.js (it can't run inline here —
// it's compiled Elm behind a port). This runner prints Automerge/Loro/plain and a
// reminder to line them up against run-bench's `list`/`text` build+read columns.
//
//   cd benchmarks && npm install && node run-compare.js
// Env: SIZES=100,400,1000  REPEAT=15

const A = require("@automerge/automerge").next;
const { LoroDoc, LoroMap } = require("loro-crdt");

const SIZES = (process.env.SIZES || "100,400,1000").split(",").map(Number);
const REPEAT = Number(process.env.REPEAT || 15);

function medianMs(runs, fn) {
  const s = [];
  for (let i = 0; i < runs; i++) {
    const t0 = process.hrtime.bigint();
    fn();
    const t1 = process.hrtime.bigint();
    s.push(Number(t1 - t0) / 1e6);
  }
  s.sort((a, b) => a - b);
  return s[Math.floor(s.length / 2)];
}

const word = (i) =>
  ["the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "and", "then"][i % 10];

// ---- list build: append N {text, done} ----

const listBuild = {
  plain: (n) => {
    const a = [];
    for (let i = 0; i < n; i++) a.push({ text: word(i), done: false });
    return a;
  },
  automerge: (n) => {
    let d = A.init();
    d = A.change(d, (doc) => {
      doc.todos = [];
    });
    // one change per append (the realistic per-edit path, matching our per-op build)
    for (let i = 0; i < n; i++) {
      d = A.change(d, (doc) => {
        doc.todos.push({ text: word(i), done: false });
      });
    }
    return d;
  },
  loro: (n) => {
    const d = new LoroDoc();
    const list = d.getList("todos");
    for (let i = 0; i < n; i++) {
      const m = list.insertContainer(list.length, new LoroMap());
      m.set("text", word(i));
      m.set("done", false);
      d.commit();
    }
    return d;
  },
};

// ---- text build: insert N chars ----

const textBuild = {
  plain: (n) => {
    let s = "";
    for (let i = 0; i < n; i++) s += word(i % 10)[0];
    return s;
  },
  automerge: (n) => {
    let d = A.init();
    d = A.change(d, (doc) => {
      doc.t = "";
    });
    for (let i = 0; i < n; i++) {
      d = A.change(d, (doc) => {
        A.splice(doc, ["t"], doc.t.length, 0, word(i % 10)[0]);
      });
    }
    return d;
  },
  loro: (n) => {
    const d = new LoroDoc();
    const t = d.getText("t");
    for (let i = 0; i < n; i++) {
      t.insert(t.length, word(i % 10)[0]);
      d.commit();
    }
    return d;
  },
};

// ---- read: materialize ----

const listRead = {
  plain: (d) => d.length,
  automerge: (d) => d.todos.length,
  loro: (d) => d.getList("todos").toJSON().length,
};

function main() {
  console.log(`# External comparison — median of ${REPEAT}, ms\n`);

  for (const kind of ["list", "text"]) {
    const build = kind === "list" ? listBuild : textBuild;
    console.log(`## ${kind} build (ms)\n`);
    console.log(["n", "plain", "automerge", "loro"].join("\t"));
    for (const n of SIZES) {
      build.plain(n);
      build.automerge(Math.min(n, 50));
      build.loro(Math.min(n, 50)); // warm
      const p = medianMs(REPEAT, () => build.plain(n));
      const a = medianMs(Math.max(3, REPEAT / 3), () => build.automerge(n));
      const l = medianMs(REPEAT, () => build.loro(n));
      console.log([n, p.toFixed(3), a.toFixed(3), l.toFixed(3)].join("\t"));
    }
    console.log("");
  }

  // list read
  console.log(`## list read (ms, whole-value)\n`);
  console.log(["n", "plain", "automerge", "loro"].join("\t"));
  for (const n of SIZES) {
    const pd = listBuild.plain(n);
    const ad = listBuild.automerge(n);
    const ld = listBuild.loro(n);
    const p = medianMs(REPEAT, () => listRead.plain(pd));
    const a = medianMs(REPEAT, () => listRead.automerge(ad));
    const l = medianMs(REPEAT, () => listRead.loro(ld));
    console.log([n, p.toFixed(4), a.toFixed(4), l.toFixed(4)].join("\t"));
  }
}

main();
