// Packed-format CEILING experiment (ROADMAP "compact binary").
//
// The research (Automerge/Loro) says a custom format and a general compressor are
// COMPLEMENTARY layers, and the load-bearing additive wins over gzip are the ones
// gzip cannot compute: delta-encoding of op counters, and interning replica ids so a
// UUID/string column becomes a small-int column that delta/RLE-collapses. No public
// source gives the gzip(JSON) vs gzip(custom) number — that's the number that decides
// whether building this is worth it, so we measure it here.
//
// This is a SIZING sketch, not a production codec: we take the JSON the Elm harness
// already emits (`mode:"wire"`), re-encode its ops into a compact binary buffer
// applying those transforms, and compare raw + gzip of JSON vs packed, for the full
// doc and a one-edit delta. If gzip(packed) doesn't beat gzip(JSON) by a lot, a real
// custom format isn't worth it.
//
//   cd benchmarks
//   ../node_modules/.bin/elm make src/Headless.elm --output headless.js --optimize
//   node run-packed.js

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
    app.ports.command.send({ workload, n, retain: false, reset: false, roundtrip: false, mode: "wire" });
  });
}

const gz = (buf) => zlib.gzipSync(buf, { level: 9 }).length;
const rawDeflate = (buf) => zlib.deflateRawSync(buf, { level: 9 }).length;

// ---- varint (unsigned LEB128) + zigzag for signed ------------------------
function pushUVarint(bytes, n) {
  // n is a non-negative integer
  while (n >= 0x80) {
    bytes.push((n & 0x7f) | 0x80);
    n = Math.floor(n / 128);
  }
  bytes.push(n & 0x7f);
}
function zigzag(n) {
  return n < 0 ? -n * 2 - 1 : n * 2;
}
function pushSVarint(bytes, n) {
  pushUVarint(bytes, zigzag(n));
}

// ---- pack a payload's ops into a compact columnar-ish binary buffer -------
//
// A wire payload is { kind:"ops", ops:[ {id:[c,rep], deps:[[c,rep]...], a:{...}} ] }
// (or a snapshot; we only pack the ops list — snapshots also carry a base Node which
// we approximate by packing what op-shaped data we can and leaving the rest as-is).
//
// Transforms applied (the research-prioritized additive-over-gzip ones):
//   • intern replicas → small-int index (Loro/Automerge actor table)
//   • split OpId into (counter, repIdx) columns; DELTA-encode counters per column
//   • drop JSON keys/punctuation entirely (positional binary)
//   • varint everything
// Action bodies (targets, prims, seed nodes) are the messy part of a real codec; for
// a ceiling estimate we serialize each action compactly but not exhaustively — we
// capture the OpId-heavy structure (ids + deps + target elem ids) which the wire
// measurement showed dominates deltas, and treat action *payload* strings/prims as
// length-prefixed literal bytes (which gzip will handle just as it does in JSON).
function packOps(ops) {
  const bytes = [];

  // 1. replica table
  const repIndex = new Map();
  const reps = [];
  const internRep = (r) => {
    if (!repIndex.has(r)) {
      repIndex.set(r, reps.length);
      reps.push(r);
    }
    return repIndex.get(r);
  };
  // pre-scan to build a stable table
  const scanId = ([, r]) => internRep(r);
  for (const op of ops) {
    scanId(op.id);
    (op.deps || []).forEach(scanId);
  }

  // header: rep count + each rep string (length-prefixed utf8)
  pushUVarint(bytes, reps.length);
  for (const r of reps) {
    const b = Buffer.from(r, "utf8");
    pushUVarint(bytes, b.length);
    for (const x of b) bytes.push(x);
  }

  // 2. op count
  pushUVarint(bytes, ops.length);

  // 3. columns, delta-encoded where sequential.
  //    id counters (delta), id repIdx, dep counts + dep (counter delta within op,
  //    repIdx), then the action body as a compact literal.
  let prevIdCounter = 0;
  for (const op of ops) {
    const [c, r] = op.id;
    pushSVarint(bytes, c - prevIdCounter); // delta vs previous op's id counter
    prevIdCounter = c;
    pushUVarint(bytes, repIndex.get(r));

    const deps = op.deps || [];
    pushUVarint(bytes, deps.length);
    // deps sorted by counter so intra-op deltas are small & positive
    const sorted = deps.slice().sort((a, b) => a[0] - b[0]);
    let prevDep = 0;
    for (const [dc, dr] of sorted) {
      pushSVarint(bytes, dc - prevDep);
      prevDep = dc;
      pushUVarint(bytes, repIndex.get(dr));
    }

    // action: tag byte + compact body. We JSON-stringify the action *value* payload
    // (target/prim/seed) as literal bytes — this is the part a real codec would also
    // pack, but it's mostly content that gzip compresses equally in either format, so
    // leaving it as literal here does not flatter the packed format.
    const abody = Buffer.from(JSON.stringify(op.a), "utf8");
    pushUVarint(bytes, abody.length);
    for (const x of abody) bytes.push(x);
  }

  return Buffer.from(bytes);
}

// Extract the ops array from a wire payload (ops or snapshot kind).
function opsOf(payloadStr) {
  const p = JSON.parse(payloadStr);
  return p.ops || [];
}

const SIZES = (process.env.SIZES || "400,1000").split(",").map(Number);
const WORKLOADS = (process.env.WORKLOADS || "demo,text,list,tree").split(",");

function row(label, jsonStr) {
  const json = Buffer.from(jsonStr, "utf8");
  const packed = packOps(opsOf(jsonStr));
  const jGz = gz(json);
  const pGz = gz(packed);
  const pDef = rawDeflate(packed);
  return [
    label,
    json.length,
    jGz,
    packed.length,
    pGz,
    pDef,
    // the decision number: gzip(packed) vs gzip(JSON)
    ((pGz / jGz) * 100).toFixed(0) + "%",
  ].join("\t");
}

(async () => {
  console.log("# FULL DOC — packed vs JSON (raw + gzip). Decision col = gzip(packed)/gzip(JSON)\n");
  console.log(["case", "json", "gz(json)", "packed", "gz(pkd)", "defl(pkd)", "gzP/gzJ"].join("\t"));
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      const { full } = await wire(workload, n);
      console.log(row(`${workload}/${n}`, full));
    }
  }

  console.log("\n# ONE-EDIT DELTA — packed vs JSON\n");
  console.log(["case", "json", "gz(json)", "packed", "gz(pkd)", "defl(pkd)", "gzP/gzJ"].join("\t"));
  for (const workload of WORKLOADS) {
    for (const n of SIZES) {
      const { delta } = await wire(workload, n);
      console.log(row(`${workload}/${n}`, delta));
    }
  }

  console.log(
    "\nRead: 'gzP/gzJ' < 100% means gzip(packed) beats gzip(JSON) by that much. Also\n" +
      "compare defl(pkd) (raw DEFLATE, no gzip framing) for tiny deltas where gzip's 18-byte\n" +
      "frame dominates. Decide: is the win large enough to justify a real columnar codec?"
  );
})();
