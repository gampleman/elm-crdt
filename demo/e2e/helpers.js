// Shared helpers for the multi-client demo tests: opening a client (one browser
// context = one independent replica), navigating to the Files tab, creating/opening a
// file, and reading the editor's rendered block structure back out of the DOM.

import { expect } from "@playwright/test";

// Track every context a test opens so it can be torn down between tests. Leftover
// contexts stay connected to the shared relay and would leak their document into the
// next test's fresh clients (a real source of cross-test contamination — it masqueraded
// as a spurious "schema read error" during development). `closeAllClients` (called from
// an afterEach) disconnects them all.
const openContexts = new Set();

/** Open a fresh client (independent replica) at the app root. Returns { context, page }. */
export async function openClient(browser, opts = {}) {
  const context = await browser.newContext();
  openContexts.add(context);
  const page = await context.newPage();
  // isolated relay port (8091) so a stray tab on the default 8080 relay can't leak its
  // document into this test replica (a real contamination we hit during development).
  // `opts.historyCap` forces a tiny auto-compaction bound (default is 1000 in the app).
  const params = new URLSearchParams({ relayPort: "8091" });
  if (opts.historyCap != null) params.set("historyCap", String(opts.historyCap));
  await page.goto("/?" + params.toString());
  // wait for the Elm app + tab bar to render
  await page.locator(".tabs .tab", { hasText: "Files" }).waitFor();
  return { context, page };
}

/** Close every client opened this test (call from afterEach), so no peer leaks over. */
export async function closeAllClients() {
  for (const ctx of openContexts) {
    await ctx.close().catch(() => {});
  }
  openContexts.clear();
}

/** Switch a client to the Files tab. */
export async function gotoFiles(page) {
  await page.locator(".tabs .tab", { hasText: "Files" }).click();
  await page.locator(".files, .file-editor").first().waitFor();
}

/** Create a new file and open its editor. */
export async function createFile(page, name) {
  await gotoFiles(page);
  // scope to the Files pane's add-row (the checkpoint bar also uses `.add-row`)
  await page.locator(".files .add-row input").fill(name);
  await page.locator(".files .add-row button", { hasText: "create" }).click();
  await page.locator("crdt-richtext .pm-mount").waitFor();
}

/** Open an existing file (must already be on the Files tab / file list). */
export async function openFile(page, name) {
  await gotoFiles(page);
  await page.locator(".file-open", { hasText: name }).click();
  await page.locator("crdt-richtext .pm-mount").waitFor();
}

/** The editable ProseMirror surface for the open file. */
export function editor(page) {
  return page.locator("crdt-richtext .pm-mount .ProseMirror");
}

/** Click a toolbar button by its label (B, I, H1, •, 1., …). */
export async function toolbar(page, label) {
  await page.locator(`crdt-richtext .pm-toolbar button`, { hasText: new RegExp(`^${escapeRe(label)}$`) }).click();
}

/**
 * Read the current block structure from the DOM as a list of
 * { type, depth, text }. `type`/`depth` come from the data-* attributes the block
 * node renders (data-type / data-depth); text is the block's textContent.
 */
export async function readBlocks(page) {
  return page.evaluate(() => {
    const root = document.querySelector("crdt-richtext .ProseMirror");
    if (!root) return [];
    return Array.from(root.querySelectorAll(":scope > .block")).map((el) => {
      // Remote-peer caret widgets (`.remote-caret-rt`) are decorations, not document
      // text — strip them from a clone so their name labels don't pollute the read.
      // (The CRDT sync reads PM nodes, not DOM, so it's unaffected either way.)
      const clone = el.cloneNode(true);
      clone.querySelectorAll(".remote-caret-rt").forEach((c) => c.remove());
      return {
        type: el.getAttribute("data-type") || "",
        depth: parseInt(el.getAttribute("data-depth") || "0", 10) || 0,
        text: clone.textContent,
      };
    });
  });
}

/** Compact render matching the Elm tests' format: type:depth"text" | … */
export async function renderBlocks(page) {
  const blocks = await readBlocks(page);
  return blocks.map((b) => `${b.type}:${b.depth}"${b.text}"`).join(" | ");
}

/** Wait until `renderBlocks(page)` equals `expected` (polls; for convergence waits). */
export async function expectBlocks(page, expected) {
  await expect(async () => {
    expect(await renderBlocks(page)).toBe(expected);
  }).toPass();
}

/** Place the caret in the editor and type text (uses real keyboard events). */
export async function type(page, text) {
  await editor(page).click();
  await page.keyboard.type(text);
}

// --- inline marks -----------------------------------------------------------

// Toolbar labels → the mark name the CRDT stores (and the DOM tag TipTap renders).
export const MARK_BUTTONS = {
  bold: { label: "B", tag: "STRONG" },
  italic: { label: "I", tag: "EM" },
  underline: { label: "U", tag: "U" },
  strike: { label: "S", tag: "S" },
  code: { label: "</>", tag: "CODE" },
};

/**
 * The document's plain text plus, for each character, the set of inline marks active
 * on it — read straight from the rendered DOM by walking text nodes and their ancestor
 * mark tags (so this is what the user actually sees). Block boundaries contribute no
 * characters (matching the CRDT's char stream). Returns
 * `{ text, marks: string[][] }` where `marks[i]` is the sorted mark names on char `i`.
 */
export async function charMarks(page) {
  return page.evaluate((tagToMark) => {
    const root = document.querySelector("crdt-richtext .ProseMirror");
    if (!root) return { text: "", marks: [] };
    const text = [];
    const marks = [];
    root.querySelectorAll(":scope > .block").forEach((block) => {
      const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT);
      let node;
      while ((node = walker.nextNode())) {
        // collect mark tags on the ancestor chain up to the block
        const active = new Set();
        let el = node.parentElement;
        while (el && el !== block) {
          const m = tagToMark[el.tagName];
          if (m) active.add(m);
          el = el.parentElement;
        }
        const sorted = [...active].sort();
        for (const ch of node.textContent) {
          text.push(ch);
          marks.push(sorted);
        }
      }
    });
    return { text: text.join(""), marks };
  }, Object.fromEntries(Object.values(MARK_BUTTONS).map((m) => [m.tag, markNameFromTag(m.tag)])));
}

// map a DOM tag back to the CRDT mark name (inverse of MARK_BUTTONS)
function markNameFromTag(tag) {
  return Object.keys(MARK_BUTTONS).find((k) => MARK_BUTTONS[k].tag === tag);
}

/**
 * Select the document character range [from, to) (char offsets over the whole doc,
 * block boundaries excluded) by building a native DOM Range over the block text nodes
 * and applying it as the window selection. ProseMirror reads the DOM selection, so a
 * subsequent toolbar command marks exactly this range. Works across block boundaries.
 */
export async function selectCharRange(page, from, to) {
  await editor(page).click();
  await page.evaluate(
    ({ from, to }) => {
      const root = document.querySelector("crdt-richtext .ProseMirror");
      // flatten all text nodes across blocks, with a running char offset
      const points = []; // { node, nodeStart }
      let total = 0;
      root.querySelectorAll(":scope > .block").forEach((block) => {
        const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT);
        let node;
        while ((node = walker.nextNode())) {
          points.push({ node, nodeStart: total });
          total += node.textContent.length;
        }
      });
      // locate the (node, offset) for a doc char offset
      const locate = (charOff) => {
        for (const p of points) {
          const end = p.nodeStart + p.node.textContent.length;
          if (charOff <= end) return { node: p.node, offset: charOff - p.nodeStart };
        }
        const last = points[points.length - 1];
        return last
          ? { node: last.node, offset: last.node.textContent.length }
          : { node: root, offset: 0 };
      };
      const a = locate(from);
      const b = locate(to);
      const range = document.createRange();
      range.setStart(a.node, a.offset);
      range.setEnd(b.node, b.offset);
      const sel = window.getSelection();
      sel.removeAllRanges();
      sel.addRange(range);
    },
    { from, to }
  );
}

/** Click an inline-format toolbar button by mark name (bold/italic/underline/…). */
export async function clickMark(page, markName) {
  await toolbar(page, MARK_BUTTONS[markName].label);
}

function escapeRe(s) {
  return s.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}
