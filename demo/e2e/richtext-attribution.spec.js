// History attribution: scrubbing the timeline back to a past edit shows WHO made that
// edit, as a caret at the spot it touched — reusing the presence-caret widget, but sourced
// from the op log (each op's author) rather than ephemeral presence. So it works even for a
// peer's past edit, and even after that peer has gone.

import { test, expect } from "@playwright/test";
import {
  openClient,
  closeAllClients,
  createFile,
  openFile,
  editor,
  type,
  expectBlocks,
} from "./helpers.js";

// Drag client `page`'s History slider to step `n`.
async function scrubTo(page, n) {
  const range = page.locator(".scrub-range");
  await range.evaluate((el, value) => {
    el.value = String(value);
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }, n);
  await page.waitForTimeout(150);
}

// The remote/attribution caret name flags currently drawn in the editor.
async function caretNames(page) {
  return page.evaluate(() =>
    Array.from(document.querySelectorAll("crdt-richtext .remote-caret-rt-flag")).map(
      (el) => el.textContent
    )
  );
}

async function historyLength(page) {
  const label = await page.locator(".scrub-label").innerText();
  const m = label.match(/of (\d+)|\((\d+) edits\)/);
  return Number(m[1] || m[2]);
}

// The character offset of the (single) attribution caret within its block: the number of
// text characters that precede the `.remote-caret-rt` widget in the same block. Returns
// null if no caret is shown.
async function caretOffset(page) {
  return page.evaluate(() => {
    const caret = document.querySelector("crdt-richtext .remote-caret-rt");
    if (!caret) return null;
    const block = caret.closest(".block");
    if (!block) return null;
    // walk text nodes in document order; stop when we reach the caret widget.
    const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT);
    let count = 0;
    let node;
    while ((node = walker.nextNode())) {
      // a text node inside the caret widget itself (its name flag) doesn't count
      if (caret.contains(node)) continue;
      const rel = caret.compareDocumentPosition(node);
      if (rel & Node.DOCUMENT_POSITION_FOLLOWING) break; // node is after the caret
      count += node.textContent.length;
    }
    return count;
  });
}

test.describe("history scrubbing attributes past edits to their author", () => {
  test.afterEach(closeAllClients);

  test("scrubbing to a peer's past edit shows their caret at the edited spot", async ({ browser }) => {
    // Two clients edit ONE fresh file (a unique name so no prior test's file ops on the
    // shared relay collide with this history). A types; B appends. Both edits land in the
    // shared history tagged with their real authors.
    const file = "attr-" + Date.now() + ".md";

    const a = await openClient(browser);
    await createFile(a.page, file);
    await type(a.page, "hello");
    await expectBlocks(a.page, ':0"hello"');

    const b = await openClient(browser);
    await openFile(b.page, file);
    await expectBlocks(b.page, ':0"hello"');
    await editor(b.page).click();
    await b.page.keyboard.press("End");
    await b.page.keyboard.type(" world");
    await expectBlocks(b.page, ':0"hello world"');

    // A sees the convergence, then scrubs its OWN timeline through the whole history.
    await expectBlocks(a.page, ':0"hello world"');

    // Wait until A's OP LOG (not just the rendered value) has fully merged B's edits:
    // "hello world" is 11 character-insert ops. The rendered blocks can update from a
    // pushed `docBlocks` a beat before every op has merged, so scrubbing immediately could
    // read a short, still-A-only history — poll the op count until it's settled.
    await expect(async () => {
      expect(await historyLength(a.page)).toBeGreaterThanOrEqual(11);
    }).toPass();

    const len = await historyLength(a.page);

    // Walk every step, collecting the attribution caret's author. A step whose edit
    // touches this file shows exactly one caret; a step touching only other files (stray
    // relay peers can add unrelated ops) shows none — so tolerate 0-or-1 per step.
    const authors = new Set();
    for (let step = 1; step <= len; step++) {
      await scrubTo(a.page, step >= len ? len - 1 : step);
      const names = await caretNames(a.page);
      expect(names.length).toBeLessThanOrEqual(1);
      names.forEach((n) => authors.add(n));
    }

    // This file's history holds A's "hello" and B's " world", so scrubbing attributed at
    // least two distinct authors at their edited spots — the whole point: a PAST edit is
    // attributed to whoever actually made it, sourced from the op log, not live presence.
    expect(authors.size).toBeGreaterThanOrEqual(2);

    // Returning to live leaves scrub mode (attribution carets are a scrub-only overlay).
    await scrubTo(a.page, len);
  });

  test("the attribution caret sits just AFTER an inserted character, not before it", async ({ browser }) => {
    // One client typing "abc" appends one char per step. When we scrub to a step, the
    // caret for that step's insertion must land at the END of the (previewed) text — just
    // PAST the char it added — not one to its left (the reported off-by-one). Since every
    // step here appends, the correct offset is exactly the previewed text's length.
    const file = "pos-" + Date.now() + ".md";
    const { page } = await openClient(browser);
    await createFile(page, file);
    await type(page, "abc");
    await expectBlocks(page, ':0"abc"');

    await expect(async () => {
      expect(await historyLength(page)).toBeGreaterThanOrEqual(3);
    }).toPass();

    const len = await historyLength(page);

    // the previewed block's text at the current scrub position (caret widget stripped).
    const previewText = () =>
      page.evaluate(() => {
        const b = document.querySelector("crdt-richtext .block");
        if (!b) return "";
        const c = b.cloneNode(true);
        c.querySelectorAll(".remote-caret-rt").forEach((x) => x.remove());
        return c.textContent;
      });

    // Walk every step; whenever a caret is shown, it must sit at the end of the text (an
    // append's caret is just past the inserted char). Assert this happened for the steps
    // that grew the text — at least the two that appended "b" and "c" after the first char.
    let checked = 0;
    for (let step = 1; step <= len; step++) {
      await scrubTo(page, step);
      await page.waitForTimeout(50);
      const off = await caretOffset(page);
      if (off === null) continue; // a step that didn't touch this file's text
      const text = await previewText();
      expect(off).toBe(text.length); // caret AFTER the last (just-inserted) char
      if (text.length > 0) checked++;
    }
    expect(checked).toBeGreaterThanOrEqual(2);
  });
});
