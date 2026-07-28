// History scrubbing must also reflect in the rich-text editor. The editor is a custom
// element fed its content as a `docBlocks` property; while previewing a past version the
// Elm view must feed it the blocks AS OF that version (`Crdt.readBlocksAt`), so dragging
// the timeline shows the document as it was — not the live content frozen in the editor.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients, createFile, editor, type, renderBlocks } from "./helpers.js";

test.describe("history scrubbing reflects in the rich-text editor", () => {
  test.afterEach(closeAllClients);

  // Drag the History slider to step `n`.
  async function scrubTo(page, n) {
    const range = page.locator(".scrub-range");
    await range.evaluate((el, value) => {
      el.value = String(value);
      el.dispatchEvent(new Event("input", { bubbles: true }));
    }, n);
    await page.waitForTimeout(150);
  }

  // the visible text of the open editor (block texts concatenated).
  async function editorText(page) {
    return (await renderBlocks(page)).replace(/[^"]*"([^"]*)"/g, "$1");
  }

  test("scrubbing back shows the editor's earlier content, and returning restores it", async ({ browser }) => {
    const { page } = await openClient(browser);

    await createFile(page, "notes.md");
    await type(page, "hello");
    await expect(async () => {
      expect(await editorText(page)).toBe("hello");
    }).toPass();

    // the op count now is our "hello" checkpoint; every later keystroke adds ops past it.
    const opsAtHello = Number((await page.locator(".op-count").first().innerText()).match(/(\d+)/)[1]);

    // type more → "hello world"
    await type(page, " world");
    await expect(async () => {
      expect(await editorText(page)).toBe("hello world");
    }).toPass();

    // scrub back to the "hello" checkpoint → the editor must show a PREFIX of "hello world"
    // that stops at/before "hello" (i.e. the " world" tail is gone). We don't assert the
    // exact character (a step may land mid-run), only that history genuinely rewound the
    // editor: shorter than live, and no "world".
    await scrubTo(page, opsAtHello);
    await expect(async () => {
      const t = await editorText(page);
      expect("hello world".startsWith(t)).toBe(true); // a strict prefix of the live text
      expect(t.length).toBeLessThan("hello world".length); // actually rewound
      expect(t).not.toContain("world");
    }).toPass();

    // drag past the end → drops back to the live document; editor shows full content again
    await scrubTo(page, 999999);
    await expect(async () => {
      expect(await editorText(page)).toBe("hello world");
    }).toPass();
  });
});
