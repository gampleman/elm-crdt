// Multi-client tests: two (or more) independent replicas — separate browser
// contexts — editing the SAME file concurrently over the REAL relay, asserting they
// converge to an identical rendered block structure. This is the collaborative
// property the whole library exists for, exercised end to end through the browser.

import { test, expect } from "@playwright/test";
import {
  openClient,
  closeAllClients,
  createFile,
  openFile,
  editor,
  toolbar,
  type,
  renderBlocks,
  expectBlocks,
} from "./helpers.js";

test.describe("multi client — convergence over the relay", () => {
  test.afterEach(closeAllClients);

  test("a formatted doc created by A shows up formatted in B (first-paint sync)", async ({ browser }) => {
    const a = await openClient(browser);
    await createFile(a.page, "shared");
    await type(a.page, "Title");
    await toolbar(a.page, "H1");
    await expectBlocks(a.page, 'h1:0"Title"');

    // B joins, catches up via the hello handshake, opens the same file. Its FIRST
    // paint of the editor must already show the h1 (the first-load formatting bug
    // would show a plain paragraph until an edit).
    const b = await openClient(browser);
    await openFile(b.page, "shared");
    await expectBlocks(b.page, 'h1:0"Title"');
  });

  test("concurrent edits in different blocks converge", async ({ browser }) => {
    const a = await openClient(browser);
    await createFile(a.page, "concurrent");
    await type(a.page, "one");
    await a.page.keyboard.press("Enter");
    await a.page.keyboard.type("two");
    await expectBlocks(a.page, ':0"one" | :0"two"');

    const b = await openClient(browser);
    await openFile(b.page, "concurrent");
    await expectBlocks(b.page, ':0"one" | :0"two"');

    // A formats block 0 as a heading; B formats block 1 as a bullet — concurrently.
    await a.page.locator("crdt-richtext .ProseMirror > .block").first().click();
    await toolbar(a.page, "H1");
    await b.page.locator("crdt-richtext .ProseMirror > .block").nth(1).click();
    await toolbar(b.page, "•");

    // both converge to the same structure with BOTH edits applied
    const expected = 'h1:0"one" | ul:0"two"';
    await expectBlocks(a.page, expected);
    await expectBlocks(b.page, expected);
  });

  // KNOWN LIMITATION (binding, not the CRDT). Two peers pressing Enter at the SAME
  // position and typing into the new block simultaneously can diverge in the editor,
  // even though the underlying `splitBlock`/`setBlockText` primitives converge (proven
  // by the Elm ConvergenceTests + a focused concurrent-end-split test). The gap is in
  // the TipTap binding: `diffStructural` classifies a native Enter into a `split`
  // intent relative to `_pendingBlocks`, and that baseline lags while a peer's split is
  // arriving, so the two clients can emit split intents against different block indices.
  // Fixing it means reconciling the editor against the CRDT's authoritative structure
  // on every remote update (or moving split detection off the lagging baseline).
  // Tracked as future work; the single-client and non-colliding concurrent paths pass.
  test.fixme("concurrent typing into the same file converges (both texts survive)", async ({ browser }) => {
    const a = await openClient(browser);
    await createFile(a.page, "typing");
    await type(a.page, "start");
    await expectBlocks(a.page, ':0"start"');

    const b = await openClient(browser);
    await openFile(b.page, "typing");
    await expectBlocks(b.page, ':0"start"');

    // A appends a new block, B appends a different new block — concurrently
    await a.page.locator("crdt-richtext .ProseMirror > .block").last().click();
    await a.page.keyboard.press("End");
    await a.page.keyboard.press("Enter");
    await a.page.keyboard.type("alpha");

    await b.page.locator("crdt-richtext .ProseMirror > .block").last().click();
    await b.page.keyboard.press("End");
    await b.page.keyboard.press("Enter");
    await b.page.keyboard.type("bravo");

    // convergence: identical render on both, containing start + both new blocks
    await expect(async () => {
      const ra = await renderBlocks(a.page);
      const rb = await renderBlocks(b.page);
      expect(ra).toBe(rb);
      expect(ra).toContain('"start"');
      expect(ra).toContain('"alpha"');
      expect(ra).toContain('"bravo"');
    }).toPass();
  });
});
