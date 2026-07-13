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

  // Two peers pressing Enter at the SAME position and typing into the new block
  // simultaneously. This once diverged because `diffStructural` diffed against the
  // lagging `_pendingBlocks` (only updated on Elm re-renders): after a local split with
  // no re-render, the next keystroke saw a block-count mismatch and fell back to a
  // non-commuting `reconcile`. Fixed by diffing against a running `_baseline` refreshed
  // after every transaction, so a split + typing classifies as the commutative
  // `split` + `text` primitives (which the Elm ConvergenceTests prove converge).
  test("concurrent typing into the same file converges (both texts survive)", async ({ browser }) => {
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
