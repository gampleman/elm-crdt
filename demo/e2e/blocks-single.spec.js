// Single-client block-editing tests in a real browser. These exercise the TipTap
// binding + CRDT round-trip that the pure-Elm suite can't see. The headline test is
// the reported first-load bug: a formatted document must render WITH its formatting on
// initial paint, not only after a subsequent edit.

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

test.describe("single client — block editing", () => {
  test.afterEach(closeAllClients);

  test("closing and reopening a file shows it WITH formatting on first paint (reported bug)", async ({ browser }) => {
    const { page } = await openClient(browser);

    await createFile(page, "notes");
    await type(page, "Heading here");
    await toolbar(page, "H1"); // format block 0 as h1
    await expectBlocks(page, 'h1:0"Heading here"');

    // Close back to the file list, then reopen — this remounts a FRESH editor whose
    // first paint is built from the block list (via the initial HTML-string content,
    // the path the reported bug was on: it dropped data-type/-depth on parse, so the
    // file loaded unformatted until the next edit). Assert the first painted state.
    await page.locator(".file-back", { hasText: "Files" }).click();
    await openFile(page, "notes");
    expect(await renderBlocks(page)).toBe('h1:0"Heading here"');
  });

  test("Enter splits into two blocks; type lands in the second", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "split");
    await type(page, "He");
    await page.keyboard.press("Enter");
    await page.keyboard.type("d");
    await expectBlocks(page, ':0"He" | :0"d"');
  });

  test("Enter in a bullet list continues the list (type/depth inherited)", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "list");
    await type(page, "first");
    await toolbar(page, "•"); // bullet list
    await expectBlocks(page, 'ul:0"first"');
    await editor(page).click();
    await page.keyboard.press("End");
    await page.keyboard.press("Enter");
    await page.keyboard.type("second");
    await expectBlocks(page, 'ul:0"first" | ul:0"second"');
  });

  test("delete-all then reformat does NOT resurrect old text", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "clear");
    await type(page, "alpha");
    await page.keyboard.press("Enter");
    await page.keyboard.type("bravo");
    await toolbar(page, "H1"); // format the second block
    await expectBlocks(page, ':0"alpha" | h1:0"bravo"');

    // select-all + delete (a multi-block delete in ONE transaction — the reconcile
    // path). Then type new text and format it. The old text must not come back.
    await editor(page).click();
    await page.keyboard.press("ControlOrMeta+a");
    await page.keyboard.press("Delete");
    await page.keyboard.type("fresh");
    await toolbar(page, "•");
    await expectBlocks(page, 'ul:0"fresh"');
  });
});
