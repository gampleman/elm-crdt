// Rich-text presence: two replicas editing the same file should see each other's
// caret drawn inside the ProseMirror editor. A's caret is broadcast as a stable
// `Crdt.Cursor` on the presence channel, resolved by B to a live character offset,
// and rendered as a `.remote-caret-rt` widget decoration.

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

// A remote caret's name flags, read from the DOM (each `.remote-caret-rt-flag`'s text).
async function remoteCaretNames(page) {
  return page.evaluate(() =>
    Array.from(document.querySelectorAll("crdt-richtext .remote-caret-rt-flag")).map(
      (el) => el.textContent
    )
  );
}

// Where the (single) remote caret sits, as a document-wide CHARACTER offset — the number
// of text characters (across all blocks; block boundaries contribute none) that precede
// the widget. This is the space caret offsets are defined in. Null when none is rendered.
async function remoteCaretDocOffset(page) {
  return page.evaluate(() => {
    const caret = document.querySelector("crdt-richtext .remote-caret-rt");
    if (!caret) return null;
    const root = document.querySelector("crdt-richtext .ProseMirror");
    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    let chars = 0;
    let node;
    while ((node = walker.nextNode())) {
      if (caret.contains(node)) continue; // the widget's own name flag
      if (caret.compareDocumentPosition(node) & Node.DOCUMENT_POSITION_FOLLOWING) break;
      chars += node.textContent.length;
    }
    return chars;
  });
}

test.describe("rich-text presence carets", () => {
  test.afterEach(closeAllClients);

  test("a peer's caret appears in the other peer's editor", async ({ browser }) => {
    const a = await openClient(browser);
    await createFile(a.page, "carets");
    await type(a.page, "hello world");
    await expectBlocks(a.page, ':0"hello world"');

    const b = await openClient(browser);
    await openFile(b.page, "carets");
    await expectBlocks(b.page, ':0"hello world"');

    // A places its caret in the middle of the text (click + Home so it's deterministic).
    await editor(a.page).click();
    await a.page.keyboard.press("Home");

    // B should render exactly one remote caret (A's), labeled with A's peer name.
    await expect(async () => {
      const names = await remoteCaretNames(b.page);
      expect(names.length).toBe(1);
    }).toPass();

    // and A, with no other peer's caret to show, renders none of its own.
    expect(await remoteCaretNames(a.page)).toEqual([]);
  });

  test("in a MULTI-block document the caret lands on the right character", async ({ browser }) => {
    // Regression: cursor offsets are CHARACTER offsets, but they used to be resolved
    // against the raw element sequence — which includes one block marker per block
    // boundary — so a caret in block 2 rendered one character off per preceding block.
    // A single-block doc can't catch that (the spaces coincide), hence this test.
    const a = await openClient(browser);
    await createFile(a.page, "multiblock");
    await type(a.page, "hello");
    await a.page.keyboard.press("Enter");
    await a.page.keyboard.type("world");
    await expectBlocks(a.page, ':0"hello" | :0"world"');

    const b = await openClient(browser);
    await openFile(b.page, "multiblock");
    await expectBlocks(b.page, ':0"hello" | :0"world"');

    // A's caret is at the end of "world" from typing it: 10 characters precede it
    // ("hello" + "world"). Before the fix the block marker inflated the resolved
    // offset, so B rendered the caret one character off.
    await expect(async () => {
      expect(await remoteCaretDocOffset(b.page)).toBe(10);
    }).toPass();

    // Move A's caret to the start of "world": 5 characters precede it. (In flat
    // character space this is the same position as end-of-"hello"; which side of the
    // block boundary it renders on is presentation, the character offset is the
    // contract.)
    await a.page.keyboard.press("Home");
    await expect(async () => {
      expect(await remoteCaretDocOffset(b.page)).toBe(5);
    }).toPass();

    // And inside a block past the boundary: after "wo" → 7 characters precede.
    await a.page.keyboard.press("ArrowRight");
    await a.page.keyboard.press("ArrowRight");
    await expect(async () => {
      expect(await remoteCaretDocOffset(b.page)).toBe(7);
    }).toPass();
  });
});
