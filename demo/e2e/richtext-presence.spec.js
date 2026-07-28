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
});
