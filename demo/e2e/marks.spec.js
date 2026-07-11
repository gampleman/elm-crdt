// Inline-mark tests: select a specific character range, click a formatting button,
// and assert that EXACTLY that range gained the mark — nothing before, nothing after,
// across block boundaries and over documents that already carry inline/block
// formatting. This is where boundary off-by-ones hide (e.g. a mark landing one char
// early per preceding block marker — a real bug this suite caught).
//
// The oracle is `charMarks(page)`: the per-character mark set read from the rendered
// DOM. We compare against an independently-computed expectation, so a wrong offset,
// an over-wide range, or a mark leaking past a block boundary all fail loudly.

import { test, expect } from "@playwright/test";
import {
  openClient,
  closeAllClients,
  createFile,
  editor,
  type,
  charMarks,
  selectCharRange,
  clickMark,
  MARK_BUTTONS,
} from "./helpers.js";

test.describe("inline marks over selections", () => {
  test.afterEach(closeAllClients);

  // Assert that exactly the doc-char range [from, to) carries `markName` (and every
  // other char does not carry it), after applying it via the toolbar.
  async function expectMarkExactly(page, from, to, markName) {
    await selectCharRange(page, from, to);
    await clickMark(page, markName);
    await expect(async () => {
      const { text, marks } = await charMarks(page);
      const got = marks.map((m) => m.includes(markName));
      const want = text.split("").map((_, i) => i >= from && i < to);
      expect(got).toEqual(want);
    }).toPass();
  }

  test("bold a mid-word range marks exactly that range", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "m1");
    await type(page, "hello world");
    await expectMarkExactly(page, 2, 7, "bold"); // "llo w"
  });

  test("bold a range in the SECOND block targets the right chars (offset across a marker)", async ({ browser }) => {
    // This is the exact shape of the offset bug: with a block marker in the sequence,
    // char offsets must skip it. Bold "de" in block 1 of "ab"|"cdef".
    const { page } = await openClient(browser);
    await createFile(page, "m2");
    await type(page, "ab");
    await page.keyboard.press("Enter");
    await page.keyboard.type("cdef");
    // doc chars: a b c d e f  → bold [3,5) = "de"
    await expectMarkExactly(page, 3, 5, "bold");
  });

  test("bold across a block boundary marks both sides but nothing outside", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "m3");
    await type(page, "abc");
    await page.keyboard.press("Enter");
    await page.keyboard.type("def");
    // doc chars: a b c d e f → bold [2,5) = "cde" (spans the boundary)
    await expectMarkExactly(page, 2, 5, "bold");
  });

  test("stacking a second mark on the same range keeps the first (both active)", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "m4");
    await type(page, "hello world");
    await selectCharRange(page, 2, 7);
    await clickMark(page, "bold");
    await selectCharRange(page, 2, 7);
    await clickMark(page, "italic");
    await expect(async () => {
      const { text, marks } = await charMarks(page);
      for (let i = 0; i < text.length; i++) {
        const inRange = i >= 2 && i < 7;
        expect(marks[i].includes("bold")).toBe(inRange);
        expect(marks[i].includes("italic")).toBe(inRange);
      }
    }).toPass();
  });

  test("marking a sub-range of an existing bold run leaves the rest bold", async ({ browser }) => {
    const { page } = await openClient(browser);
    await createFile(page, "m5");
    await type(page, "hello world");
    await selectCharRange(page, 0, 11); // bold everything
    await clickMark(page, "bold");
    // now italicize a middle slice; bold must remain on all, italic only on [3,6)
    await selectCharRange(page, 3, 6);
    await clickMark(page, "italic");
    await expect(async () => {
      const { text, marks } = await charMarks(page);
      for (let i = 0; i < text.length; i++) {
        expect(marks[i].includes("bold")).toBe(true);
        expect(marks[i].includes("italic")).toBe(i >= 3 && i < 6);
      }
    }).toPass();
  });

  test("code is exclusive: applying it over a bold range replaces bold with code", async ({ browser }) => {
    // TipTap's `code` mark excludes other inline marks. Pinned so the behavior is
    // intentional, not accidental — the fuzz suite excludes `code` for this reason.
    const { page } = await openClient(browser);
    await createFile(page, "m6");
    await type(page, "hello world");
    await selectCharRange(page, 0, 11);
    await clickMark(page, "bold");
    await selectCharRange(page, 2, 7);
    await clickMark(page, "code");
    await expect(async () => {
      const { text, marks } = await charMarks(page);
      for (let i = 0; i < text.length; i++) {
        const inCode = i >= 2 && i < 7;
        expect(marks[i].includes("code")).toBe(inCode);
        // code excludes bold, so the code range is NOT bold; the rest stays bold
        expect(marks[i].includes("bold")).toBe(!inCode);
      }
    }).toPass();
  });
});
