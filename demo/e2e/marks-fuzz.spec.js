// Fuzzed inline-mark test. Builds a random document that already carries a mix of
// inline marks and block formatting, then repeatedly: picks a random char range and a
// random mark, applies it via the toolbar, and asserts the ONLY change to the
// per-character mark grid is that mark over exactly the selected range — every other
// character, and every other mark on the selected chars, is untouched. Text is never
// altered by a mark op.
//
// The expectation is computed by mirroring the toggle on a JS model of the current
// grid (TipTap toggles: if every char in the selection already has the mark, the
// button clears it; otherwise it sets it on all). This catches off-by-one offsets,
// ranges that leak past block boundaries, and marks that stomp neighbours.

import { test, expect } from "@playwright/test";
import {
  openClient,
  closeAllClients,
  createFile,
  editor,
  charMarks,
  selectCharRange,
  clickMark,
  toolbar,
  MARK_BUTTONS,
} from "./helpers.js";

// Deterministic PRNG so a failure reproduces from its seed (Math.random is banned in
// the repo's tooling anyway). Simple mulberry32.
function rng(seed) {
  return function () {
    seed |= 0;
    seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// `code` is excluded from the fuzz set on purpose: TipTap's code mark is *exclusive*
// (it excludes all other inline marks and vice versa), so it doesn't compose with the
// independent-mark oracle below. The remaining marks (bold/italic/underline/strike)
// are mutually independent, which is what lets us assert an exact per-char grid.
const MARK_NAMES = Object.keys(MARK_BUTTONS).filter((m) => m !== "code");
const WORDS = ["alpha", "bravo", "charlie", "delta", "echo", "foxtrot"];
const BLOCK_TYPES = ["h1", "h2", "blockquote", "ul", "ol"];

// Build a random document via real keystrokes + toolbar clicks: several blocks of
// words, some blocks typed with Enter between them, some formatted as headings/lists,
// and a few pre-existing inline marks. Returns nothing; leaves the editor populated.
async function buildRandomDoc(page, rand) {
  await editor(page).click();
  const nBlocks = 1 + Math.floor(rand() * 3); // 1..3 blocks
  for (let b = 0; b < nBlocks; b++) {
    if (b > 0) await page.keyboard.press("Enter");
    const nWords = 1 + Math.floor(rand() * 2);
    const words = [];
    for (let w = 0; w < nWords; w++) words.push(WORDS[Math.floor(rand() * WORDS.length)]);
    await page.keyboard.type(words.join(" "));
    // maybe format this block
    if (rand() < 0.4) {
      const t = BLOCK_TYPES[Math.floor(rand() * BLOCK_TYPES.length)];
      await toolbar(page, { h1: "H1", h2: "H2", blockquote: "❝", ul: "•", ol: "1." }[t]);
    }
  }
  // a couple of pre-existing inline marks over random ranges
  const { text } = await charMarks(page);
  const n = text.length;
  for (let k = 0; k < 2 && n > 1; k++) {
    const a = Math.floor(rand() * n);
    const c = a + 1 + Math.floor(rand() * (n - a));
    await selectCharRange(page, a, c);
    await clickMark(page, MARK_NAMES[Math.floor(rand() * MARK_NAMES.length)]);
  }
}

// The expected grid after toggling `mark` over [from, to): TipTap sets the mark on the
// whole selection unless every char in it already has the mark, in which case it clears.
function toggle(grid, from, to, mark) {
  const allHave = grid.slice(from, to).every((s) => s.includes(mark));
  return grid.map((s, i) => {
    if (i < from || i >= to) return s;
    const set = new Set(s);
    if (allHave) set.delete(mark);
    else set.add(mark);
    return [...set].sort();
  });
}

test.describe("inline marks — fuzzed", () => {
  test.afterEach(closeAllClients);

  for (let seed = 1; seed <= 6; seed++) {
    test(`random doc + random mark ops preserve exact ranges (seed ${seed})`, async ({ browser }) => {
      const rand = rng(seed * 97 + 13);
      const { page } = await openClient(browser);
      await createFile(page, `fz${seed}`);
      await buildRandomDoc(page, rand);

      // snapshot the starting grid
      let snap = await charMarks(page);
      const n = snap.text.length;
      test.skip(n < 2, "doc too short to select a range");

      // apply several random mark ops, checking the exact delta each time
      for (let step = 0; step < 5; step++) {
        const cur = await charMarks(page);
        const len = cur.text.length;
        const from = Math.floor(rand() * len);
        const to = from + 1 + Math.floor(rand() * (len - from));
        const mark = MARK_NAMES[Math.floor(rand() * MARK_NAMES.length)];

        const expected = toggle(cur.marks, from, to, mark);

        await selectCharRange(page, from, to);
        await clickMark(page, mark);

        await expect(async () => {
          const after = await charMarks(page);
          // text unchanged by a mark op
          expect(after.text).toBe(cur.text);
          // grid matches the expected toggle exactly (every char, every mark)
          expect(after.marks.map((m) => m.join(","))).toEqual(expected.map((m) => m.join(",")));
        }).toPass({ timeout: 5000 });
      }
    });
  }
});
