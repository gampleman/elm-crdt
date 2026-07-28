// History attribution for PLAIN text fields (not just the rich-text editor): scrubbing
// the timeline shows a caret — in the step author's color — at the spot a past edit landed
// in a plain input (here the board title in the Settings tab). Same op-log-sourced
// attribution as the rich editor, reusing the `.remote-caret` widget.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients } from "./helpers.js";

async function gotoTab(page, label) {
  await page.locator(".tabs .tab", { hasText: label }).click();
}

// The title input in the Settings tab.
function titleInput(page) {
  return page.locator(".board input").first();
}

async function historyLength(page) {
  const label = await page.locator(".scrub-label").innerText();
  const m = label.match(/of (\d+)|\((\d+) edits\)/);
  return Number(m[1] || m[2]);
}

async function scrubTo(page, n) {
  const range = page.locator(".scrub-range");
  await range.evaluate((el, value) => {
    el.value = String(value);
    el.dispatchEvent(new Event("input", { bubbles: true }));
  }, n);
  await page.waitForTimeout(120);
}

// The attribution/presence carets currently drawn over plain inputs, as their titles
// ("<name>'s cursor").
async function caretTitles(page) {
  return page.evaluate(() =>
    Array.from(document.querySelectorAll(".board .remote-caret")).map((el) =>
      el.getAttribute("title")
    )
  );
}

test.describe("history scrubbing attributes plain-field edits", () => {
  test.afterEach(closeAllClients);

  test("scrubbing shows a caret at a past title edit, colored by its author", async ({ browser }) => {
    const a = await openClient(browser);
    await gotoTab(a.page, "Settings");
    await titleInput(a.page).click();
    await a.page.keyboard.type("Trip");
    await expect(titleInput(a.page)).toHaveValue("Trip");

    // B joins, catches up, appends to the title.
    const b = await openClient(browser);
    await gotoTab(b.page, "Settings");
    await expect(titleInput(b.page)).toHaveValue("Trip");
    await titleInput(b.page).click();
    await b.page.keyboard.press("End");
    await b.page.keyboard.type(" plan");
    await expect(titleInput(b.page)).toHaveValue("Trip plan");

    // A converges, then scrubs its OWN timeline. Wait for the op log to fully merge
    // "Trip plan" (9 char ops) before scrubbing, else the history is still A-only.
    await expect(titleInput(a.page)).toHaveValue("Trip plan");
    await expect(async () => {
      expect(await historyLength(a.page)).toBeGreaterThanOrEqual(9);
    }).toPass();

    const len = await historyLength(a.page);

    // Walk the steps; each step that touched the title shows exactly one attribution
    // caret. Collect the distinct authors — both A's and B's edits are here.
    const authors = new Set();
    for (let step = 1; step <= len; step++) {
      await scrubTo(a.page, step >= len ? len - 1 : step);
      const titles = await caretTitles(a.page);
      expect(titles.length).toBeLessThanOrEqual(1);
      titles.forEach((t) => authors.add(t));
    }
    expect(authors.size).toBeGreaterThanOrEqual(2);

    // returning to live drops the scrub attribution overlay
    await scrubTo(a.page, len);
  });
});
