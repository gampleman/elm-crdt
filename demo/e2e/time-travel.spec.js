// Time-travel preview consistency. Scrubbing the History slider into the past shows a
// read-only PREVIEW of the document at that step. Everything on screen must reflect that
// previewed version — including the todos summary counter ("X / Y done"), which on the
// live path is rendered lazily from a referentially-stable slice of the LIVE doc. If the
// preview path still read that live slice, the counter would disagree with the previewed
// list below it (the reported bug). This test drives the real slider and asserts the
// counter matches the previewed list at each step.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients } from "./helpers.js";

test.describe("time-travel preview keeps the todos counter in sync", () => {
  test.afterEach(closeAllClients);

  async function gotoTab(page, label) {
    await page.locator(".tabs .tab", { hasText: label }).click();
    await page.waitForTimeout(150);
  }

  async function addTodo(page, text) {
    await gotoTab(page, "Todos");
    await page.locator(".add-row input").first().fill(text);
    await page.locator(".add-row button", { hasText: "add" }).first().click();
    await page.waitForTimeout(200);
  }

  // The "done" count shown in the summary ("2 / 3 done" -> 2) and the total.
  async function summaryCount(page) {
    const t = await page.locator(".todo-summary").first().innerText();
    const m = t.match(/(\d+)\s*\/\s*(\d+)/);
    return { done: Number(m[1]), total: Number(m[2]) };
  }

  // The count actually rendered in the list: number of todo rows, and how many are checked.
  async function listCount(page) {
    const total = await page.locator(".todos li.todo").count();
    const done = await page.locator('.todos li.todo input[type="checkbox"]:checked').count();
    return { done, total };
  }

  // Move the History slider to step `n` and let the preview render.
  async function scrubTo(page, n) {
    const range = page.locator(".scrub-range");
    await range.evaluate((el, value) => {
      el.value = String(value);
      el.dispatchEvent(new Event("input", { bubbles: true }));
    }, n);
    await page.waitForTimeout(150);
  }

  test("counter matches the previewed list as the slider moves back through history", async ({ browser }) => {
    const { page } = await openClient(browser);

    // Build some history: three todos, then mark the first done.
    await addTodo(page, "alpha");
    await addTodo(page, "beta");
    await addTodo(page, "gamma");
    await page.locator('.todos li.todo input[type="checkbox"]').first().click();
    await page.waitForTimeout(200);

    // Live: 1 of 3 done, and the summary agrees with the list.
    {
      const s = await summaryCount(page);
      const l = await listCount(page);
      expect(s).toEqual({ done: 1, total: 3 });
      expect(s).toEqual(l);
    }

    // Scrub back to the very start of history: an empty board. The summary MUST follow
    // the previewed list (0 / 0), not stay on the live 1 / 3.
    await scrubTo(page, 0);
    {
      const s = await summaryCount(page);
      const l = await listCount(page);
      expect(s).toEqual(l); // the core invariant: counter == previewed list
      expect(s.total).toBe(0);
    }

    // Walk forward a few steps; at every step the counter tracks the previewed list.
    for (const step of [1, 2, 3, 4, 5]) {
      await scrubTo(page, step);
      const s = await summaryCount(page);
      const l = await listCount(page);
      expect(s).toEqual(l);
    }
  });
});
