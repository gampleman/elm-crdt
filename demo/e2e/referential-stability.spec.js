// Referential-stability + diff verification (docs/12). The demo keeps a stable decoded
// `todosSlice` in its Model, refreshed ONLY when a change actually touched the todos
// (asked of the merge/ingest `Diff` via the typed `Ref.touched`). A `Html.Lazy` view
// over that slice (`viewTodoSummary`) logs "render:todo-summary" whenever it actually
// re-renders. So the number of those logs is a direct probe of whether `Html.Lazy`
// fast-paths: an unrelated edit must NOT re-render the summary; a todos edit must.
//
// This is the payoff of incremental merge (identity-preserving) + the diff (selective
// re-read), demonstrated for both local and remote (peer) edits.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients, gotoFiles } from "./helpers.js";

test.describe("referential stability (Html.Lazy skips unrelated re-renders)", () => {
  test.afterEach(closeAllClients);

  // Count "render:todo-summary" console lines on a page.
  function summaryRenderCounter(page) {
    const c = { n: 0 };
    page.on("console", (m) => {
      if (m.text().includes("render:todo-summary")) c.n += 1;
    });
    return c;
  }

  async function gotoTab(page, label) {
    await page.locator(".tabs .tab", { hasText: label }).click();
    await page.waitForTimeout(150);
  }

  async function addTodo(page, text) {
    await gotoTab(page, "Todos");
    await page.locator(".add-row input").first().fill(text);
    await page.locator(".add-row button", { hasText: "add" }).first().click();
    await page.waitForTimeout(250);
  }

  test("local: editing the title does not re-render the todos summary; editing a todo does", async ({ browser }) => {
    const { page } = await openClient(browser);
    const c = summaryRenderCounter(page);

    await addTodo(page, "task one");

    // edit an UNRELATED part (the board title, on Settings)
    await gotoTab(page, "Settings");
    const before = c.n;
    await page.locator(".field-wrap input").first().fill("A New Title");
    await page.waitForTimeout(300);
    expect(c.n - before).toBe(0); // lazy skipped: todos slice reference unchanged

    // edit a todo — the summary must re-render
    const beforeTodo = c.n;
    await addTodo(page, "task two");
    expect(c.n - beforeTodo).toBeGreaterThanOrEqual(1);
  });

  test("remote: a peer editing settings doesn't re-render my todos summary; a peer editing a todo does", async ({ browser }) => {
    const a = await openClient(browser);
    const c = summaryRenderCounter(a.page);
    await addTodo(a.page, "shared task");
    await gotoTab(a.page, "Todos"); // A observes on the Todos tab

    const b = await openClient(browser);
    await b.page.waitForTimeout(400); // B catches up via the hello handshake

    // B edits the title (unrelated to todos)
    await gotoTab(b.page, "Settings");
    const beforeTitle = c.n;
    await b.page.locator(".field-wrap input").first().fill("Peer Title");
    await b.page.waitForTimeout(500);
    expect(c.n - beforeTitle).toBe(0); // A's summary not re-rendered by the remote title edit

    // B adds a todo — A's summary must re-render
    const beforeTodo = c.n;
    await addTodo(b.page, "peer task");
    await a.page.waitForTimeout(500);
    expect(c.n - beforeTodo).toBeGreaterThanOrEqual(1);
  });
});
