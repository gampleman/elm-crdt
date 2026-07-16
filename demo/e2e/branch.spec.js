// Branching (Doc.fork) end-to-end, across two real replicas over the relay.
//
// The History panel's "⑃ branch" button forks a private branch: edits made on it stay
// LOCAL (never broadcast) until you "merge to main"; "discard" throws them away. This
// test drives the whole workflow with two clients and asserts the peer only sees the
// branch's work after a merge, never before — and not at all after a discard.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients } from "./helpers.js";

test.describe("branching (fork / merge / discard) across two clients", () => {
  test.afterEach(closeAllClients);

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

  // The todo texts currently rendered on a page.
  async function todoTexts(page) {
    return page.locator(".todos li.todo input[type='text'], .todos li.todo .text-input").evaluateAll(
      (els) => els.map((e) => e.value ?? e.textContent).filter((t) => t != null)
    );
  }

  // Number of todo rows shown.
  function todoCount(page) {
    return page.locator(".todos li.todo").count();
  }

  test("branch edits stay private until merged; discard drops them", async ({ browser }) => {
    const a = await openClient(browser);
    await addTodo(a.page, "shared");

    const b = await openClient(browser);
    await b.page.waitForTimeout(500); // B catches up via the hello handshake
    await gotoTab(b.page, "Todos");
    expect(await todoCount(b.page)).toBe(1); // both see the shared todo

    // A forks a private branch.
    await a.page.locator(".branch-row button", { hasText: "branch" }).click();
    await a.page.waitForTimeout(150);
    await expect(a.page.locator(".branch-banner")).toBeVisible();

    // A edits on the branch — B must NOT see it (edits are local while branched).
    await addTodo(a.page, "on-branch");
    expect(await todoCount(a.page)).toBe(2); // A sees its own branch edit
    await expect(a.page.locator(".branch-detail")).toContainText("ahead");
    await b.page.waitForTimeout(500);
    expect(await todoCount(b.page)).toBe(1); // B still only sees the shared todo

    // A merges the branch back — now B receives the branch's work.
    await a.page.locator(".branch-actions button", { hasText: "merge to main" }).click();
    await a.page.waitForTimeout(500);
    await expect(a.page.locator(".branch-banner")).toHaveCount(0); // banner gone
    await b.page.waitForTimeout(500);
    expect(await todoCount(b.page)).toBe(2); // B now sees the merged todo

    // A forks again, edits, then DISCARDS — the edit vanishes and B never sees it.
    await a.page.locator(".branch-row button", { hasText: "branch" }).click();
    await a.page.waitForTimeout(150);
    await addTodo(a.page, "throwaway");
    expect(await todoCount(a.page)).toBe(3);
    await a.page.locator(".branch-actions button", { hasText: "discard" }).click();
    await a.page.waitForTimeout(400);
    await expect(a.page.locator(".branch-banner")).toHaveCount(0);
    expect(await todoCount(a.page)).toBe(2); // back to the mainline (throwaway gone)
    await b.page.waitForTimeout(400);
    expect(await todoCount(b.page)).toBe(2); // B never saw the discarded edit
  });

  test("a peer's edits made during a branch survive the merge-back (concurrent work merges)", async ({ browser }) => {
    const a = await openClient(browser);
    await addTodo(a.page, "shared");

    const b = await openClient(browser);
    await b.page.waitForTimeout(500);
    await gotoTab(b.page, "Todos");

    // A branches and edits privately.
    await a.page.locator(".branch-row button", { hasText: "branch" }).click();
    await a.page.waitForTimeout(150);
    await addTodo(a.page, "from-A-branch");

    // Meanwhile B edits the mainline — A's branched view holds the mainline aside and
    // keeps merging B in the background.
    await addTodo(b.page, "from-B-main");
    await a.page.waitForTimeout(500);

    // A merges: the result must contain BOTH the branch edit and B's concurrent edit.
    await a.page.locator(".branch-actions button", { hasText: "merge to main" }).click();
    await a.page.waitForTimeout(600);

    expect(await todoCount(a.page)).toBe(3); // shared + from-A-branch + from-B-main
    await b.page.waitForTimeout(500);
    expect(await todoCount(b.page)).toBe(3); // and B converges to the same
  });
});
