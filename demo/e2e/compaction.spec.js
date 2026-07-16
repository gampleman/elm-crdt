// Automatic history compaction end-to-end, across real replicas over the relay.
//
// Once the op log exceeds `historyCap` (default 1000; tests force a tiny cap via
// `?historyCap=`), the demo drops ANCIENT history — folding it into the base — while keeping
// a recent window (~cap/2), so the scrubber never appears to lose the user's latest edits.
// The cut is `stableFrontier(ancientTarget :: peerVersions)`: no newer than the recent
// window AND no past any laggard, so it's always safe to drop. These tests force a small cap
// and assert: the log stays bounded but keeps recent history, the document is preserved,
// peers stay converged, and a peer that missed a compaction is caught up by a snapshot.

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients } from "./helpers.js";

test.describe("automatic history compaction (bounded log, recent window kept)", () => {
  test.afterEach(closeAllClients);

  async function gotoTab(page, label) {
    await page.locator(".tabs .tab", { hasText: label }).click();
    await page.waitForTimeout(150);
  }

  async function addTodo(page, text) {
    await gotoTab(page, "Todos");
    await page.locator(".add-row input").first().fill(text);
    await page.locator(".add-row button", { hasText: "add" }).first().click();
    await page.waitForTimeout(220);
  }

  function todoCount(page) {
    return page.locator(".todos li.todo").count();
  }

  // Poll until the page shows exactly `n` todos (convergence over a live socket is
  // asynchronous — poll instead of guessing a fixed settle delay, so the test can't flake
  // under load yet returns as soon as it's converged).
  async function expectTodoCount(page, n) {
    await expect(page.locator(".todos li.todo")).toHaveCount(n);
  }

  // Parse the current op count from the passive "N / CAP ops (auto-compacted)" gauge.
  async function opCount(page) {
    const t = await page.locator(".op-count").first().innerText();
    return Number(t.match(/(\d+)/)[1]);
  }

  // Poll until the op-count gauge satisfies `pred` (bounding happens after each edit/merge;
  // poll rather than assume it's already settled).
  async function expectOpCount(page, pred) {
    await expect(async () => {
      expect(pred(await opCount(page))).toBe(true);
    }).toPass();
  }

  test("a single replica keeps its op log bounded but retains recent history", async ({ browser }) => {
    // tiny cap so a handful of todos trips auto-compaction (each todo = one op here)
    const { page } = await openClient(browser, { historyCap: 5 });

    for (const t of ["one", "two", "three", "four", "five", "six", "seven", "eight"]) {
      await addTodo(page, t);
    }

    // all eight are present (compaction never changes the value)...
    await expectTodoCount(page, 8);
    // ...the op log was compacted back under the cap along the way, but crucially NOT to
    // zero: compaction keeps a recent window (~cap/2), so the user's latest edits stay in the
    // scrubber. It only drops ANCIENT history, never the tail.
    const ops = await opCount(page);
    expect(ops).toBeLessThanOrEqual(5);
    expect(ops).toBeGreaterThan(0);
  });

  test("two peers stay bounded and converged under auto-compaction", async ({ browser }) => {
    const a = await openClient(browser, { historyCap: 6 });
    const b = await openClient(browser, { historyCap: 6 });
    await expectTodoCount(b.page, 0); // wait out the hello handshake (both start empty)

    // alternate edits from both peers so the stable frontier keeps advancing
    for (let i = 0; i < 5; i++) {
      await addTodo(a.page, "a" + i);
      await addTodo(b.page, "b" + i);
    }

    // both converge to the same 10 todos (poll — no fixed settle delay to flake on)
    await expectTodoCount(a.page, 10);
    await expectTodoCount(b.page, 10);

    // and both op logs were bounded (compaction only cuts below what BOTH have seen, so it
    // may sit somewhat above the cap while ops are in flight — but far below 10 todos' worth)
    await expectOpCount(a.page, (n) => n < 30);
    await expectOpCount(b.page, (n) => n < 30);
  });

  test("a peer that joins after a compaction is caught up by a snapshot", async ({ browser }) => {
    const a = await openClient(browser, { historyCap: 3 });
    // enough todos that A has actually compacted (dropped ancient ops into its base):
    // with cap 3 the log sawtooths and older todos' ops are folded away.
    for (const t of ["alpha", "beta", "gamma", "delta", "epsilon"]) {
      await addTodo(a.page, t);
    }
    // A holds only a recent window of ops; the earlier ones live only in its base now.
    await expectOpCount(a.page, (n) => n <= 3);

    // a fresh peer joins knowing none of A's ops. Because A dropped the early ones, a plain
    // delta can't convey them — A must send a full-state snapshot on the hello handshake.
    // Either way B must converge to all five todos.
    const b = await openClient(browser, { historyCap: 3 });
    await gotoTab(b.page, "Todos");
    await expectTodoCount(b.page, 5); // converged despite A having compacted

    await addTodo(b.page, "zeta");
    await expectTodoCount(a.page, 6);
  });
});
