// The Outline tab (movable tree) indent / outdent controls, end-to-end in the browser.
//
// Each node's row has → (indent: nest under the preceding sibling), ← (outdent: promote to
// the grandparent), + (add child), ✕ (remove). This test drives the buttons on the real app
// and asserts the tree structure actually changes — indent nests a node, outdent promotes it
// back — via the rendered nesting (a nested node lives in a `.outline-node .outline`).

import { test, expect } from "@playwright/test";
import { openClient, closeAllClients } from "./helpers.js";

test.describe("outline indent / outdent", () => {
  test.afterEach(closeAllClients);

  async function gotoOutline(page) {
    await page.locator(".tabs .tab", { hasText: "Outline" }).click();
    // the empty outline `ul` has no height (hidden to Playwright), so wait on the always-
    // visible "+ node" button to know the tab has rendered.
    await page.locator(".board button", { hasText: "+ node" }).waitFor();
  }

  // Add `n` root nodes, then label them by index. (Labelling is separate from adding: filling
  // a node's input while another "+ node" is mid-render can race the reconcile.)
  async function addRoots(page, labels) {
    for (let i = 0; i < labels.length; i++) {
      await page.locator(".board button", { hasText: "+ node" }).click();
      await expect(page.locator(".outline-root > .outline-node")).toHaveCount(i + 1);
    }
    for (let i = 0; i < labels.length; i++) {
      await page.locator(".outline-root > .outline-node").nth(i).locator("input").first().fill(labels[i]);
      await page.waitForTimeout(100);
    }
  }

  // The nth top-level node's control button by its glyph. `.outline-root` is the top list;
  // `.outline` alone also matches nested lists, so scope to the root to count top-level nodes.
  function rootBtn(page, i, glyph) {
    return page.locator(".outline-root > .outline-node").nth(i).locator(".outline-row button", { hasText: glyph }).first();
  }

  test("→ nests a node under its previous sibling; ← promotes it back", async ({ browser }) => {
    const { page } = await openClient(browser);
    await gotoOutline(page);

    await addRoots(page, ["First", "Second"]);

    // two roots, nothing nested yet
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(2);
    await expect(page.locator(".outline-node .outline .outline-node")).toHaveCount(0);
    // the first node's indent is disabled (no preceding sibling); the second's is enabled
    await expect(rootBtn(page, 0, "→")).toBeDisabled();
    await expect(rootBtn(page, 1, "→")).toBeEnabled();

    // indent "Second" under "First"
    await rootBtn(page, 1, "→").click();

    // now one root, and exactly one nested node
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(1);
    await expect(page.locator(".outline-node .outline .outline-node")).toHaveCount(1);
    const nested = page.locator(".outline-node .outline .outline-node").first();
    await expect(nested.locator("input").first()).toHaveValue("Second");

    // outdent it back to a root via its ← button
    await nested.locator(".outline-row button", { hasText: "←" }).first().click();

    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(2);
    await expect(page.locator(".outline-node .outline .outline-node")).toHaveCount(0);
  });

  test("outdent promotes one level at a time (not straight to root)", async ({ browser }) => {
    const { page } = await openClient(browser);
    await gotoOutline(page);

    // A, B, C at root; indent C under B, then B(+C) under A → A > B > C (depth 2)
    await addRoots(page, ["A", "B", "C"]);
    await rootBtn(page, 2, "→").click(); // C under B
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(2);
    await rootBtn(page, 1, "→").click(); // B (with C) under A
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(1);

    // C is now two levels deep
    const deepC = page.locator(".outline-node .outline .outline-node .outline .outline-node").first();
    await expect(deepC.locator("input").first()).toHaveValue("C");

    // outdent C once → it should sit beside B under A (depth 1), NOT jump to root
    await deepC.locator(".outline-row button", { hasText: "←" }).first().click();

    // still a single root (A); C rose one level to be A's child, alongside B
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(1);
    // A now has two direct children, in order B then C (C landed right AFTER its former
    // parent B, not appended elsewhere); none at the third level
    const aKids = page.locator(".outline-root > .outline-node > .outline > .outline-node");
    await expect(aKids).toHaveCount(2);
    await expect(aKids.nth(0).locator("input").first()).toHaveValue("B");
    await expect(aKids.nth(1).locator("input").first()).toHaveValue("C");
    await expect(page.locator(".outline-node .outline .outline-node .outline .outline-node")).toHaveCount(0);
  });

  test("outdent lands a node right after its former parent, not at the end of the list", async ({ browser }) => {
    // reproduces the reported bug: with a LATER root present, outdenting must place the
    // node immediately below its old parent — not append it to the bottom of the roots.
    const { page } = await openClient(browser);
    await gotoOutline(page);

    // roots: P, Z. Give P a child K (indent K under P).
    await addRoots(page, ["P", "K", "Z"]);
    // indent K (index 1) under P (index 0)
    await rootBtn(page, 1, "→").click();
    // now roots are [P, Z]; P has child K
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(2);
    const nestedK = page.locator(".outline-root > .outline-node").nth(0).locator(".outline > .outline-node").first();
    await expect(nestedK.locator("input").first()).toHaveValue("K");

    // outdent K → it becomes a root. It must sit between P and Z (right after P),
    // NOT after Z at the end.
    await nestedK.locator(".outline-row button", { hasText: "←" }).first().click();

    const roots = page.locator(".outline-root > .outline-node");
    await expect(roots).toHaveCount(3);
    await expect(roots.nth(0).locator("input").first()).toHaveValue("P");
    await expect(roots.nth(1).locator("input").first()).toHaveValue("K"); // right after P
    await expect(roots.nth(2).locator("input").first()).toHaveValue("Z");
  });

  test("outdent adopts the following siblings, preserving visible order", async ({ browser }) => {
    // reproduces the reported bug: a child with a LATER sibling. Outdenting it must keep the
    // top-to-bottom reading order — so the following siblings become ITS children (standard
    // outliner behavior), rather than the node leapfrogging below them.
    const { page } = await openClient(browser);
    await gotoOutline(page);

    // Build  F > [B1, X, B2]  (F a root with three children in that order)
    await addRoots(page, ["F", "B1", "X", "B2"]);
    await rootBtn(page, 1, "→").click(); // B1 under F
    await rootBtn(page, 1, "→").click(); // X under F  (X is now index 1 among roots)
    await rootBtn(page, 1, "→").click(); // B2 under F
    await expect(page.locator(".outline-root > .outline-node")).toHaveCount(1);
    const fKids = page.locator(".outline-root > .outline-node").nth(0).locator("> .outline > .outline-node");
    await expect(fKids).toHaveCount(3);
    await expect(fKids.nth(1).locator("input").first()).toHaveValue("X");

    // outdent X (the middle child, with following sibling B2)
    await fKids.nth(1).locator(".outline-row button", { hasText: "←" }).first().click();

    // F keeps B1; X is now F's second child (a sibling of F at root — wait, no: X rose to
    // root level, right after F). The reading order top-to-bottom must stay F, B1, X, B2.
    const roots = page.locator(".outline-root > .outline-node");
    await expect(roots).toHaveCount(2); // F and X
    await expect(roots.nth(0).locator("input").first()).toHaveValue("F");
    await expect(roots.nth(1).locator("input").first()).toHaveValue("X");
    // F retains only B1
    const fKidsAfter = roots.nth(0).locator("> .outline > .outline-node");
    await expect(fKidsAfter).toHaveCount(1);
    await expect(fKidsAfter.nth(0).locator("input").first()).toHaveValue("B1");
    // B2 (the former following sibling) is now a child of X
    const xKids = roots.nth(1).locator("> .outline > .outline-node");
    await expect(xKids).toHaveCount(1);
    await expect(xKids.nth(0).locator("input").first()).toHaveValue("B2");
  });
});
