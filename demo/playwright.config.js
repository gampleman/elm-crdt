// Playwright config for the elm-crdt demo's multi-client browser tests.
//
// These tests drive the REAL app (built `elm.js` + `bundle.js`) in a headless
// browser, with the REAL relay, and open multiple independent browser contexts as
// separate replicas — exactly the collaborative setup the demo is for. This is the
// layer the pure-Elm suite can't reach: the TipTap binding, the caret/reconcile JS,
// the port round-trip, and genuine two-client convergence over a socket.
//
// `webServer` boots two processes before the suite: the WebSocket relay (8091) and a
// static file server for the built app (8100). The build itself is a global-setup
// step (see global-setup.js) so we always test fresh artifacts.
//
// Port 8100 (not 8000) so the suite doesn't collide with a docs-preview server people
// commonly run on 8000 — reusing that would serve the docs, not the demo, and every test
// would see a blank app. Both `reuseExistingServer: false` so we always spin up our own.

import { defineConfig, devices } from "@playwright/test";

export default defineConfig({
  testDir: "./e2e",
  // collaboration timing (socket round-trips + convergence) needs a little slack
  timeout: 30_000,
  expect: { timeout: 10_000 },
  fullyParallel: false, // tests share one relay + server; keep them sequential
  workers: 1,
  reporter: process.env.CI ? "line" : [["list"]],
  globalSetup: "./e2e/global-setup.js",
  use: {
    baseURL: "http://localhost:8100",
    trace: "retain-on-failure",
  },
  projects: [{ name: "chromium", use: { ...devices["Desktop Chrome"] } }],
  webServer: [
    {
      command: "PORT=8091 node server/relay.js",
      port: 8091,
      reuseExistingServer: false,
      stdout: "ignore",
    },
    {
      // serve the built app statically on 8100 (not 8000 — see header note); the build
      // runs in global-setup first. Never reuse an existing server on this port.
      command: "npx http-server -p 8100 -c-1 --silent .",
      port: 8100,
      reuseExistingServer: false,
      stdout: "ignore",
    },
  ],
});
