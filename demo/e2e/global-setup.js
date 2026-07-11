// Global setup: build the app once before the browser suite runs, so the static
// server serves fresh `elm.js` + `bundle.js`. Runs `npm run build` (elm make +
// esbuild). Throws (failing the whole run) if the build fails, so we never test
// stale artifacts.

import { execSync } from "node:child_process";

export default function globalSetup() {
  execSync("npm run build", { stdio: "inherit" });
}
