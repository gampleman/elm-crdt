module OpLogPhase1Gaps exposing (suite)

{-| **Known gaps / shortcuts in the Phase 0 op-log spike**, written as `todo`
tests so they stay loud (elm-test reports them as outstanding) instead of being
buried in a design doc. Each maps to a step in `docs/02-oplog.md`.

These are deliberately `Test.todo`, not failing assertions, because the spike's
shortcuts are _capability_ gaps, not latent bugs on valid input: our Lamport
clock (`Id.observe`) guarantees that if op A causally precedes B then
`counter(A) < counter(B)`, so the spike's "sort by `OpId`" already produces a
valid causal linearization for full-state convergence. What it can't yet do is
everything that needs _explicit_ causal structure (frontiers) or new actions.

As each item is implemented in Phase 1+, replace its `todo` with a real test.

-}

import Test exposing (Test, describe, todo)


suite : Test
suite =
    describe "op-log — known gaps to close (docs/02-oplog.md)"
        [ -- Phase 1 — DONE:
          --   Core (OpLogTests.elm): deps:Frontier, causal materialize +
          --     linearization-independence fuzz, checkout, op-union merge laws,
          --     SetReg LWW-by-stamp, SetPresence + LWW key removal.
          --   Public surface (OpDocTests.elm): Crdt.OpDoc wraps OpLog + a schema;
          --     path-addressed edits emit ops, merge = op-union, read materializes;
          --     convergence ported (concurrent edits converge both merge orders).
          --   Wire format (DONE): Crdt.OpJson encode/decode for ops; OpDoc.encode
          --     / decodeInto round-trip + idempotence tested; the demo now runs on
          --     OpDoc and syncs ops over the WebSocket.
          -- Expose Crdt.OpDoc publicly — DONE. `Seed` is opaque (Crdt.Internal,
          -- re-exported from Schema) so edits don't leak `Node`; the Node/Op-leaking
          -- introspection (allOps/cachedState/freshState) was replaced with
          -- non-leaking `opCount`/`cacheConsistent`; `Crdt.OpDoc` is in package
          -- exposed-modules and `elm make --docs` validates the public API.
          -- Phase 2: materialization performance (the make-or-break risk)
          --   DONE: incremental cache in Crdt.OpDoc (local ops fold onto the cached
          --   state; merges re-materialize). cachedState == freshState invariant
          --   tested (OpDocTests). Benchmark (benchmarks/) shows the read-path gate
          --   passed: fresh-reads O(N), cached-reads far flatter, gap widens with N.
          --   Also fixed a stack overflow in Rga ordering (20k-chain regression test).
          --   EDIT-PATH PERF (DONE): the append fast-path (OpDoc.lastAppend single
          --   slot) skips lastVisibleId's O(n) re-order for runs of appends — build
          --   dropped from ~21ms to ~7ms at N=200; benchmark read speedup now 24x at
          --   N=400. The intra-edit O(D*n) in text replace also fixed (Rga.visibleIds
          --   computes the order once). Append-order correctness tested (OpDocTests).
          -- Remaining Phase 2 tail (lower priority now):
          todo "general O(log n) visible-index access (random-index insert/remove still call toElementsInOrder); needs a persistent order index, not just the append slot"
        , todo "snapshot + tail: bound merge re-materialization by re-folding only ops after a cached snapshot frontier"

        -- Phase 3: delta sync — DONE
        --   `OpLog.opsAfter : Frontier -> OpStore -> List Op` returns the ops a
        --   peer at a given frontier lacks (complement of the causal ancestors) —
        --   correct regardless of delivery order / counter gaps, unlike a
        --   max-counter version vector (which the original todo wrongly proposed;
        --   our Lamport clock is gappy so a vector would skip ops). `OpDoc.version`
        --   gives the frontier, `OpDoc.encodeSince` ships only the delta. Tested
        --   (OpDocTests): delta brings a peer current, delta ≡ full state, empty
        --   delta for a caught-up peer. The demo now broadcasts deltas per edit
        --   with a hello/full-state handshake on connect for catch-up.
        -- Phase 4: collaborative history (the feature that drove the pivot)
        --   DONE: OpDoc.Version (a causal frontier) + version + readAt give
        --   collaborative time-travel — tested that a version captured before a
        --   merge excludes both the peer's concurrent ops and later local edits,
        --   while the live doc has everything (OpDocTests). This is the thing the
        --   local snapshot-stack Crdt.History could never do.
        -- Named checkpoints (DONE): OpDoc.Checkpoint (message + author + Version),
        --   OpDoc.checkpoint / checkpoints / checkpointMessage / -Author / -Version.
        --   Stored in the doc, emit no ops; readAt the version time-travels. Demo
        --   now uses these instead of a local checkpoint list. Tested.
        -- Remaining history work:
        , todo "explicit fork/branch type from a Version + a way to diff/compare two branches"
        , todo "undo/redo emit inverse ops that sync, replacing Crdt.History's local whole-root snapshot stacks"

        -- Phase 5: moves + GC
        -- MoveElem: ATTEMPTED then reverted. Re-pointing an element's RGA `origin`
        -- with LWW does NOT work: a list is a chained RGA (b.origin=a, c.origin=b),
        -- so moving the head "after c" makes a→c→b→a — an origin CYCLE. The
        -- cycle-safe ordering sweep (kept in Rga.toElementsInOrder) prevents data
        -- loss but falls back to id-order, so the move silently doesn't reorder.
        -- Correct list-move is a known-hard problem (cf. Kleppmann 2020, "Moving
        -- Elements in List CRDTs"); needs a dedicated position scheme, not origin
        -- re-pointing. Deferred deliberately rather than ship a broken reorder.
        , todo "MoveElem action: reordering a list item preserves its identity (needs a real move scheme, not origin LWW; see note)"

        -- Stable cursors (DONE): Crdt.Cursor (Anchor=Start|After OpId, Cursor =
        --   id-based Target + anchor; Range = pair of cursors). OpDoc.cursorAt /
        --   cursorOffset / cursorRange; Rga.liveCountThrough resolves robustly
        --   across concurrent inserts AND deletion of the anchor (tombstones
        --   retained). Tested (CursorTests): round-trip, track-on-insert-before,
        --   stable-on-insert-after, survive-anchor-delete, convergence, nested
        --   stability, range, JSON. Demo: live remote title caret (ch-approx) via
        --   Presence.custom carrying a Cursor. Crdt.Cursor is public.
        -- Counter (DONE): `Cnt` node = per-op signed contributions, value = sum,
        --   merge = union (Dict.merge with larger-delta tiebreak for robustness).
        --   `S.counter` combinator, `OpDoc.increment` / `Edit.increment` ops,
        --   JSON via Crdt.Json + OpJson. Tested: accumulate, decrement, concurrent
        --   +1/+1 => 2 (not LWW 1), order-independent, wire round-trip, fuzz laws.
        -- GC / shallow snapshot (DONE, phases 0–3): OpLog.compact (Node->Frontier
        --   ->OpStore->(Node,OpStore)) with a fuzzed read-equivalence proof;
        --   OpDoc.gc folds ops <= a cut into `base`, advances `baseFrontier`,
        --   drops them (read unchanged, time-travel below the cut lost — by
        --   design). Snapshot-transfer wire: encode/encodeSince send a snapshot
        --   (base+frontier+tail) when a peer is behind baseFrontier, decodeInto
        --   adopts it. Tested (GcTests): read-equiv, lagging-peer merge w/o
        --   resurrection, clock-safe, cursor-below-cut resolves, snapshot catch-up.
        --   SOUNDNESS is the caller's: dropping ops is only safe across merges if
        --   the cut is stable (single-replica/persist always safe). See docs/04-gc.md.
        -- Remaining GC tail (deferred): physically drop settled tombstones from
        -- `base`'s RGA (phase 4); server-mediated stable frontier (regime 2);
        -- decentralized agreement on a GC frontier (regime 3).
        , todo "tombstone compaction: physically drop settled (<=cut, unreferenced) tombstones from base's RGA"
        , todo "stable-frontier policy: relay-tracked or decentralized agreement on a safe GC cut across peers"

        -- Cross-cutting design note surfaced by the spike (still open)
        , todo "text op granularity: one op per character bloats the DAG — evaluate run-length insert ops"
        ]
