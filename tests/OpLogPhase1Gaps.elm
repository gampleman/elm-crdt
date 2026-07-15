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

import Expect
import Test exposing (Test, describe, test, todo)


suite : Test
suite =
    describe "op-log — known gaps to close (docs/02-oplog.md)"
        [ -- Phase 1 — DONE:
          --   Core (OpLogTests.elm): deps:Frontier, causal materialize +
          --     linearization-independence fuzz, checkout, op-union merge laws,
          --     SetReg LWW-by-stamp, SetPresence + LWW key removal.
          --   Public surface (DocTests.elm): Crdt.Doc wraps OpLog + a schema;
          --     path-addressed edits emit ops, merge = op-union, read materializes;
          --     convergence ported (concurrent edits converge both merge orders).
          --   Wire format (DONE): Crdt.OpJson encode/decode for ops; Doc.encode
          --     / decodeInto round-trip + idempotence tested; the demo now runs on
          --     Doc and syncs ops over the WebSocket.
          -- Expose Crdt.Doc publicly — DONE. `Seed` is opaque (Crdt.Internal,
          -- re-exported from Schema) so edits don't leak `Node`; the Node/Op-leaking
          -- introspection (allOps/cachedState/freshState) was replaced with
          -- non-leaking `opCount`/`cacheConsistent`; `Crdt.Doc` is in package
          -- exposed-modules and `elm make --docs` validates the public API.
          -- Phase 2: materialization performance (the make-or-break risk)
          --   DONE: incremental cache in Crdt.Doc (local ops fold onto the cached
          --   state; merges re-materialize). cachedState == freshState invariant
          --   tested (DocTests). Benchmark (benchmarks/) shows the read-path gate
          --   passed: fresh-reads O(N), cached-reads far flatter, gap widens with N.
          --   Also fixed a stack overflow in Rga ordering (20k-chain regression test).
          --   EDIT-PATH PERF (DONE): the append fast-path (Doc.lastAppend single
          --   slot) skips lastVisibleId's O(n) re-order for runs of appends — build
          --   dropped from ~21ms to ~7ms at N=200; benchmark read speedup now 24x at
          --   N=400. The intra-edit O(D*n) in text replace also fixed (Rga.visibleIds
          --   computes the order once). Append-order correctness tested (DocTests).
          -- Remaining Phase 2 tail (lower priority now):
          todo "general O(log n) visible-index access (random-index insert/remove still call toElementsInOrder); needs a persistent order index, not just the append slot"
        , todo "snapshot + tail: bound merge re-materialization by re-folding only ops after a cached snapshot frontier"

        -- Phase 3: delta sync — DONE
        --   `OpLog.opsAfter : Frontier -> OpStore -> List Op` returns the ops a
        --   peer at a given frontier lacks (complement of the causal ancestors) —
        --   correct regardless of delivery order / counter gaps, unlike a
        --   max-counter version vector (which the original todo wrongly proposed;
        --   our Lamport clock is gappy so a vector would skip ops). `Doc.version`
        --   gives the frontier, `Doc.encodeSince` ships only the delta. Tested
        --   (DocTests): delta brings a peer current, delta ≡ full state, empty
        --   delta for a caught-up peer. The demo now broadcasts deltas per edit
        --   with a hello/full-state handshake on connect for catch-up.
        -- Phase 4: collaborative history (the feature that drove the pivot)
        --   DONE: Doc.Version (a causal frontier) + version + readAt give
        --   collaborative time-travel — tested that a version captured before a
        --   merge excludes both the peer's concurrent ops and later local edits,
        --   while the live doc has everything (DocTests). This is the thing the
        --   local snapshot-stack Crdt.History could never do.
        -- Named checkpoints (DONE): Doc.Checkpoint (message + author + Version),
        --   Doc.checkpoint / checkpoints / checkpointMessage / -Author / -Version.
        --   Stored in the doc, emit no ops; readAt the version time-travels. Demo
        --   now uses these instead of a local checkpoint list. Tested.
        -- Remaining history work:
        , todo "explicit fork/branch type from a Version + a way to diff/compare two branches"

        -- undo/redo DONE (Loro-style LOCAL, op-log): Doc.recordEdit/undo/redo/
        -- canUndo/canRedo. Inverts THIS replica's own ops as fresh ops (so it syncs
        -- and converges), NOT a whole-state diff — a peer's concurrent edit to the
        -- same list survives my undo. delete-undo re-creates content with a fresh id
        -- (tombstones permanent); undo/redo self-record so cycles stay valid. Tested
        -- tests/UndoTests.elm (16, incl. concurrent-survives, undo-syncs-to-peer,
        -- stacks-survive-merge). Demo: undo/redo buttons; typing session (focus→blur)
        -- and a whole drag-reorder each collapse to one step. (Old state-based
        -- Crdt.History.undo/redo still exists for the legacy Crdt.Doc flavor.)
        , test "undo/redo emit inverse ops that sync (DONE — see UndoTests)" <|
            \_ -> Expect.pass

        -- Phase 5: moves + GC
        -- MoveElem: DONE via Crdt.MoveList (move-cells, max-OpId-cell wins) rather
        -- than re-pointing RGA `origin` — the first attempt, which DID re-point and
        -- created origin cycles (a→c→b→a moving the head after the tail). Cells are
        -- append-only so the cell RGA can't cycle; a value's home is its newest
        -- cell, so the latest move wins (LWW by (counter, replica)) with no separate
        -- home register. Wired through Node (Mov variant), Json, OpLog (MoveElem),
        -- OpJson, Schema.movableList, Doc.listMove. Tested: MoveListTests (12,
        -- core), MoveTests (9, public API: reorder, identity-after-edit, no-loss,
        -- concurrent-same-item LWW, move+insert, JSON round-trip, nested fields).
        -- Demo: native HTML5 drag-and-drop reorder of todos via Doc.listMove.
        , test "MoveElem action: reordering a list item preserves its identity (DONE — see MoveTests/MoveListTests)" <|
            \_ -> Expect.pass

        -- Stable cursors (DONE): Crdt.Cursor (Anchor=Start|After OpId, Cursor =
        --   id-based Target + anchor; Range = pair of cursors). Doc.cursorAt /
        --   cursorOffset / cursorRange; Rga.liveCountThrough resolves robustly
        --   across concurrent inserts AND deletion of the anchor (tombstones
        --   retained). Tested (CursorTests): round-trip, track-on-insert-before,
        --   stable-on-insert-after, survive-anchor-delete, convergence, nested
        --   stability, range, JSON. Demo: live remote title caret (ch-approx) via
        --   Presence.custom carrying a Cursor. Crdt.Cursor is public.
        -- Counter (DONE): `Cnt` node = per-op signed contributions, value = sum,
        --   merge = union (Dict.merge with larger-delta tiebreak for robustness).
        --   `S.counter` combinator, `Doc.increment` / `Edit.increment` ops,
        --   JSON via Crdt.Json + OpJson. Tested: accumulate, decrement, concurrent
        --   +1/+1 => 2 (not LWW 1), order-independent, wire round-trip, fuzz laws.
        -- GC / shallow snapshot (DONE, phases 0–3): OpLog.compact (Node->Frontier
        --   ->OpStore->(Node,OpStore)) with a fuzzed read-equivalence proof;
        --   Doc.gc folds ops <= a cut into `base`, advances `baseFrontier`,
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
