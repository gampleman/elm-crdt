port module Headless exposing (main)

{-| Headless timing harness for the Phase 2 go/no-go gate.

`elm-explorations/benchmark` needs a browser; this `Platform.worker` is driven
from Node (`run.js`) so the perf result is reproducible at the CLI. JS sends a
command `{ mode, n }`, Elm does the work and replies `done`, and JS brackets each
command with `performance.now()`. Port round-trip overhead is constant, so the
**scaling across N** is the signal.

Modes (each builds an N-op document, then does 100 reads):

  - `"cached"` — 100 `OpDoc.read`s off the maintained cache (the hot path);
  - `"fresh"` — 100 `OpDoc.freshState`s (a full re-fold per read; what every
    read would cost without the cache).

`cached` should stay ~flat in N; `fresh` should grow with N. That divergence is
the gate.

-}

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema as S exposing (Crdt)


type alias Doc =
    { items : List Item }


type alias Item =
    { label : String }


schema : Crdt Doc
schema =
    S.record Doc |> S.field "items" .items (S.list itemSchema) |> S.build


itemSchema : Crdt Item
itemSchema =
    S.record Item |> S.field "label" .label S.text |> S.build


itemsPath : Path
itemsPath =
    Path.root |> Path.field "items"


build : Int -> OpDoc Doc
build n =
    List.range 1 n
        |> List.foldl
            (\_ doc -> OpDoc.listAppend itemsPath (itemSchema |> S.with (Item "x")) doc |> Result.withDefault doc)
            (OpDoc.init (Id.replica "bench") schema)


{-| Do `reps` reads of an N-op doc in the given mode, returning a checksum so the
work can't be optimized away.

The doc is built **once** here, but JS times the whole call. The `"build"` mode
isolates construction cost; `"cached"` and `"fresh"` both include the same build,
so comparing them cancels the build term and isolates the read path — the thing
the materialization cache targets.

-}
run : String -> Int -> Int
run mode n =
    let
        doc =
            build n

        reps =
            100
    in
    case mode of
        "build" ->
            -- just force the built doc once (isolates construction cost)
            forceNode (OpDoc.cachedState doc)

        "cached" ->
            List.range 1 reps
                |> List.foldl
                    (\_ acc ->
                        case OpDoc.read doc of
                            Ok d ->
                                acc + List.length d.items

                            Err _ ->
                                acc
                    )
                    0

        _ ->
            -- "fresh": force a full re-materialize each read
            List.range 1 reps
                |> List.foldl (\_ acc -> acc + forceNode (OpDoc.freshState doc)) 0


forceNode : a -> Int
forceNode node =
    -- Basics.identity-style force: compare to itself to ensure evaluation.
    if node == node then
        1

    else
        0


type alias Command =
    { mode : String, n : Int }


main : Program () () Command
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \cmd model ->
                ( model, done (run cmd.mode cmd.n) )
        , subscriptions = \_ -> command identity
        }


{-| JS -> Elm: run this (mode, n).
-}
port command : (Command -> msg) -> Sub msg


{-| Elm -> JS: a checksum signalling the work is done (so JS can stop the clock).
-}
port done : Int -> Cmd msg
