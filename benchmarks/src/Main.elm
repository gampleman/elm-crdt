module Main exposing (main)

{-| Phase 2 go/no-go benchmark (see `docs/02-oplog.md`).

Compares the **incrementally-cached** read path (`OpDoc.read`, which reads the
maintained `cachedState`) against a **full re-materialization** of the same
op store (`OpDoc.freshState`, which folds every op from the base).

The point: building/reading a document of N ops must be O(1) per read with the
cache, vs O(N) per read without — i.e. O(N) vs O(N²) over a session. At a few
hundred ops the cached path should be dramatically faster, and the gap should
widen with N. If it doesn't, the op-log API is not viable for interactive use and
this is the gate that catches it.

Run with `elm-benchmark` / `elm-explorations/benchmark`'s browser runner:

    elm make src/Main.elm --output benchmark.js   # then open in a browser

-}

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Path as Path exposing (Path)
import Crdt.Schema as S exposing (Crdt)
import Dict


type alias Doc =
    { title : String
    , items : List Item
    }


type alias Item =
    { label : String }


schema : Crdt Doc
schema =
    S.record Doc
        |> S.field "title" .title S.text
        |> S.field "items" .items (S.list itemSchema)
        |> S.build


itemSchema : Crdt Item
itemSchema =
    S.record Item
        |> S.field "label" .label S.text
        |> S.build


todosPath : Path
todosPath =
    Path.root |> Path.field "items"


{-| Build a document by appending `n` list items (each append emits one op).
-}
build : Int -> OpDoc Doc
build n =
    List.range 1 n
        |> List.foldl
            (\_ doc ->
                OpDoc.listAppend todosPath (itemSchema |> S.with (Item "x")) doc
                    |> Result.withDefault doc
            )
            (OpDoc.init (Id.replica "bench") schema)


{-| At each size, compare reading via the maintained cache vs a full re-fold.
Both must return the same value (the cache invariant); only their cost differs.
-}
suite : Benchmark
suite =
    Benchmark.describe "op-log read: cached vs full re-materialize"
        ([ 50, 200, 800 ]
            |> List.map
                (\n ->
                    let
                        doc =
                            build n
                    in
                    Benchmark.compare ("N=" ++ String.fromInt n)
                        "cached (OpDoc.read / cachedState)"
                        (\_ -> OpDoc.cachedState doc)
                        "full re-materialize (freshState)"
                        (\_ -> OpDoc.freshState doc)
                )
        )


main : BenchmarkProgram
main =
    program suite
