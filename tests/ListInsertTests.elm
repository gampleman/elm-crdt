module ListInsertTests exposing (suite)

{-| Arbitrary-position list insert (`Doc.listInsert` / `Crdt.insert`), on both a plain
`list` (`Seq`) and a `movableList` (`Mov`). Covers: prepend / middle / end placement,
`end == append`, that it works without a movable list (the whole point — "newest first"
shouldn't need reordering), and that concurrent inserts at the same spot converge
deterministically and don't interleave (the Fugue guarantee).
-}

import Crdt.Doc.Internal as Doc exposing (Doc)
import Crdt.Id as Id
import Crdt.Path as Path exposing (Path)
import Crdt.Schema.Internal as S exposing (Crdt)
import Expect
import Test exposing (Test, describe, test)


type alias Sample =
    { fixed : List Item
    , movable : List Item
    }


type alias Item =
    { label : String }


schema : Crdt S.Nested Sample
schema =
    S.record Sample
        |> S.field "fixed" .fixed (S.list itemSchema)
        |> S.field "movable" .movable (S.movableList itemSchema)
        |> S.build


itemSchema : Crdt S.Nested Item
itemSchema =
    S.record Item |> S.field "label" .label S.text |> S.build


fixedPath : Path
fixedPath =
    Path.root |> Path.field "fixed"


movablePath : Path
movablePath =
    Path.root |> Path.field "movable"


initDoc : String -> Doc Sample
initDoc name =
    Doc.init (Id.replica name) schema


ok : Doc Sample -> Result Doc.Error (Doc Sample) -> Doc Sample
ok fb =
    Result.withDefault fb


item : String -> S.Seed
item label =
    itemSchema |> S.with (Item label)


insert : Path -> Int -> String -> Doc Sample -> Doc Sample
insert path i label doc =
    Doc.listInsert path i (item label) doc |> ok doc


append : Path -> String -> Doc Sample -> Doc Sample
append path label doc =
    Doc.listAppend path (item label) doc |> ok doc


labels : Path -> Doc Sample -> List String
labels path doc =
    Doc.read doc
        |> Result.map
            (\s ->
                if path == fixedPath then
                    List.map .label s.fixed

                else
                    List.map .label s.movable
            )
        |> Result.withDefault [ "<err>" ]


mergeIn : Doc Sample -> Doc Sample -> Doc Sample
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to


{-| Run the placement tests against whichever list path is given, so plain `list` and
`movableList` are proven identical for insert.
-}
placementTests : String -> Path -> Test
placementTests name path =
    describe ("insert placement — " ++ name)
        [ test "insert at 0 prepends" <|
            \_ ->
                let
                    doc =
                        initDoc "a" |> append path "b" |> append path "c" |> insert path 0 "a"
                in
                labels path doc |> Expect.equal [ "a", "b", "c" ]
        , test "insert in the middle lands between neighbors" <|
            \_ ->
                let
                    doc =
                        initDoc "a" |> append path "a" |> append path "c" |> insert path 1 "b"
                in
                labels path doc |> Expect.equal [ "a", "b", "c" ]
        , test "insert at length is the same as append" <|
            \_ ->
                let
                    viaInsert =
                        initDoc "a" |> append path "a" |> append path "b" |> insert path 2 "c"

                    viaAppend =
                        initDoc "a" |> append path "a" |> append path "b" |> append path "c"
                in
                Expect.equal (labels path viaInsert) (labels path viaAppend)
        , test "insert past the end clamps to append" <|
            \_ ->
                let
                    doc =
                        initDoc "a" |> append path "a" |> insert path 99 "b"
                in
                labels path doc |> Expect.equal [ "a", "b" ]
        , test "insert into an empty list" <|
            \_ ->
                labels path (insert path 0 "only" (initDoc "a"))
                    |> Expect.equal [ "only" ]
        , test "repeated prepend builds newest-first" <|
            \_ ->
                let
                    doc =
                        initDoc "a"
                            |> insert path 0 "1"
                            |> insert path 0 "2"
                            |> insert path 0 "3"
                in
                labels path doc |> Expect.equal [ "3", "2", "1" ]
        ]


suite : Test
suite =
    describe "list insert at index"
        [ placementTests "plain list" fixedPath
        , placementTests "movable list" movablePath
        , describe "concurrent inserts converge (Fugue, no interleaving)"
            [ test "two peers prepend concurrently → both present, deterministic order, converged" <|
                \_ ->
                    let
                        base =
                            initDoc "seed" |> append fixedPath "x"

                        alice =
                            mergeIn base (initDoc "alice") |> insert fixedPath 0 "A"

                        bob =
                            mergeIn base (initDoc "bob") |> insert fixedPath 0 "B"

                        ab =
                            mergeIn bob alice

                        ba =
                            mergeIn alice bob
                    in
                    Expect.all
                        [ -- converged
                          \_ -> Expect.equal (labels fixedPath ab) (labels fixedPath ba)
                        , -- both inserts survived, "x" still last
                          \_ -> Expect.equal True (List.member "A" (labels fixedPath ab))
                        , \_ -> Expect.equal True (List.member "B" (labels fixedPath ab))
                        , \_ -> Expect.equal (Just "x") (List.head (List.reverse (labels fixedPath ab)))
                        ]
                        ()
            , test "concurrent multi-item inserts at the same gap stay contiguous (no interleave)" <|
                \_ ->
                    let
                        base =
                            initDoc "seed" |> append fixedPath "L" |> append fixedPath "R"

                        -- alice inserts a1,a2 between L and R; bob inserts b1,b2 there too
                        alice =
                            mergeIn base (initDoc "alice")
                                |> insert fixedPath 1 "a1"
                                |> insert fixedPath 2 "a2"

                        bob =
                            mergeIn base (initDoc "bob")
                                |> insert fixedPath 1 "b1"
                                |> insert fixedPath 2 "b2"

                        merged =
                            mergeIn bob alice

                        result =
                            labels fixedPath merged

                        -- the a-run and b-run must each be contiguous: aa before bb, or bb before aa
                        aRun =
                            [ "L", "a1", "a2", "b1", "b2", "R" ]

                        bRun =
                            [ "L", "b1", "b2", "a1", "a2", "R" ]
                    in
                    Expect.equal True (result == aRun || result == bRun)
            ]
        ]
