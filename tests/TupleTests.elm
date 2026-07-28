module TupleTests exposing (suite)

{-| `Crdt.tuple` — a pair `( a, b )` where each half keeps its own merge semantics (unlike a
whole-value `register`). Backed by a two-field record, so the two components merge
independently. Checks: reads as a pair, each half edits through its own ref with the right
semantics (text merges char-wise, counter sums), and concurrent edits to the two halves
converge without clobbering each other.
-}

import Crdt exposing (Ref)
import Crdt.Doc as Doc
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Test exposing (Test, describe, test)


type alias Model =
    { pair : ( String, Int ) }


pairRefs :
    { first : Ref ( String, Int ) Crdt.Settable String
    , second : Ref ( String, Int ) Crdt.Counter Int
    , schema : Crdt.Schema Crdt.Nested ( String, Int )
    }
pairRefs =
    Crdt.tuple Crdt.text Crdt.counter


doc : { pair : Ref Model Crdt.Nested ( String, Int ), schema : Crdt.Schema Crdt.Nested Model }
doc =
    Crdt.record Model (\p s -> { pair = p, schema = s })
        |> Crdt.field "pair" .pair pairRefs
        |> Crdt.build


init : String -> Doc.Doc Model
init name =
    Crdt.init (Id.replica name) doc.schema


ok : Doc.Doc Model -> Result Edit.EditError (Doc.Doc Model) -> Doc.Doc Model
ok fb =
    Result.withDefault fb


read : Doc.Doc Model -> ( String, Int )
read d =
    Doc.read d |> Result.map .pair |> Result.withDefault ( "<err>", -1 )


mergeIn : Doc.Doc Model -> Doc.Doc Model -> Doc.Doc Model
mergeIn from to =
    Doc.decodeInto (Doc.encode from) to |> Result.withDefault to



-- refs into each half: doc.refs.pair |> at pairRefs.first / .second


firstRef : Ref Model Crdt.Settable String
firstRef =
    doc.pair |> Crdt.at pairRefs.first


secondRef : Ref Model Crdt.Counter Int
secondRef =
    doc.pair |> Crdt.at pairRefs.second


suite : Test
suite =
    describe "Crdt.tuple (mergeable pair)"
        [ test "reads as a pair; each half edits through its own ref" <|
            \_ ->
                let
                    d =
                        init "a"
                            |> (\x -> Edit.set firstRef "likes" x |> ok x)
                            |> (\x -> Edit.increment secondRef 5 x |> ok x)
                in
                read d |> Expect.equal ( "likes", 5 )
        , test "the counter half sums concurrent increments (keeps counter semantics)" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\x -> Edit.set firstRef "score" x |> ok x)

                    alice =
                        mergeIn base (init "alice") |> (\x -> Edit.increment secondRef 1 x |> ok x)

                    bob =
                        mergeIn base (init "bob") |> (\x -> Edit.increment secondRef 1 x |> ok x)

                    merged =
                        mergeIn bob alice
                in
                -- both +1s survive → 2, not last-write-wins 1
                read merged |> Expect.equal ( "score", 2 )
        , test "the text half merges character-wise" <|
            \_ ->
                let
                    base =
                        init "seed"

                    alice =
                        mergeIn base (init "alice") |> (\x -> Edit.set firstRef "hello" x |> ok x)

                    bob =
                        mergeIn base (init "bob") |> (\x -> Edit.set firstRef "world" x |> ok x)

                    ab =
                        mergeIn bob alice

                    ba =
                        mergeIn alice bob
                in
                -- converges (Fugue), and each run stays contiguous
                Expect.all
                    [ \_ -> Expect.equal (Tuple.first (read ab)) (Tuple.first (read ba))
                    , \_ -> Expect.equal True (List.member (Tuple.first (read ab)) [ "helloworld", "worldhello" ])
                    ]
                    ()
        , test "concurrent edits to DIFFERENT halves both land (no conflict between halves)" <|
            \_ ->
                let
                    base =
                        init "seed" |> (\x -> Edit.set firstRef "start" x |> ok x)

                    -- alice edits the text half, bob the counter half, concurrently
                    alice =
                        mergeIn base (init "alice") |> (\x -> Edit.set firstRef "renamed" x |> ok x)

                    bob =
                        mergeIn base (init "bob") |> (\x -> Edit.increment secondRef 3 x |> ok x)

                    merged =
                        mergeIn bob alice
                in
                read merged |> Expect.equal ( "renamed", 3 )
        ]
