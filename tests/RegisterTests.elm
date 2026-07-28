module RegisterTests exposing (suite)

{-| `Crdt.register` — an LWW register holding an arbitrary JSON-encoded value. Covers:
the default read on a fresh document, a `set`/`read` round-trip of a custom type, the
JSON wire round-trip, and concurrent last-write-wins convergence (whole-value, both
merge orders).
-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)



-- DOMAIN ----------------------------------------------------------------------


type Priority
    = Low
    | Medium
    | High


encodePriority : Priority -> JE.Value
encodePriority p =
    JE.string
        (case p of
            Low ->
                "low"

            Medium ->
                "medium"

            High ->
                "high"
        )


priorityDecoder : JD.Decoder Priority
priorityDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                case s of
                    "low" ->
                        JD.succeed Low

                    "medium" ->
                        JD.succeed Medium

                    "high" ->
                        JD.succeed High

                    _ ->
                        JD.fail ("bad priority: " ++ s)
            )


prioritySchema : C.Leaf C.Settable Priority
prioritySchema =
    C.register Low encodePriority priorityDecoder


type alias Task =
    { priority : Priority }


type alias TaskDoc =
    { priority : Ref Task C.Settable Priority
    , schema : C.Schema C.Nested Task
    }


task : TaskDoc
task =
    C.record Task TaskDoc
        |> C.field "priority" .priority prioritySchema
        |> C.build


init : String -> Doc Task
init name =
    C.init (Id.replica name) task.schema


ok : Doc Task -> Result Edit.EditError (Doc Task) -> Doc Task
ok fallback =
    Result.withDefault fallback


read : Doc Task -> Result Doc.ReadError Task
read =
    Doc.read



-- SUITE -----------------------------------------------------------------------


suite : Test
suite =
    describe "Crdt.register"
        [ test "a fresh document reads the default" <|
            \_ ->
                read (init "a") |> Expect.equal (Ok { priority = Low })
        , test "set then read round-trips a custom type" <|
            \_ ->
                let
                    doc =
                        init "a"
                in
                Edit.set task.priority High doc
                    |> ok doc
                    |> read
                    |> Expect.equal (Ok { priority = High })
        , test "the value survives the JSON wire round-trip" <|
            \_ ->
                let
                    doc =
                        init "a"

                    edited =
                        Edit.set task.priority Medium doc |> ok doc
                in
                Doc.encode edited
                    |> (\v -> Doc.decodeInto v (init "reader"))
                    |> Result.mapError (\_ -> "decode failed")
                    |> Result.andThen (read >> Result.mapError Doc.readErrorToString)
                    |> Expect.equal (Ok { priority = Medium })
        , test "concurrent edits converge whole-value (LWW), same both merge orders" <|
            \_ ->
                let
                    base =
                        init "a"

                    alice =
                        Edit.set task.priority High base |> ok base

                    bob =
                        Edit.set task.priority Medium (init "b") |> ok (init "b")

                    ab =
                        read (Doc.merge alice bob)

                    ba =
                        read (Doc.merge bob alice)
                in
                Expect.all
                    [ \_ -> Expect.equal ab ba
                    , \_ -> Expect.notEqual ab (Ok { priority = Low })
                    ]
                    ()
        ]
