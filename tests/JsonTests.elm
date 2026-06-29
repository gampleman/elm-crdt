module JsonTests exposing (suite)

{-| JSON transport must be **lossless**: encoding a doc and decoding it back
yields a doc that reads identically and merges identically. This is what the
demo relies on to ship state over the WebSocket.
-}

import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Path as Path
import Crdt.Schema as S
import Expect
import Helpers exposing (Todo, boardSchema, todoSchema)
import Test exposing (Test, describe, test)


alice : Id.ReplicaId
alice =
    Id.replica "alice"


sampleDoc : Doc
sampleDoc =
    let
        base =
            Crdt.init alice boardSchema

        step f doc =
            f doc |> Result.withDefault doc
    in
    base
        |> step (Edit.setText (Path.root |> Path.field "title") "Trip")
        |> step (Edit.listAppend (Path.root |> Path.field "todos") (todoSchema |> S.with (Todo "pack" False)))
        |> step (Edit.setKey (Path.root |> Path.field "notes") "n" (S.text |> S.with "remember"))


suite : Test
suite =
    describe "JSON transport"
        [ test "decode (encode doc) reads identically" <|
            \_ ->
                case Crdt.decode alice (Crdt.encode sampleDoc) of
                    Ok decoded ->
                        Expect.equal
                            (Crdt.read boardSchema sampleDoc)
                            (Crdt.read boardSchema decoded)

                    Err e ->
                        Expect.fail ("decode failed: " ++ e)
        , test "decoded doc merges idempotently with the original (no resurrection / dup IDs)" <|
            \_ ->
                case Crdt.decode alice (Crdt.encode sampleDoc) of
                    Ok decoded ->
                        Expect.equal
                            (Crdt.read boardSchema sampleDoc)
                            (Crdt.read boardSchema (Crdt.merge sampleDoc decoded))

                    Err e ->
                        Expect.fail ("decode failed: " ++ e)
        , test "tombstones survive the JSON roundtrip" <|
            \_ ->
                let
                    notesPath =
                        Path.root |> Path.field "notes"

                    withRemoval =
                        Edit.setKey notesPath "k" (S.text |> S.with "v") sampleDoc
                            |> Result.andThen (Edit.removeKey notesPath "k")
                            |> Result.withDefault sampleDoc
                in
                case Crdt.decode alice (Crdt.encode withRemoval) of
                    Ok decoded ->
                        -- re-merging with a replica that still has "k" must NOT resurrect it
                        Expect.equal
                            (Crdt.read boardSchema withRemoval)
                            (Crdt.read boardSchema (Crdt.merge decoded withRemoval))

                    Err e ->
                        Expect.fail ("decode failed: " ++ e)
        ]
