module ClockTests exposing (suite)

{-| Clock safety: the Lamport clock must advance on merge, not only on local
edits. Otherwise an edit→merge→edit cycle can mint colliding OpIds and silently
corrupt convergence. We assert that all element IDs minted across a realistic
session are unique.
-}

import Crdt exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Path as Path
import Crdt.Schema as S
import Expect
import Helpers exposing (Todo, boardSchema, todoSchema)
import Test exposing (Test, describe, test)


todosPath : Path.Path
todosPath =
    Path.root |> Path.field "todos"


append : String -> Doc -> Doc
append label doc =
    Edit.listAppend todosPath (todoSchema |> S.with (Todo label False)) doc
        |> Result.withDefault doc


suite : Test
suite =
    describe "clock safety"
        [ test "edit -> merge -> edit does not mint duplicate OpIds" <|
            \_ ->
                let
                    a0 =
                        Crdt.init (Id.replica "alice") boardSchema

                    b0 =
                        Crdt.init (Id.replica "bob") boardSchema

                    -- both edit, then alice merges bob's state, then edits again
                    a1 =
                        append "a1" a0

                    b1 =
                        append "b1" b0

                    a2 =
                        Crdt.merge a1 b1 |> append "a2"
                in
                -- Crdt.opIds exposes every minted id in the doc for this assertion
                Expect.equal
                    (List.length (Crdt.opIds a2))
                    (List.length (dedupe (Crdt.opIds a2)))
        , test "clock observed from merge exceeds the incoming max counter" <|
            \_ ->
                let
                    fast =
                        List.foldl append (Crdt.init (Id.replica "fast") boardSchema) [ "1", "2", "3", "4", "5" ]

                    slow =
                        Crdt.init (Id.replica "slow") boardSchema

                    -- after merging fast's high-counter state, slow's next edit
                    -- must use a counter greater than anything it just saw
                    merged =
                        Crdt.merge slow fast |> append "after"
                in
                Expect.greaterThan
                    (Crdt.maxCounter fast)
                    (Crdt.maxCounter merged)
        ]


dedupe : List a -> List a
dedupe xs =
    List.foldr
        (\x acc ->
            if List.member x acc then
                acc

            else
                x :: acc
        )
        []
        xs
