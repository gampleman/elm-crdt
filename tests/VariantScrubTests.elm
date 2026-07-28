module VariantScrubTests exposing (suite)

{-| Regression: scrubbing history through a `switch` to a variant WITH a payload
must never expose an intermediate state where `$tag` names the new variant but its
payload isn't materialized yet. A `switch` emits several ops (the `$tag` register
flip, the new payload subtree, the old payload tombstone); `versionAt` stops at a
causal-order prefix, so a boundary can land between the tag flip and the payload
creation. Before the fix this read as `MissingField "variant argument 0"`.
-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Expect
import Test exposing (Test, describe, test)


type Status
    = Planning
    | Active
    | Archived String


statusSchema : { archived : Ref Status C.Settable String, schema : C.Schema (C.Variants Status) Status }
statusSchema =
    C.custom
        (\planning active archived v ->
            case v of
                Planning ->
                    planning

                Active ->
                    active

                Archived s ->
                    archived s
        )
        (\archived schema -> { archived = archived, schema = schema })
        |> C.variant0 "planning" Planning
        |> C.variant0 "active" Active
        |> C.variant1 "archived" Archived C.text
        |> C.buildCustom


type alias Board =
    { status : Status }


board : { status : Ref Board (C.Variants Status) Status, schema : C.Schema C.Nested Board }
board =
    C.record Board (\s schema -> { status = s, schema = schema })
        |> C.field "status" .status statusSchema
        |> C.build


init : Doc Board
init =
    C.init (Id.replica "alice") board.schema


ok : Doc Board -> Result Edit.EditError (Doc Board) -> Doc Board
ok fb =
    Result.withDefault fb


suite : Test
suite =
    describe "scrubbing history through a variant switch"
        [ test "every scrub step reads without error after switching to Archived" <|
            \_ ->
                let
                    doc =
                        init
                            |> (\d -> Edit.switch board.status Active d |> ok d)
                            |> (\d -> Edit.switch board.status (Archived "done with it") d |> ok d)

                    n =
                        Doc.historyLength doc

                    errsAtEachStep =
                        List.range 0 n
                            |> List.filterMap
                                (\i ->
                                    case Doc.readAt (Doc.versionAt i doc) doc of
                                        Ok _ ->
                                            Nothing

                                        Err e ->
                                            Just ( i, Doc.readErrorToString e )
                                )
                in
                errsAtEachStep |> Expect.equal []
        ]
