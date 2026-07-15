module MigrationTests exposing (suite)

{-| Schema evolution (docs/13): a document written under one schema version reads
sensibly under another, and peers on different versions converge. The mechanism is
read-time tolerance plus **seed-the-default** — never data rewriting:

  - `withDefault d` seeds `d` (not an arbitrary empty) into the base, so an older
    document lacking the field reads `d`;
  - `optional` represents `Maybe` as a uniform map, reading `Nothing` for old data;
  - `map` transforms a value's shape both ways;
  - `aliasedField` (rename) resolves by highest stamp across old/new names, so a real
    write to either name beats a base seed;
  - `catchAll` reads an unknown sum `$tag` as a fallback variant instead of erroring.

Every case is exercised through the REAL join path (a peer `init`s its own schema's base,
then `decodeInto`s the other peer's document), because that is where a per-replica base
would otherwise mask absence. The convergence test is the property that matters.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Id as Id
import Expect
import Test exposing (Test, describe, test)



-- PRIORITY (a sum-ish enum stored as a string via `map`) ---------------------


type Priority
    = Low
    | Medium
    | High


priorityFromString : String -> Priority
priorityFromString s =
    case s of
        "low" ->
            Low

        "high" ->
            High

        _ ->
            Medium


priorityToString : Priority -> String
priorityToString p =
    case p of
        Low ->
            "low"

        Medium ->
            "medium"

        High ->
            "high"


prioritySchema : C.Schema C.Settable Priority
prioritySchema =
    C.map priorityFromString priorityToString C.string



-- V1 / V2 SCHEMAS ------------------------------------------------------------
-- V1: { text, complete }.  V2 evolves it:
--   • "complete" renamed to "done"     (aliasedField "done" ["complete"], defaulted)
--   • added "priority" (withDefault Medium)
--   • added "tags"     (optional text)


type alias TodoV1 =
    { text : String, done : Bool }


type alias TodoV1Refs =
    { text : Ref TodoV1 C.Settable String, done : Ref TodoV1 C.Settable Bool }


todoV1 : C.RecordRefs TodoV1 TodoV1Refs
todoV1 =
    C.record TodoV1 TodoV1Refs
        |> C.field "text" .text C.text
        -- v1's own name for the flag is "complete"
        |> C.aliasedField "complete" [] .done (C.withDefault False C.bool)
        |> C.build


type alias TodoV2 =
    { text : String
    , done : Bool
    , priority : Priority
    , tags : Maybe String
    }


type alias TodoV2Refs =
    { text : Ref TodoV2 C.Settable String
    , done : Ref TodoV2 C.Settable Bool
    , priority : Ref TodoV2 C.Settable Priority
    , tags : Ref TodoV2 C.Settable (Maybe String)
    }


todoV2 : C.RecordRefs TodoV2 TodoV2Refs
todoV2 =
    C.record TodoV2 TodoV2Refs
        |> C.field "text" .text C.text
        |> C.aliasedField "done" [ "complete" ] .done (C.withDefault False C.bool)
        |> C.field "priority" .priority (C.withDefault Medium prioritySchema)
        |> C.field "tags" .tags (C.optional C.text)
        |> C.build



-- HELPERS --------------------------------------------------------------------


initV1 : String -> Doc TodoV1
initV1 name =
    C.init (Id.replica name) todoV1.schema


initV2 : String -> Doc TodoV2
initV2 name =
    C.init (Id.replica name) todoV2.schema


ok : Doc a -> Result C.EditError (Doc a) -> Doc a
ok fb =
    Result.withDefault fb


{-| The realistic join: a peer `init`s its own schema's base, then merges the other
peer's whole document over the wire. Returns the joined doc read through the joiner's
schema.
-}
joinReadV2 : Doc a -> Result String TodoV2
joinReadV2 from =
    Doc.decodeInto (Doc.encode from) (initV2 "j")
        |> Result.mapError (\_ -> "decode")
        |> Result.andThen (\d -> C.read d |> Result.mapError C.readErrorToString)


joinReadV1 : Doc a -> Result String TodoV1
joinReadV1 from =
    Doc.decodeInto (Doc.encode from) (initV1 "j")
        |> Result.mapError (\_ -> "decode")
        |> Result.andThen (\d -> C.read d |> Result.mapError C.readErrorToString)


suite : Test
suite =
    describe "schema evolution / migration (docs/13)"
        [ test "a v1 doc joined by a v2 peer: defaults fill in, rename resolves" <|
            \_ ->
                let
                    v1doc =
                        initV1 "a"
                            |> (\d -> C.set todoV1.refs.text "buy milk" d |> ok d)
                            |> (\d -> C.set todoV1.refs.done True d |> ok d)
                in
                joinReadV2 v1doc
                    |> Expect.equal
                        (Ok
                            { text = "buy milk"
                            , done = True -- read via the "complete" alias, highest stamp
                            , priority = Medium -- withDefault: field absent in v1
                            , tags = Nothing -- optional: field absent in v1
                            }
                        )
        , test "a v2 doc joined by a v1 peer: v1 reads text + its defaulted flag, ignores extras" <|
            \_ ->
                -- v2 writes "done" (new name) + priority/tags. v1 only knows "text" and
                -- "complete". v1 has no alias for "done", so its flag reads the DEFAULT
                -- (False) — a graceful default, not an error (withDefault makes v1
                -- tolerant of its own missing field). priority/tags are ignored.
                let
                    v2doc =
                        initV2 "b"
                            |> (\d -> C.set todoV2.refs.text "ship it" d |> ok d)
                            |> (\d -> C.set todoV2.refs.done True d |> ok d)
                            |> (\d -> C.set todoV2.refs.priority High d |> ok d)
                in
                joinReadV1 v2doc
                    |> Expect.equal (Ok { text = "ship it", done = False })
        , test "withDefault: old data reads the default; a written value wins" <|
            \_ ->
                let
                    old =
                        initV1 "a" |> joinReadV2 |> Result.map .priority

                    new =
                        initV2 "b"
                            |> (\d -> C.set todoV2.refs.priority High d |> ok d)
                            |> C.read
                            |> Result.mapError C.readErrorToString
                            |> Result.map .priority
                in
                Expect.all
                    [ \_ -> Expect.equal (Ok Medium) old
                    , \_ -> Expect.equal (Ok High) new
                    ]
                    ()
        , test "optional: old data reads Nothing; a written Just wins" <|
            \_ ->
                let
                    old =
                        initV1 "a" |> joinReadV2 |> Result.map .tags

                    new =
                        initV2 "b"
                            |> (\d -> C.set todoV2.refs.tags (Just "urgent") d |> ok d)
                            |> C.read
                            |> Result.mapError C.readErrorToString
                            |> Result.map .tags
                in
                Expect.all
                    [ \_ -> Expect.equal (Ok Nothing) old
                    , \_ -> Expect.equal (Ok (Just "urgent")) new
                    ]
                    ()
        , test "map round-trips a value's shape (Priority <-> string)" <|
            \_ ->
                let
                    doc =
                        initV2 "b"
                            |> (\d -> C.set todoV2.refs.priority Low d |> ok d)
                            |> (\d -> C.set todoV2.refs.priority High d |> ok d)
                in
                C.read doc |> Result.map .priority |> Expect.equal (Ok High)
        , test "rename: the flag written under EITHER name resolves in v2 (highest stamp)" <|
            \_ ->
                let
                    fromOldName =
                        initV1 "a"
                            |> (\d -> C.set todoV1.refs.done True d |> ok d)
                            |> joinReadV2
                            |> Result.map .done

                    fromNewName =
                        initV2 "b"
                            |> (\d -> C.set todoV2.refs.done True d |> ok d)
                            |> C.read
                            |> Result.mapError C.readErrorToString
                            |> Result.map .done
                in
                Expect.all
                    [ \_ -> Expect.equal (Ok True) fromOldName
                    , \_ -> Expect.equal (Ok True) fromNewName
                    ]
                    ()
        , test "convergence: v1 and v2 peers edit concurrently and each reads consistently" <|
            \_ ->
                -- THE property that matters. Shared base, concurrent edits, exchange ops.
                let
                    base =
                        initV1 "seed" |> (\d -> C.set todoV1.refs.text "start" d |> ok d)

                    v2peer =
                        Doc.decodeInto (Doc.encode base) (initV2 "v2")
                            |> Result.withDefault (initV2 "v2")
                            |> (\d -> C.set todoV2.refs.priority High d |> ok d)

                    v1peer =
                        base |> (\d -> C.set todoV1.refs.text "start!" d |> ok d)

                    v2final =
                        Doc.decodeInto (Doc.encode v1peer) v2peer |> Result.withDefault v2peer

                    v1final =
                        Doc.decodeInto (Doc.encode v2peer) v1peer |> Result.withDefault v1peer
                in
                Expect.all
                    [ \_ -> Expect.equal (C.read v1final |> Result.map .text) (Ok "start!")
                    , \_ -> Expect.equal (C.read v2final |> Result.map .text) (Ok "start!")
                    , \_ -> Expect.equal (C.read v2final |> Result.map .priority) (Ok High)
                    , \_ -> Expect.ok (C.read v2final)
                    ]
                    ()
        , test "catchAll: an older peer reads a newer variant's tag as the fallback, not an error" <|
            \_ ->
                -- StatusNew adds "archived"; StatusOld has only active/done + a catchAll.
                let
                    newDoc =
                        C.init (Id.replica "new") statusNewDoc.schema
                            |> (\d -> C.switch statusNewDoc.refs.status Archived d |> ok d)

                    oldRead =
                        Doc.decodeInto (Doc.encode newDoc) (C.init (Id.replica "old") statusOldDoc.schema)
                            |> Result.mapError (\_ -> "decode")
                            |> Result.andThen (\d -> C.read d |> Result.mapError C.readErrorToString)
                            |> Result.map .status
                in
                oldRead |> Expect.equal (Ok (UnknownStatus "archived"))
        , test "withDefault collapses a newer peer's unknown variant to the default, not an error" <|
            \_ ->
                -- statusPlainDoc has NO catchAll; its `status` field wraps the custom
                -- schema in `withDefault OActive`, so an unknown tag reads as OActive.
                let
                    newDoc =
                        C.init (Id.replica "new") statusNewDoc.schema
                            |> (\d -> C.switch statusNewDoc.refs.status Archived d |> ok d)

                    oldRead =
                        Doc.decodeInto (Doc.encode newDoc) (C.init (Id.replica "old") statusPlainDoc.schema)
                            |> Result.mapError (\_ -> "decode")
                            |> Result.andThen (\d -> C.read d |> Result.mapError C.readErrorToString)
                            |> Result.map .status
                in
                oldRead |> Expect.equal (Ok OPActive)
        ]



-- CATCH-ALL FIXTURE ----------------------------------------------------------


type StatusNew
    = NActive
    | NDone
    | Archived


type alias StatusNewRefs =
    {}


statusNew : C.CustomRefs StatusNew StatusNewRefs
statusNew =
    C.custom
        (\active done archived v ->
            case v of
                NActive ->
                    active

                NDone ->
                    done

                Archived ->
                    archived
        )
        StatusNewRefs
        |> C.variant0 "active" NActive
        |> C.variant0 "done" NDone
        |> C.variant0 "archived" Archived
        |> C.buildCustom


type StatusOld
    = OActive
    | ODone
    | UnknownStatus String


type alias StatusOldRefs =
    {}


statusOld : C.CustomRefs StatusOld StatusOldRefs
statusOld =
    C.custom
        (\active done unknown v ->
            case v of
                OActive ->
                    active

                ODone ->
                    done

                UnknownStatus tag ->
                    unknown tag
        )
        StatusOldRefs
        |> C.variant0 "active" OActive
        |> C.variant0 "done" ODone
        |> C.catchAll UnknownStatus
        |> C.buildCustom


type alias StatusDocNew =
    { status : StatusNew }


statusNewDoc : C.RecordRefs StatusDocNew { status : Ref StatusDocNew (C.Variants StatusNew) StatusNew }
statusNewDoc =
    C.record StatusDocNew (\s -> { status = s })
        |> C.field "status" .status statusNew.schema
        |> C.build


type alias StatusDocOld =
    { status : StatusOld }


statusOldDoc : C.RecordRefs StatusDocOld { status : Ref StatusDocOld (C.Variants StatusOld) StatusOld }
statusOldDoc =
    C.record StatusDocOld (\s -> { status = s })
        |> C.field "status" .status statusOld.schema
        |> C.build



-- PLAIN-OLD FIXTURE (no catchAll; relies on withDefault to tolerate unknown tags) --------


type StatusPlain
    = OPActive
    | OPDone


statusPlain : C.CustomRefs StatusPlain {}
statusPlain =
    C.custom
        (\active done v ->
            case v of
                OPActive ->
                    active

                OPDone ->
                    done
        )
        {}
        |> C.variant0 "active" OPActive
        |> C.variant0 "done" OPDone
        |> C.buildCustom


type alias StatusDocPlain =
    { status : StatusPlain }


statusPlainDoc : C.RecordRefs StatusDocPlain { status : Ref StatusDocPlain (C.Variants StatusPlain) StatusPlain }
statusPlainDoc =
    C.record StatusDocPlain (\s -> { status = s })
        |> C.field "status" .status (C.withDefault OPActive statusPlain.schema)
        |> C.build
