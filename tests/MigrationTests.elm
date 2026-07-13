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

import Crdt.Id as Id
import Crdt.OpDoc as OpDoc exposing (OpDoc)
import Crdt.Ref as Ref exposing (Ref)
import Crdt.Schema as S exposing (Crdt)
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


prioritySchema : Crdt S.Settable Priority
prioritySchema =
    S.map priorityFromString priorityToString S.string



-- V1 / V2 SCHEMAS ------------------------------------------------------------
-- V1: { text, complete }.  V2 evolves it:
--   • "complete" renamed to "done"     (aliasedField "done" ["complete"], defaulted)
--   • added "priority" (withDefault Medium)
--   • added "tags"     (optional text)


type alias TodoV1 =
    { text : String, done : Bool }


type alias TodoV1Refs =
    { text : Ref TodoV1 S.Settable String, done : Ref TodoV1 S.Settable Bool }


todoV1 : Ref.RecordRefs TodoV1 TodoV1Refs
todoV1 =
    Ref.record TodoV1 TodoV1Refs
        |> Ref.field "text" .text S.text
        -- v1's own name for the flag is "complete"
        |> Ref.aliasedField "complete" [] .done (S.withDefault False S.bool)
        |> Ref.build


type alias TodoV2 =
    { text : String
    , done : Bool
    , priority : Priority
    , tags : Maybe String
    }


type alias TodoV2Refs =
    { text : Ref TodoV2 S.Settable String
    , done : Ref TodoV2 S.Settable Bool
    , priority : Ref TodoV2 S.Settable Priority
    , tags : Ref TodoV2 S.Settable (Maybe String)
    }


todoV2 : Ref.RecordRefs TodoV2 TodoV2Refs
todoV2 =
    Ref.record TodoV2 TodoV2Refs
        |> Ref.field "text" .text S.text
        |> Ref.aliasedField "done" [ "complete" ] .done (S.withDefault False S.bool)
        |> Ref.field "priority" .priority (S.withDefault Medium prioritySchema)
        |> Ref.field "tags" .tags (S.optional S.text)
        |> Ref.build



-- HELPERS --------------------------------------------------------------------


initV1 : String -> OpDoc TodoV1
initV1 name =
    OpDoc.init (Id.replica name) todoV1.schema


initV2 : String -> OpDoc TodoV2
initV2 name =
    OpDoc.init (Id.replica name) todoV2.schema


ok : OpDoc a -> Result OpDoc.Error (OpDoc a) -> OpDoc a
ok fb =
    Result.withDefault fb


{-| The realistic join: a peer `init`s its own schema's base, then merges the other
peer's whole document over the wire. Returns the joined doc read through the joiner's
schema.
-}
joinReadV2 : OpDoc a -> Result String TodoV2
joinReadV2 from =
    OpDoc.decodeInto (OpDoc.encode from) (initV2 "j")
        |> Result.mapError (\_ -> "decode")
        |> Result.andThen (\d -> OpDoc.read d |> Result.mapError S.errorToString)


joinReadV1 : OpDoc a -> Result String TodoV1
joinReadV1 from =
    OpDoc.decodeInto (OpDoc.encode from) (initV1 "j")
        |> Result.mapError (\_ -> "decode")
        |> Result.andThen (\d -> OpDoc.read d |> Result.mapError S.errorToString)


suite : Test
suite =
    describe "schema evolution / migration (docs/13)"
        [ test "a v1 doc joined by a v2 peer: defaults fill in, rename resolves" <|
            \_ ->
                let
                    v1doc =
                        initV1 "a"
                            |> (\d -> Ref.set todoV1.refs.text "buy milk" d |> ok d)
                            |> (\d -> Ref.set todoV1.refs.done True d |> ok d)
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
                            |> (\d -> Ref.set todoV2.refs.text "ship it" d |> ok d)
                            |> (\d -> Ref.set todoV2.refs.done True d |> ok d)
                            |> (\d -> Ref.set todoV2.refs.priority High d |> ok d)
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
                            |> (\d -> Ref.set todoV2.refs.priority High d |> ok d)
                            |> OpDoc.read
                            |> Result.mapError S.errorToString
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
                            |> (\d -> Ref.set todoV2.refs.tags (Just "urgent") d |> ok d)
                            |> OpDoc.read
                            |> Result.mapError S.errorToString
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
                            |> (\d -> Ref.set todoV2.refs.priority Low d |> ok d)
                            |> (\d -> Ref.set todoV2.refs.priority High d |> ok d)
                in
                OpDoc.read doc |> Result.map .priority |> Expect.equal (Ok High)
        , test "rename: the flag written under EITHER name resolves in v2 (highest stamp)" <|
            \_ ->
                let
                    fromOldName =
                        initV1 "a"
                            |> (\d -> Ref.set todoV1.refs.done True d |> ok d)
                            |> joinReadV2
                            |> Result.map .done

                    fromNewName =
                        initV2 "b"
                            |> (\d -> Ref.set todoV2.refs.done True d |> ok d)
                            |> OpDoc.read
                            |> Result.mapError S.errorToString
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
                        initV1 "seed" |> (\d -> Ref.set todoV1.refs.text "start" d |> ok d)

                    v2peer =
                        OpDoc.decodeInto (OpDoc.encode base) (initV2 "v2")
                            |> Result.withDefault (initV2 "v2")
                            |> (\d -> Ref.set todoV2.refs.priority High d |> ok d)

                    v1peer =
                        base |> (\d -> Ref.set todoV1.refs.text "start!" d |> ok d)

                    v2final =
                        OpDoc.decodeInto (OpDoc.encode v1peer) v2peer |> Result.withDefault v2peer

                    v1final =
                        OpDoc.decodeInto (OpDoc.encode v2peer) v1peer |> Result.withDefault v1peer
                in
                Expect.all
                    [ \_ -> Expect.equal (OpDoc.read v1final |> Result.map .text) (Ok "start!")
                    , \_ -> Expect.equal (OpDoc.read v2final |> Result.map .text) (Ok "start!")
                    , \_ -> Expect.equal (OpDoc.read v2final |> Result.map .priority) (Ok High)
                    , \_ -> Expect.ok (OpDoc.read v2final)
                    ]
                    ()
        , test "catchAll: an older peer reads a newer variant's tag as the fallback, not an error" <|
            \_ ->
                -- StatusNew adds "archived"; StatusOld has only active/done + a catchAll.
                let
                    newDoc =
                        OpDoc.init (Id.replica "new") statusNewDoc.schema
                            |> (\d -> Ref.switch statusNewDoc.refs.status Archived d |> ok d)

                    oldRead =
                        OpDoc.decodeInto (OpDoc.encode newDoc) (OpDoc.init (Id.replica "old") statusOldDoc.schema)
                            |> Result.mapError (\_ -> "decode")
                            |> Result.andThen (\d -> OpDoc.read d |> Result.mapError S.errorToString)
                            |> Result.map .status
                in
                oldRead |> Expect.equal (Ok (UnknownStatus "archived"))
        ]



-- CATCH-ALL FIXTURE ----------------------------------------------------------


type StatusNew
    = NActive
    | NDone
    | Archived


type alias StatusNewRefs =
    {}


statusNew : Ref.CustomRefs StatusNew StatusNewRefs
statusNew =
    Ref.custom
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
        |> Ref.variant0 "active" NActive
        |> Ref.variant0 "done" NDone
        |> Ref.variant0 "archived" Archived
        |> Ref.buildCustom


type StatusOld
    = OActive
    | ODone
    | UnknownStatus String


type alias StatusOldRefs =
    {}


statusOld : Ref.CustomRefs StatusOld StatusOldRefs
statusOld =
    Ref.custom
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
        |> Ref.variant0 "active" OActive
        |> Ref.variant0 "done" ODone
        |> Ref.catchAll "unknown" UnknownStatus
        |> Ref.buildCustom


type alias StatusDocNew =
    { status : StatusNew }


statusNewDoc : Ref.RecordRefs StatusDocNew { status : Ref StatusDocNew (S.Variants StatusNew) StatusNew }
statusNewDoc =
    Ref.record StatusDocNew (\s -> { status = s })
        |> Ref.field "status" .status statusNew.schema
        |> Ref.build


type alias StatusDocOld =
    { status : StatusOld }


statusOldDoc : Ref.RecordRefs StatusDocOld { status : Ref StatusDocOld (S.Variants StatusOld) StatusOld }
statusOldDoc =
    Ref.record StatusDocOld (\s -> { status = s })
        |> Ref.field "status" .status statusOld.schema
        |> Ref.build
