module ReadErrorTests exposing (suite)

{-| **A failed read says _where_ it failed.**

`Crdt.Doc.ReadError` reports a document that doesn't match the schema reading it — the
schema-evolution case no `withDefault`/`optional`/`catchAll` was declared for. Schemas
compose arbitrarily deep, so the leaf complaint alone ("missing field: weight") is close to
useless on a real document: it names the field and nothing about which of the hundred list
elements, dict keys or nested records it sat in. So every combinator that descends tags the
failure with the step it took, and the error renders as `path › to › it: what went wrong`
(the same shape, for the same reason, as `Json.Decode.Error`).

The trail is load-bearing beyond diagnostics, which is the second group here.
`withDefault`/`optional` absorb an `UnknownVariant` (a newer peer's variant) and nothing
else — and they now have to match the **leaf** of a possibly-wrapped error. So the
tolerance is asserted with the unknown tag both at the top level and one level down inside
the defaulted value: the nested case is what a naive `Err (UnknownVariant _)` pattern
silently stops catching once paths exist.

The last two groups cover the one place a mismatch used to be **swallowed** rather than
reported: text. `Txt`/`Rich` elements are typed with the open `Node` union
(`design-docs/16-typed-sequence-content.md`), so nothing stops an op putting a container or
a multi-character run in a text sequence — and the read used to `filterMap` it away, the same
way on every replica, so no convergence test could see the loss either. The ops here are
hand-written because no edit API can produce them: that is the point, since the shapes that
need handling are the ones a mismatched schema, a mangled relay or someone else's bug puts
on the wire. Plain text admits single characters only; rich text admits characters, block
markers and nest tokens, and both are asserted — including that the checks accept everything
the library itself writes.

-}

import Crdt as C exposing (Ref)
import Crdt.Doc as Doc exposing (Doc)
import Crdt.Edit as Edit
import Crdt.Id as Id
import Crdt.Schema.Internal as SI
import Dict
import Expect
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "read errors"
        [ describe "say where"
            [ test "a field missing from a list element names the list, the index and the field" <|
                \_ ->
                    -- The writer's items carry only a title; the reader's schema requires a
                    -- `weight` too and declares no tolerance for its absence. The path stops
                    -- at the element: a `MissingField` already names the field it wanted, so
                    -- the descent that would have added it never happened.
                    writerList [ "a", "b", "c" ]
                        |> joinRead strictList.schema
                        |> Expect.equal (Err "items › 0: missing field: weight")
            , test "a field missing from a dict value names the failing key" <|
                \_ ->
                    -- A key, not a positional index: the path has to carry what the
                    -- container actually addresses by, or it points at nothing.
                    writerDict [ "alpha", "beta" ]
                        |> joinRead strictDict.schema
                        |> Expect.equal (Err "notes › alpha: missing field: weight")
            , test "the rendered form is path steps, separated, then the reason" <|
                \_ ->
                    -- Pinned directly, because the join path above cannot produce every
                    -- shape: in particular a leaf error with no path must not grow a stray
                    -- separator, and depth must not be flattened away.
                    [ SI.MissingField "x"
                    , SI.At "items" (SI.MissingField "x")
                    , SI.At "items" (SI.At "0" (SI.At "weight" (SI.MissingField "x")))
                    , SI.At "s" (SI.TypeMismatch "expected list")
                    , SI.At "s" (SI.UnknownVariant "archived")
                    ]
                        |> List.map SI.errorToString
                        |> Expect.equalLists
                            [ "missing field: x"
                            , "items: missing field: x"
                            , "items › 0 › weight: missing field: x"
                            , "s: type mismatch: expected list"
                            , "s: unknown variant: archived"
                            ]
            ]
        , describe "still tolerate an unknown variant, at any depth"
            [ test "withDefault absorbs an unknown tag at its own level" <|
                \_ ->
                    -- The case that worked before paths existed, kept as the control.
                    writerFlatStatus
                        |> joinRead flatDefaulted.schema
                        |> Result.map .status
                        |> Expect.equal (Ok Active)
            , test "withDefault absorbs an unknown tag nested inside the defaulted value" <|
                \_ ->
                    -- The tolerance is declared on `inner` (a whole record); the unknown tag
                    -- is on that record's `status` field, so the error reaching `withDefault`
                    -- is wrapped in the field's path. Matching the error rather than its leaf
                    -- would fail this read while still passing the flat case above.
                    writerNestedStatus
                        |> joinRead nestedDefaulted.schema
                        |> Result.map .inner
                        |> Expect.equal (Ok (Inner Active))
            , test "a real error inside a defaulted value is NOT absorbed" <|
                \_ ->
                    -- The guard against over-tolerance: `withDefault` covers evolution, not
                    -- corruption. A required field genuinely absent must still fail, with
                    -- its path intact.
                    writerList [ "a" ]
                        |> joinRead defaultedStrictList.schema
                        |> Expect.equal (Err "items › 0: missing field: weight")
            ]
        , describe "a text sequence takes characters only, and the codec enforces it"
            [ test "a text element carrying a whole node, or more than one character, is rejected" <|
                \_ ->
                    -- Where the invariant can actually be violated on the wire: inside a
                    -- `txt` NODE (a snapshot base, a `pres` seed, a list-of-text element).
                    -- A `Txt` element encodes its character directly, so there is no shape
                    -- for a node to arrive in, and the one thing a `String` can still get
                    -- wrong — more than one character — is checked once at the edge
                    -- (`Crdt.Json.charDecoder`). One element per character is what keeps
                    -- `applyCharDiff`'s id array aligned with the text, so a two-character
                    -- element would silently shift every cursor and diff position after it.
                    [ deliverBase (txtNode """{"t":"reg","v":{"k":"string","x":"a"},"s":[5,"peer"]}""")
                    , deliverBase (txtNode "\"ab\"")
                    , deliverBase (txtNode "7")
                    , deliverBase (txtNode "null")
                    ]
                        |> Expect.equalLists (List.repeat 4 (Err "rejected"))
            , test "one character is accepted, and an emoji counts as one" <|
                \_ ->
                    -- An astral code point is two UTF-16 units but one `Char`, and that is how
                    -- every writer splits text — measuring the wrong one would reject
                    -- perfectly ordinary text.
                    [ deliverBase (txtNode "\"q\"")
                    , deliverBase (txtNode "\"🙈\"")
                    ]
                        |> Expect.equalLists [ Ok (), Ok () ]
            , test "text written through the API survives a whole-document round trip" <|
                \_ ->
                    -- The control: the codec must not reject what the library itself writes.
                    C.init (Id.replica "w") textDoc.schema
                        |> (\d -> Edit.set textDoc.note "hi ✅ ünïcøde" d |> ok d)
                        |> roundTripRead
                        |> Expect.equal (Ok "hi ✅ ünïcøde")
            , test "an insert aimed at a text field is INERT, not destructive" <|
                \_ ->
                    -- `ins` carries a node seed, so it belongs to `Seq`/`Mov`; text is inserted
                    -- by `itxt` and block structure by `tok`. Such an op still decodes (it is
                    -- a well-formed op, just for the wrong container), so the fold has to
                    -- ignore it — and specifically must NOT fall through to its
                    -- create-the-container case, which would replace the whole text field
                    -- with an empty `Seq` and take the text with it.
                    let
                        written =
                            C.init (Id.replica "w") textDoc.schema
                                |> (\d -> Edit.set textDoc.note "keep me" d |> ok d)
                    in
                    Doc.decodeInto (opsPayload [ insertOp "note" """{"t":"map","e":{}}""" ]) written
                        |> Result.mapError (\_ -> "rejected")
                        |> Result.andThen
                            (\d -> Doc.read d |> Result.map .note |> Result.mapError Doc.readErrorToString)
                        |> Expect.equal (Ok "keep me")
            ]
        , describe "a rich sequence takes characters and structural tokens"
            [ test "a character and both block tokens decode" <|
                \_ ->
                    -- The three-case vocabulary, tagged on the wire: `{"c":ch}` for a
                    -- character, `{"tk":"m"}`/`{"tk":"n"}` for the block-structure tokens
                    -- (`design-docs/11-block-structure.md`). These used to be a register
                    -- carrying a magic `PInt`, distinguishable only by convention — so an
                    -- element could claim to be a marker by accident.
                    [ deliverBase (richNode """{"c":"a"}""")
                    , deliverBase (richNode """{"tk":"m"}""")
                    , deliverBase (richNode """{"tk":"n"}""")
                    ]
                        |> Expect.equalLists [ Ok (), Ok (), Ok () ]
            , test "an untagged, unknown-tagged or over-long rich element is rejected" <|
                \_ ->
                    [ deliverBase (richNode """{"t":"map","e":{}}""")
                    , deliverBase (richNode """{"tk":"z"}""")
                    , deliverBase (richNode "7")
                    , deliverBase (richNode """{"c":"ab"}""")
                    ]
                        |> Expect.equalLists (List.repeat 4 (Err "rejected"))
            ]
        ]



-- TEXT VOCABULARY FIXTURES ---------------------------------------------------


type alias TextDoc =
    { note : String }


textDoc : { note : Ref TextDoc C.Settable String, schema : C.Schema C.Nested TextDoc }
textDoc =
    C.record TextDoc (\n schema -> { note = n, schema = schema })
        |> C.field "note" .note C.text
        |> C.build


{-| An `ins` op placing `contentJson` as an element of field `field`. Hand-written rather
than emitted, because no edit API can produce these — that is the point: the codec has to
cope with what a mismatched schema, a mangled relay or someone else's bug puts on the wire.
-}
insertOp : String -> String -> String
insertOp field contentJson =
    """{"id":[5,"peer"],"deps":[],"a":{"k":"ins","t":[{"key":"FIELD"}],"e":[5,"peer"],"p":null,"sd":"R","s":CONTENT}}"""
        |> String.replace "FIELD" field
        |> String.replace "CONTENT" contentJson


opsPayload : List String -> JD.Value
opsPayload ops =
    ("""{"kind":"ops","ops":[""" ++ String.join "," ops ++ "]}")
        |> JD.decodeString JD.value
        |> Result.withDefault JE.null


{-| A one-element `txt` node with the given element content, for injecting as a snapshot base.
-}
txtNode : String -> String
txtNode contentJson =
    """{"t":"txt","el":[{"id":[5,"peer"],"p":null,"s":"R","c":CONTENT,"d":false}]}"""
        |> String.replace "CONTENT" contentJson


{-| A one-element `rich` node with the given element content.
-}
richNode : String -> String
richNode contentJson =
    """{"t":"rich","el":[{"id":[5,"peer"],"p":null,"s":"R","c":CONTENT,"d":false}],"marks":{}}"""
        |> String.replace "CONTENT" contentJson


{-| Hand a raw node to `decodeInto` as a snapshot **base** — the general way to make the node
codec read arbitrary state, since the same decoder runs on every `pres`/`ins` seed and on a
compacted peer's base.

The base is then ignored (an empty frontier does not strictly cover ours), and the receiving
schema is never consulted, so the assertion is purely whether the **bytes** were accepted.
That is why a `rich` node can be delivered to a text-schema document here: the question is
about the codec, not about the read.

-}
deliverBase : String -> Result String ()
deliverBase nodeJson =
    ("""{"kind":"snapshot","base":""" ++ nodeJson ++ ""","frontier":[],"ops":[]}""")
        |> JD.decodeString JD.value
        |> Result.mapError (\_ -> "test payload is not JSON")
        |> Result.andThen
            (\payload ->
                Doc.decodeInto payload (C.init (Id.replica "reader") textDoc.schema)
                    |> Result.map (always ())
                    |> Result.mapError (\_ -> "rejected")
            )


{-| A whole-document round trip, so what we wrote really goes through the codec instead of
staying in memory.
-}
roundTripRead : Doc TextDoc -> Result String String
roundTripRead from =
    Doc.decodeInto (Doc.encode from) (C.init (Id.replica "reader") textDoc.schema)
        |> Result.mapError (\_ -> "decode rejected the document")
        |> Result.andThen (\d -> Doc.read d |> Result.map .note |> Result.mapError Doc.readErrorToString)



-- HELPERS --------------------------------------------------------------------


ok : Doc a -> Result Edit.EditError (Doc a) -> Doc a
ok fallback =
    Result.withDefault fallback


{-| The realistic join: the reader `init`s its own schema's base, then takes the writer's
whole document over the wire and reads it through that schema.
-}
joinRead : C.Schema C.Nested a -> Doc b -> Result String a
joinRead schema from =
    Doc.decodeInto (Doc.encode from) (C.init (Id.replica "reader") schema)
        |> Result.mapError (\_ -> "decode")
        |> Result.andThen (\d -> Doc.read d |> Result.mapError Doc.readErrorToString)



-- ITEMS: the writer stores a title, the reader demands a weight too ----------


type alias LooseItem =
    { title : String }


looseItem : { title : Ref LooseItem C.Settable String, schema : C.Schema C.Nested LooseItem }
looseItem =
    C.record LooseItem (\t schema -> { title = t, schema = schema })
        |> C.field "title" .title C.text
        |> C.build


type alias StrictItem =
    { title : String, weight : Int }


strictItem :
    { title : Ref StrictItem C.Settable String
    , weight : Ref StrictItem C.Settable Int
    , schema : C.Schema C.Nested StrictItem
    }
strictItem =
    C.record StrictItem (\t w schema -> { title = t, weight = w, schema = schema })
        |> C.field "title" .title C.text
        |> C.field "weight" .weight C.int
        |> C.build



-- LIST FLAVOUR ---------------------------------------------------------------


type alias ListDoc a =
    { items : List a }


looseList :
    { items : Ref (ListDoc LooseItem) (C.ListK C.Fixed C.Nested LooseItem) (List LooseItem)
    , schema : C.Schema C.Nested (ListDoc LooseItem)
    }
looseList =
    C.record ListDoc (\i schema -> { items = i, schema = schema })
        |> C.field "items" .items (C.list looseItem)
        |> C.build


strictList : { schema : C.Schema C.Nested (ListDoc StrictItem) }
strictList =
    C.record ListDoc (\_ schema -> { schema = schema })
        |> C.field "items" .items (C.list strictItem)
        |> C.build


{-| Same as `strictList`, but the element is wrapped in a `withDefault` — so a genuine
failure inside it is the thing that must NOT be swallowed.
-}
defaultedStrictList : { schema : C.Schema C.Nested (ListDoc StrictItem) }
defaultedStrictList =
    C.record ListDoc (\_ schema -> { schema = schema })
        |> C.field "items" .items (C.list (C.withDefault (StrictItem "" 0) strictItem))
        |> C.build


writerList : List String -> Doc (ListDoc LooseItem)
writerList titles =
    List.foldl
        (\title d -> Edit.append looseList.items (LooseItem title) d |> ok d)
        (C.init (Id.replica "writer") looseList.schema)
        titles



-- DICT FLAVOUR ---------------------------------------------------------------


type alias DictDoc a =
    { notes : Dict.Dict String a }


looseDict :
    { notes : Ref (DictDoc LooseItem) (C.DictK C.Nested LooseItem) (Dict.Dict String LooseItem)
    , schema : C.Schema C.Nested (DictDoc LooseItem)
    }
looseDict =
    C.record DictDoc (\n schema -> { notes = n, schema = schema })
        |> C.field "notes" .notes (C.dict looseItem)
        |> C.build


strictDict : { schema : C.Schema C.Nested (DictDoc StrictItem) }
strictDict =
    C.record DictDoc (\_ schema -> { schema = schema })
        |> C.field "notes" .notes (C.dict strictItem)
        |> C.build


writerDict : List String -> Doc (DictDoc LooseItem)
writerDict keys =
    List.foldl
        (\key d -> Edit.setKey looseDict.notes key (LooseItem key) d |> ok d)
        (C.init (Id.replica "writer") looseDict.schema)
        keys



-- STATUS: a variant the reader's schema doesn't know -------------------------


type NewStatus
    = NewActive
    | Archived


newStatus : { schema : C.Schema (C.Variants NewStatus) NewStatus }
newStatus =
    C.custom
        (\active archived v ->
            case v of
                NewActive ->
                    active

                Archived ->
                    archived
        )
        (\schema -> { schema = schema })
        |> C.variant0 "active" NewActive
        |> C.variant0 "archived" Archived
        |> C.buildCustom


type OldStatus
    = Active


oldStatus : { schema : C.Schema (C.Variants OldStatus) OldStatus }
oldStatus =
    C.custom
        (\active v ->
            case v of
                Active ->
                    active
        )
        (\schema -> { schema = schema })
        |> C.variant0 "active" Active
        |> C.buildCustom



-- FLAT: withDefault sits directly on the custom schema -----------------------


type alias StatusDoc s =
    { status : s }


writerFlat : { status : Ref (StatusDoc NewStatus) (C.Variants NewStatus) NewStatus, schema : C.Schema C.Nested (StatusDoc NewStatus) }
writerFlat =
    C.record StatusDoc (\s schema -> { status = s, schema = schema })
        |> C.field "status" .status newStatus
        |> C.build


flatDefaulted : { schema : C.Schema C.Nested (StatusDoc OldStatus) }
flatDefaulted =
    C.record StatusDoc (\_ schema -> { schema = schema })
        |> C.field "status" .status (C.withDefault Active oldStatus)
        |> C.build


writerFlatStatus : Doc (StatusDoc NewStatus)
writerFlatStatus =
    C.init (Id.replica "writer") writerFlat.schema
        |> (\d -> Edit.switch writerFlat.status Archived d |> ok d)



-- NESTED: withDefault sits on a record, the unknown tag on its field ---------


type alias Inner s =
    { status : s }


type alias OuterDoc s =
    { inner : Inner s }


writerInner : { status : Ref (Inner NewStatus) (C.Variants NewStatus) NewStatus, schema : C.Schema C.Nested (Inner NewStatus) }
writerInner =
    C.record Inner (\s schema -> { status = s, schema = schema })
        |> C.field "status" .status newStatus
        |> C.build


writerOuter :
    { inner : Ref (OuterDoc NewStatus) C.Nested (Inner NewStatus)
    , schema : C.Schema C.Nested (OuterDoc NewStatus)
    }
writerOuter =
    C.record OuterDoc (\i schema -> { inner = i, schema = schema })
        |> C.field "inner" .inner writerInner
        |> C.build


readerInner : { schema : C.Schema C.Nested (Inner OldStatus) }
readerInner =
    C.record Inner (\_ schema -> { schema = schema })
        |> C.field "status" .status oldStatus
        |> C.build


nestedDefaulted : { schema : C.Schema C.Nested (OuterDoc OldStatus) }
nestedDefaulted =
    C.record OuterDoc (\_ schema -> { schema = schema })
        |> C.field "inner" .inner (C.withDefault (Inner Active) readerInner)
        |> C.build


writerNestedStatus : Doc (OuterDoc NewStatus)
writerNestedStatus =
    C.init (Id.replica "writer") writerOuter.schema
        |> (\d -> Edit.switch (writerOuter.inner |> C.at writerInner.status) Archived d |> ok d)
