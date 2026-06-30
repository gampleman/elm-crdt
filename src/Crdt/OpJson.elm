module Crdt.OpJson exposing (encodeOps, opsDecoder)

{-| JSON wire format for operations, so an op-log document can be synced.

Mirrors `Crdt.Json` (which serializes the `Node` state tree) but for the
`Crdt.OpLog` operation types. A delta or a full document is just a list of `Op`s;
the receiver merges them into its own store. `Node` seeds inside insert ops and
the `OpId`/`Prim` leaves reuse the `Crdt.Json` codecs, so there is one source of
truth for value serialization.

Wire shape per op: `{ "id": <opid>, "deps": [<opid>...], "a": <action> }`, where
an action is tagged by `"k"`:

  - `{ "k": "reg", "t": <target>, "v": <prim> }`
  - `{ "k": "pres", "t": <target>, "p": <bool>, "s": <node> }`
  - `{ "k": "ins", "t": <target>, "e": <opid>, "o": <opid>|null, "s": <node> }`
  - `{ "k": "del", "t": <target>, "e": <opid> }`

A target is a list of steps: `{ "key": <string> }` or `{ "elem": <opid> }`.

-}

import Crdt.Json as Json
import Crdt.OpLog exposing (Action(..), Op, TargetStep(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE



-- ENCODE ---------------------------------------------------------------------


encodeOps : List Op -> JE.Value
encodeOps ops =
    JE.list encodeOp ops


encodeOp : Op -> JE.Value
encodeOp op =
    JE.object
        [ ( "id", Json.encodeOpId op.id )
        , ( "deps", JE.list Json.encodeOpId op.deps )
        , ( "a", encodeAction op.action )
        ]


encodeAction : Action -> JE.Value
encodeAction action =
    case action of
        SetReg target prim ->
            JE.object
                [ ( "k", JE.string "reg" )
                , ( "t", encodeTarget target )
                , ( "v", Json.encodePrim prim )
                ]

        SetPresence { target, present, seed } ->
            JE.object
                [ ( "k", JE.string "pres" )
                , ( "t", encodeTarget target )
                , ( "p", JE.bool present )
                , ( "s", Json.encodeNode seed )
                ]

        InsertElem { container, elemId, after, seed } ->
            JE.object
                [ ( "k", JE.string "ins" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId elemId )
                , ( "o"
                  , case after of
                        Just o ->
                            Json.encodeOpId o

                        Nothing ->
                            JE.null
                  )
                , ( "s", Json.encodeNode seed )
                ]

        DeleteElem { container, elem } ->
            JE.object
                [ ( "k", JE.string "del" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId elem )
                ]

        Increment { target, delta } ->
            JE.object
                [ ( "k", JE.string "inc" )
                , ( "t", encodeTarget target )
                , ( "d", JE.int delta )
                ]


encodeTarget : List TargetStep -> JE.Value
encodeTarget =
    JE.list encodeStep


encodeStep : TargetStep -> JE.Value
encodeStep step =
    case step of
        IntoKey k ->
            JE.object [ ( "key", JE.string k ) ]

        IntoElem id ->
            JE.object [ ( "elem", Json.encodeOpId id ) ]



-- DECODE ---------------------------------------------------------------------


opsDecoder : Decoder (List Op)
opsDecoder =
    JD.list opDecoder


opDecoder : Decoder Op
opDecoder =
    JD.map3 Op
        (JD.field "id" Json.opIdDecoder)
        (JD.field "deps" (JD.list Json.opIdDecoder))
        (JD.field "a" actionDecoder)


actionDecoder : Decoder Action
actionDecoder =
    JD.field "k" JD.string
        |> JD.andThen
            (\k ->
                case k of
                    "reg" ->
                        JD.map2 SetReg
                            (JD.field "t" targetDecoder)
                            (JD.field "v" Json.primDecoder)

                    "pres" ->
                        JD.map3 (\t p s -> SetPresence { target = t, present = p, seed = s })
                            (JD.field "t" targetDecoder)
                            (JD.field "p" JD.bool)
                            (JD.field "s" Json.nodeDecoder)

                    "ins" ->
                        JD.map4 (\t e o s -> InsertElem { container = t, elemId = e, after = o, seed = s })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)
                            (JD.field "o" (JD.nullable Json.opIdDecoder))
                            (JD.field "s" Json.nodeDecoder)

                    "del" ->
                        JD.map2 (\t e -> DeleteElem { container = t, elem = e })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)

                    "inc" ->
                        JD.map2 (\t delta -> Increment { target = t, delta = delta })
                            (JD.field "t" targetDecoder)
                            (JD.field "d" JD.int)

                    other ->
                        JD.fail ("unknown action kind: " ++ other)
            )


targetDecoder : Decoder (List TargetStep)
targetDecoder =
    JD.list stepDecoder


stepDecoder : Decoder TargetStep
stepDecoder =
    JD.oneOf
        [ JD.map IntoKey (JD.field "key" JD.string)
        , JD.map IntoElem (JD.field "elem" Json.opIdDecoder)
        ]
