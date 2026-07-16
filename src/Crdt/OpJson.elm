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
  - `{ "k": "ins", "t": <target>, "e": <opid>, "p": <opid>|null, "sd": <side>, "s": <node> }`
  - `{ "k": "itxt", "t": <target>, "e": <opid>, "x": <string>, "p": <opid>|null, "sd": <side> }`
    — a run-length text insert: `x` is the whole run, `e` its first char's id (`start`);
    char `i` gets the derived id `start+i` and chains as a right-spine (see
    `OpLog.insertTextRun`). One op per typed run instead of one per character.
  - `{ "k": "del", "t": <target>, "e": <opid> }`

A target is a list of steps: `{ "key": <string> }` or `{ "elem": <opid> }`.

-}

import Crdt.Frac as Frac
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

        InsertElem { container, elemId, parent, side, seed } ->
            JE.object
                [ ( "k", JE.string "ins" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId elemId )
                , ( "p"
                  , case parent of
                        Just p ->
                            Json.encodeOpId p

                        Nothing ->
                            JE.null
                  )
                , ( "sd", Json.encodeSide side )
                , ( "s", Json.encodeNode seed )
                ]

        InsertText { container, start, text, parent, side } ->
            JE.object
                [ ( "k", JE.string "itxt" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId start )
                , ( "x", JE.string text )
                , ( "p"
                  , case parent of
                        Just p ->
                            Json.encodeOpId p

                        Nothing ->
                            JE.null
                  )
                , ( "sd", Json.encodeSide side )
                ]

        DeleteElem { container, elem } ->
            JE.object
                [ ( "k", JE.string "del" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId elem )
                ]

        MoveElem { container, elem, after } ->
            JE.object
                [ ( "k", JE.string "mov" )
                , ( "t", encodeTarget container )
                , ( "e", Json.encodeOpId elem )
                , ( "o"
                  , case after of
                        Just o ->
                            Json.encodeOpId o

                        Nothing ->
                            JE.null
                  )
                ]

        Increment { target, delta } ->
            JE.object
                [ ( "k", JE.string "inc" )
                , ( "t", encodeTarget target )
                , ( "d", JE.int delta )
                ]

        TreeMove { container, child, parent, pos, seed } ->
            JE.object
                [ ( "k", JE.string "tree" )
                , ( "t", encodeTarget container )
                , ( "c", Json.encodeOpId child )
                , ( "p"
                  , case parent of
                        Just p ->
                            Json.encodeOpId p

                        Nothing ->
                            JE.null
                  )
                , ( "pos", JE.list JE.int (Frac.toList pos) )
                , ( "s"
                  , case seed of
                        Just n ->
                            Json.encodeNode n

                        Nothing ->
                            JE.null
                  )
                ]

        AddMark { container, markId, type_, value, start, end } ->
            JE.object
                [ ( "k", JE.string "mark" )
                , ( "t", encodeTarget container )
                , ( "m", Json.encodeOpId markId )
                , ( "ty", JE.string type_ )
                , ( "v", Json.encodePrim value )
                , ( "st", Json.encodeMarkAnchor start )
                , ( "en", Json.encodeMarkAnchor end )
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
                        JD.map5 (\t e p sd s -> InsertElem { container = t, elemId = e, parent = p, side = sd, seed = s })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)
                            (JD.field "p" (JD.nullable Json.opIdDecoder))
                            (JD.field "sd" Json.sideDecoder)
                            (JD.field "s" Json.nodeDecoder)

                    "itxt" ->
                        JD.map5 (\t e x p sd -> InsertText { container = t, start = e, text = x, parent = p, side = sd })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)
                            (JD.field "x" JD.string)
                            (JD.field "p" (JD.nullable Json.opIdDecoder))
                            (JD.field "sd" Json.sideDecoder)

                    "del" ->
                        JD.map2 (\t e -> DeleteElem { container = t, elem = e })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)

                    "mov" ->
                        JD.map3 (\t e o -> MoveElem { container = t, elem = e, after = o })
                            (JD.field "t" targetDecoder)
                            (JD.field "e" Json.opIdDecoder)
                            (JD.field "o" (JD.nullable Json.opIdDecoder))

                    "inc" ->
                        JD.map2 (\t delta -> Increment { target = t, delta = delta })
                            (JD.field "t" targetDecoder)
                            (JD.field "d" JD.int)

                    "tree" ->
                        JD.map5 (\t c p pos s -> TreeMove { container = t, child = c, parent = p, pos = Frac.fromList pos, seed = s })
                            (JD.field "t" targetDecoder)
                            (JD.field "c" Json.opIdDecoder)
                            (JD.field "p" (JD.nullable Json.opIdDecoder))
                            (JD.field "pos" (JD.list JD.int))
                            (JD.field "s" (JD.nullable Json.nodeDecoder))

                    "mark" ->
                        JD.map6 (\t m ty v st en -> AddMark { container = t, markId = m, type_ = ty, value = v, start = st, end = en })
                            (JD.field "t" targetDecoder)
                            (JD.field "m" Json.opIdDecoder)
                            (JD.field "ty" JD.string)
                            (JD.field "v" Json.primDecoder)
                            (JD.field "st" Json.markAnchorDecoder)
                            (JD.field "en" Json.markAnchorDecoder)

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
