module Crdt.Edit exposing
    ( Error(..)
    , setText, setBool
    , listAppend
    , setKey, removeKey
    )

{-| Path-addressed edits. Every edit navigates a `Path` into the document's
`Node` tree, applies one primitive CRDT operation at the leaf, mints any new ids
from the document's context, and records the change for undo.

Edits are decoupled from the schema: they operate on the raw `Node`
representation, so CRDT correctness never depends on the typed codec. They take
_visible_ indices (tombstones already skipped) and return a `Result` so callers
can surface bad paths.

@docs Error
@docs setText, setBool
@docs listAppend
@docs setKey, removeKey

-}

import Crdt.Id as Id exposing (Ctx)
import Crdt.Internal as I exposing (Doc, Seed)
import Crdt.Node as Node exposing (Node, Prim(..))
import Crdt.Path as Path exposing (Path, Seg(..))
import Crdt.Rga as Rga
import Crdt.Text as Text
import Dict


{-| Why an edit failed.
-}
type Error
    = PathNotFound
    | WrongNodeType



-- PRIMITIVE SETTERS ----------------------------------------------------------


{-| Set a register leaf to a primitive, last-write-wins. Mints a fresh stamp.
-}
setPrim : Path -> Prim -> Doc -> Result Error Doc
setPrim path prim doc =
    transform path
        (\_ ctx ->
            let
                ( stamp, ctx1 ) =
                    Id.nextId ctx
            in
            Ok ( Node.reg prim stamp, ctx1 )
        )
        doc


{-| Set a boolean register.
-}
setBool : Path -> Bool -> Doc -> Result Error Doc
setBool path value =
    setPrim path (PBool value)


{-| Edit a text node so it reads as the given string, applying the minimal RGA
insert/delete diff (so concurrent edits in other regions survive the merge).
-}
setText : Path -> String -> Doc -> Result Error Doc
setText path value doc =
    transform path
        (\node ctx ->
            case node of
                Node.Txt rga ->
                    let
                        ( rga1, ctx1 ) =
                            Text.applyString ctx value rga
                    in
                    Ok ( Node.txt rga1, ctx1 )

                _ ->
                    Err WrongNodeType
        )
        doc



-- LIST EDITS -----------------------------------------------------------------


{-| Append a fresh subtree (built by a `Seed`, e.g. `schema |> with value`) to
the end of a sequence.
-}
listAppend : Path -> Seed -> Doc -> Result Error Doc
listAppend path seed doc =
    transform path
        (\node ctx ->
            case node of
                Node.Seq rga ->
                    let
                        ( childNode, ctx1 ) =
                            I.runSeed seed ctx

                        origin =
                            Rga.lastVisibleId rga

                        ( rga1, ctx2 ) =
                            Rga.insertAfter ctx1 origin childNode rga
                    in
                    Ok ( Node.seq rga1, ctx2 )

                _ ->
                    Err WrongNodeType
        )
        doc



-- DICT EDITS -----------------------------------------------------------------


{-| Set (or overwrite) a dictionary key to a fresh subtree, marking it present.
-}
setKey : Path -> String -> Seed -> Doc -> Result Error Doc
setKey path k seed doc =
    transform path
        (\node ctx ->
            case node of
                Node.Map entries ->
                    let
                        ( childNode, ctx1 ) =
                            I.runSeed seed ctx

                        ( stamp, ctx2 ) =
                            Id.nextId ctx1
                    in
                    Ok ( Node.mapFromEntries (Dict.insert k (Node.entry stamp True childNode) entries), ctx2 )

                _ ->
                    Err WrongNodeType
        )
        doc


{-| Remove a dictionary key by tombstoning its presence cell (LWW). A concurrent
set with a later stamp wins; a concurrent set with an earlier stamp loses.
-}
removeKey : Path -> String -> Doc -> Result Error Doc
removeKey path k doc =
    transform path
        (\node ctx ->
            case node of
                Node.Map entries ->
                    case Dict.get k entries of
                        Just e ->
                            let
                                ( stamp, ctx1 ) =
                                    Id.nextId ctx
                            in
                            Ok
                                ( Node.mapFromEntries (Dict.insert k { e | present = False, stamp = stamp } entries)
                                , ctx1
                                )

                        Nothing ->
                            -- nothing to remove; no-op
                            Ok ( node, ctx )

                _ ->
                    Err WrongNodeType
        )
        doc



-- NAVIGATION CORE ------------------------------------------------------------


{-| Walk `path` into the doc's root, apply `f` (which produces a replacement node
and an advanced context) at the addressed node, and rebuild the tree along the
way. Records the change for undo via `Internal.withRoot`.
-}
transform : Path -> (Node -> Ctx -> Result Error ( Node, Ctx )) -> Doc -> Result Error Doc
transform path f doc =
    let
        segs =
            Path.segments path
    in
    case go segs f (I.root doc) (I.ctx doc) of
        Ok ( newRoot, newCtx ) ->
            Ok (I.withRoot newRoot newCtx doc)

        Err e ->
            Err e


go : List Seg -> (Node -> Ctx -> Result Error ( Node, Ctx )) -> Node -> Ctx -> Result Error ( Node, Ctx )
go segs f node ctx =
    case segs of
        [] ->
            f node ctx

        seg :: rest ->
            case seg of
                Field name ->
                    descendMap name rest f node ctx

                Key name ->
                    descendMap name rest f node ctx

                Index i ->
                    descendSeq i rest f node ctx

                NodeId _ ->
                    -- trees are edited via Crdt.Ref/OpDoc, not the state-based Edit API
                    Err WrongNodeType


descendMap : String -> List Seg -> (Node -> Ctx -> Result Error ( Node, Ctx )) -> Node -> Ctx -> Result Error ( Node, Ctx )
descendMap name rest f node ctx =
    case node of
        Node.Map entries ->
            case Dict.get name entries of
                Just e ->
                    go rest f e.value ctx
                        |> Result.map
                            (\( childNode, ctx1 ) ->
                                ( Node.mapFromEntries (Dict.insert name { e | value = childNode } entries)
                                , ctx1
                                )
                            )

                Nothing ->
                    Err PathNotFound

        _ ->
            Err WrongNodeType


descendSeq : Int -> List Seg -> (Node -> Ctx -> Result Error ( Node, Ctx )) -> Node -> Ctx -> Result Error ( Node, Ctx )
descendSeq i rest f node ctx =
    case node of
        Node.Seq rga ->
            case Rga.idAtVisibleIndex i rga of
                Just id ->
                    case Rga.get id rga of
                        Just el ->
                            go rest f el.content ctx
                                |> Result.map
                                    (\( childNode, ctx1 ) ->
                                        ( Node.seq (Rga.updateElement id (\_ -> childNode) rga), ctx1 )
                                    )

                        Nothing ->
                            Err PathNotFound

                Nothing ->
                    Err PathNotFound

        _ ->
            Err WrongNodeType
