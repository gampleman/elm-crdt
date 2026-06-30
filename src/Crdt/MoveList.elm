module Crdt.MoveList exposing
    ( MoveList
    , empty, fromParts, cells, values, deletedIds
    , insert, move, delete, updateValue
    , merge
    , toList, toEntries, get, homeCell, maxCounter
    )

{-| A list whose elements can be **moved** (reordered) while keeping their
identity — nested content and any cursor anchored to an item survive a move.

The design (see `docs/05-move.md`): an element is a stable **value** identified by
an `OpId`; positions are **cells** in an append-only RGA, each carrying a
valueId. Inserting a value appends a cell; **moving** it appends _another_ cell
(at the new position) carrying the same valueId. A value's live position is the
**max-`OpId` cell** that carries it — so the latest move wins, last-writer-wins by
`(counter, replica)`, with no separate home register. Older cells are simply
skipped, like tombstones.

Why this and not "re-point the RGA origin": re-pointing creates origin cycles
(`a→c→b→a`) the moment you move a non-tail element. Cells are **append-only** — a
new cell always anchors after an _existing, older_ cell — so the cell RGA can
never cycle, and order is a pure function of the cell set. That makes `merge` a
plain semilattice (RGA-union of cells, dict-union of values, set-union of
deletions) and the read a deterministic fold, so convergence needs no special
argument.

`c` is the item content (a `Node` in practice). Value identity is the `OpId`.

@docs MoveList
@docs empty, fromParts, cells, values, deletedIds
@docs insert, move, delete, updateValue
@docs merge
@docs toList, toEntries, get, homeCell, maxCounter

-}

import Crdt.Id as Id exposing (OpId)
import Crdt.Rga as Rga exposing (Rga)
import Dict exposing (Dict)
import Set exposing (Set)


{-| A movable list.

  - `cellRga` — append-only RGA of position cells; each cell's content is the
    `OpId` of the value it places. A value may have several cells (from moves);
    the max-id one is its current home.
  - `valueOf` — valueId → item content (stable across moves).
  - `tombstones` — deleted valueIds (grow-only; delete-wins on merge).

-}
type MoveList c
    = MoveList
        { cellRga : Rga OpId
        , valueOf : Dict String c
        , tombstones : Set String
        }


{-| The empty list.
-}
empty : MoveList c
empty =
    MoveList { cellRga = Rga.empty, valueOf = Dict.empty, tombstones = Set.empty }


{-| Build from raw parts (used by JSON decode).
-}
fromParts : Rga OpId -> Dict String c -> Set String -> MoveList c
fromParts cellRga valueOf tombstones =
    MoveList { cellRga = cellRga, valueOf = valueOf, tombstones = tombstones }


{-| The underlying cell RGA (for serialization).
-}
cells : MoveList c -> Rga OpId
cells (MoveList m) =
    m.cellRga


{-| The valueId → content map (for serialization).
-}
values : MoveList c -> Dict String c
values (MoveList m) =
    m.valueOf


{-| The set of deleted valueId strings (for serialization).
-}
deletedIds : MoveList c -> Set String
deletedIds (MoveList m) =
    m.tombstones



-- EDITS ----------------------------------------------------------------------


{-| Insert a new value (id = `valueId`) after the cell `afterCell` (`Nothing` =
head), carrying `content`. The cell's id equals the valueId.
-}
insert : OpId -> Maybe OpId -> c -> MoveList c -> MoveList c
insert valueId afterCell content (MoveList m) =
    MoveList
        { m
            | cellRga = Rga.put (Rga.element valueId afterCell valueId False) m.cellRga
            , valueOf = Dict.insert (Id.opIdToString valueId) content m.valueOf
        }


{-| Move the value `valueId` to sit after the cell `afterCell`. Appends a fresh
cell (id = `moveOp`, which is newer than any existing cell, so it becomes the
value's home) carrying the same valueId. Content and identity are untouched.
-}
move : OpId -> OpId -> Maybe OpId -> MoveList c -> MoveList c
move moveOp valueId afterCell (MoveList m) =
    MoveList { m | cellRga = Rga.put (Rga.element moveOp afterCell valueId False) m.cellRga }


{-| Delete a value by id (delete-wins; idempotent).
-}
delete : OpId -> MoveList c -> MoveList c
delete valueId (MoveList m) =
    MoveList { m | tombstones = Set.insert (Id.opIdToString valueId) m.tombstones }


{-| Transform a value's content in place (for nested edits into the item).
-}
updateValue : OpId -> (c -> c) -> MoveList c -> MoveList c
updateValue valueId f (MoveList m) =
    MoveList { m | valueOf = Dict.update (Id.opIdToString valueId) (Maybe.map f) m.valueOf }



-- MERGE ----------------------------------------------------------------------


{-| Merge two movable lists: RGA-union the cells, dict-union the values (shared
ids merge their content recursively via `mergeContent`), set-union the deletions.
Each component is a semilattice, so the whole is commutative/associative/
idempotent, and the order (max-cell-per-value over the merged cells) is a
deterministic function of the result — hence convergent.
-}
merge : (c -> c -> c) -> MoveList c -> MoveList c -> MoveList c
merge mergeContent (MoveList a) (MoveList b) =
    MoveList
        { cellRga =
            -- cell content is a valueId; a shared cell id always carries the same
            -- valueId, so the content-merge is a trivial pick.
            Rga.merge (\x _ -> x) a.cellRga b.cellRga
        , valueOf =
            Dict.merge
                Dict.insert
                (\k x y -> Dict.insert k (mergeContent x y))
                Dict.insert
                a.valueOf
                b.valueOf
                Dict.empty
        , tombstones = Set.union a.tombstones b.tombstones
        }



-- READ -----------------------------------------------------------------------


{-| Per-value max cell id: the home cell of each value (the latest move wins).
-}
homeCells : Rga OpId -> Dict String OpId
homeCells cellRga =
    Rga.elements cellRga
        |> List.foldl
            (\cell acc ->
                let
                    valueKey =
                        Id.opIdToString cell.content
                in
                case Dict.get valueKey acc of
                    Just existing ->
                        if Id.compareOpId cell.id existing == GT then
                            Dict.insert valueKey cell.id acc

                        else
                            acc

                    Nothing ->
                        Dict.insert valueKey cell.id acc
            )
            Dict.empty


{-| The value entries in order: walk the cells in RGA order and emit each present,
non-deleted value once, at its home cell's position.
-}
toEntries : MoveList c -> List ( OpId, c )
toEntries (MoveList m) =
    let
        homes =
            homeCells m.cellRga
    in
    Rga.toElementsInOrder m.cellRga
        |> List.filterMap
            (\cell ->
                let
                    valueKey =
                        Id.opIdToString cell.content
                in
                -- emit only at the value's home cell, and only if live
                if
                    (Dict.get valueKey homes == Just cell.id)
                        && not (Set.member valueKey m.tombstones)
                then
                    Dict.get valueKey m.valueOf
                        |> Maybe.map (\content -> ( cell.content, content ))

                else
                    Nothing
            )


{-| The values in order.
-}
toList : MoveList c -> List c
toList ml =
    toEntries ml |> List.map Tuple.second


{-| A value's content by id (regardless of position; `Nothing` if deleted/absent).
-}
get : OpId -> MoveList c -> Maybe c
get valueId (MoveList m) =
    if Set.member (Id.opIdToString valueId) m.tombstones then
        Nothing

    else
        Dict.get (Id.opIdToString valueId) m.valueOf


{-| The id of a value's current home cell — the anchor to insert/move _after_
this value. `Nothing` if the value has no cell.
-}
homeCell : OpId -> MoveList c -> Maybe OpId
homeCell valueId (MoveList m) =
    Dict.get (Id.opIdToString valueId) (homeCells m.cellRga)


{-| Largest Lamport counter anywhere (cell ids, cell contents = valueIds, and
nested content), for clock catch-up after merge.
-}
maxCounter : (c -> Int) -> MoveList c -> Int
maxCounter contentMax (MoveList m) =
    let
        cellMax =
            Rga.elements m.cellRga
                |> List.foldl
                    (\cell acc -> max acc (max (Id.opIdCounter cell.id) (Id.opIdCounter cell.content)))
                    0

        valueMax =
            Dict.foldl (\_ c acc -> max acc (contentMax c)) 0 m.valueOf
    in
    max cellMax valueMax
