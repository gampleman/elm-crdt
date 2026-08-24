module Crdt.Node exposing
    ( Node(..), Register, Prim(..), Entry, Increment, MovNode, TreeNode
    , RichNode, RichElem(..), BlockToken(..), MarkOp, MarkAnchor, AnchorSide(..)
    , reg, mapFromEntries, entry, seq, txt, counter, increment, mov, tree, rich
    , asPrim, asMap, presentEntries, asSeq, asTxt, asCounter, asMov, asTree, asRich
    , maxCounter, compactTombstones, vacate
    , reStamp, reStampWithMap
    , Element, RgaNode
    )

{-| The uniform replicated-state type that every CRDT document is made of.

`Node` is a closed recursive union:

  - `Reg` — a last-write-wins register (a primitive leaf);
  - `Map` — a keyed collection used for **both** records and dicts. Each entry
    carries a **presence cell** (an LWW boolean with its own stamp) alongside its
    value, so that a removed key wins against a concurrent value edit by stamp.
    This is what makes dictionary key removal a well-behaved CRDT instead of an
    ambiguous set-vs-remove race;
  - `Seq` — a sequence, backed by `Crdt.Rga` at `Rga Node`;
  - `Txt` — collaborative text, also an `Rga Node` (of single-char registers);
  - `Cnt` — a PN-counter: a map of per-op signed contributions, summed to a value;
  - `Mov` — a movable (reorderable) list, backed by `Crdt.MoveList`;
  - `Tree` — a movable tree, backed by `Crdt.Tree.Internal`;
  - `Rich` — rich text: a character `Rga` plus a set of Peritext mark ops.

A `Node` is **derived state**, never merged as state: documents converge by
op-union plus deterministic replay (`Crdt.OpLog`), which folds ops onto a `Node` one
element at a time via the container mutators (`Rga.put`/`delete`, `Tree.move`,
`MoveList.move`, …). The conflict rules therefore live in that fold plus the read-time
ordering, not in a structural join — see `Crdt.OpLog`'s module docs. This module once
also carried a state-level `merge` (the join of each container, with LWW-by-stamp /
presence-AND / tombstone-OR written down explicitly); it was removed once nothing but
its own tests called it, since an unenforced parallel specification can drift from the
op path silently. `tests/NodeFuzzTests.elm` now fuzzes arbitrary (including
adversarial) `Node` values against the paths that _do_ ship.

@docs Node, Register, Prim, Entry, Increment, MovNode, TreeNode
@docs RichNode, RichElem, BlockToken, MarkOp, MarkAnchor, AnchorSide
@docs reg, mapFromEntries, entry, seq, txt, counter, increment, mov, tree, rich
@docs asPrim, asMap, presentEntries, asSeq, asTxt, asCounter, asMov, asTree, asRich
@docs maxCounter, compactTombstones, vacate
@docs reStamp, reStampWithMap
@docs Element, RgaNode

-}

import Crdt.Id.Internal as Id exposing (OpId)
import Crdt.MoveList as MoveList exposing (MoveList)
import Crdt.Rga as Rga exposing (Rga)
import Crdt.Tree.Internal as Tree exposing (Tree)
import Dict exposing (Dict)
import Set exposing (Set)


{-| The replicated state.

**Content is typed per container** (`design-docs/16-typed-sequence-content.md`). All the
sequence-shaped containers share `Crdt.Rga`'s ordering, tombstone rules and compaction —
that is what `Rga`'s content polymorphism is for — but they do **not** share a content type:

  - `Seq`/`Mov`/`Tree` hold arbitrary documents (`Node`), because a list element or a tree
    payload genuinely is one;
  - `Txt` holds one character per element (a `String` of exactly one `Char`, checked at the
    decoder — `Crdt.Json.charDecoder`);
  - `Rich` holds a `RichElem` — a character, a block marker, or a nest token.

These used to all be `Rga Node`, with a text character represented as a whole LWW register
and a block marker as a magic `PInt` inside one. That cost an `OpId` stamp per character that
nothing read, made `Rich`'s three element kinds an unchecked convention, and let an op put a
map inside a text field — which the read then silently skipped, identically on every replica,
so no convergence test could see the loss. The types rule all three out.

-}
type Node
    = Reg Register
    | Map (Dict String Entry)
    | Seq (Rga Node)
    | Txt (Rga String)
    | Cnt (Dict String Increment)
    | Mov MovNode
    | Tree TreeNode
    | Rich RichNode


{-| A movable list (reorderable sequence) of `Node` content.
-}
type alias MovNode =
    MoveList Node


{-| A movable tree of `Node` content.
-}
type alias TreeNode =
    Tree Node


{-| Rich (formatted) text: a Fugue sequence of `RichElem` plus an append-only set of
**mark operations** keyed by each op's `OpId`. Marks are Peritext-style ranges
anchored to character identities, not offsets, so they survive concurrent editing
and are agnostic to the ordering algorithm. Merge of the mark set is a `Dict.union`
(a semilattice, like the counter and tree move-set). The flatten-to-spans read model
and cover logic live in `Crdt.RichText`.
-}
type alias RichNode =
    { text : Rga RichElem
    , marks : Dict String MarkOp
    }


{-| One element of a rich-text sequence. Block structure lives **in the character
sequence** rather than in a separate container (`design-docs/11-block-structure.md`): a
`Marker` is a block boundary, and the `Nest` tokens following one give that block's indent
depth. So a rich sequence is a three-case sum, and this is it.

Only characters are visible text; the tokens are structural. Every offset a caller supplies
counts **characters only** (that is what an editor reports and what `markRange` marks over),
so the two must never be conflated — counting tokens drifts a caret one position per
preceding block boundary, which is a bug this representation has already produced once.

-}
type RichElem
    = TextChar String
    | Token BlockToken


{-| Which structural token an element is. Separate from `RichElem` because an op inserting
one carries the choice without a character to go with it (`OpLog.InsertToken`).
-}
type BlockToken
    = Marker
    | Nest


{-| One mark operation: sets (or, with `value = PNull`, clears) a formatting mark of
kind `type_` over the range `[start, end]`. `id` is the op's Lamport id and drives
per-character last-writer-wins at read time (a later op over the same character and
type wins). `value` is `PBool True` for a boolean mark being turned on, `PString _`
for a value mark (link href, color), or `PNull` to clear.

`MarkOp`/`MarkAnchor`/`AnchorSide` live here rather than in `Crdt.RichText.Internal`
(where the logic that interprets them lives) only because they are _part of the `Rich`
node_ — and `RichText.Internal` imports this module for `Node`/`RgaNode`, so defining
them there would be an import cycle. They are data with no behaviour attached; the
flatten-to-spans and cover rules that give them meaning are all in `RichText`.

-}
type alias MarkOp =
    { id : OpId
    , type_ : String
    , value : Prim
    , start : MarkAnchor
    , end : MarkAnchor
    }


{-| A mark range endpoint: a character `ref` (or `Nothing` = the very start/end of
the text) plus a `side` saying whether the boundary sits before or after that
character. The side controls **boundary expansion** — a start anchored `Before` S
and an end anchored `After` E make the mark grow as you type inside it (bold/italic),
whereas a non-expanding right edge (end `Before` the following char) does not
(`code`, `link`).
-}
type alias MarkAnchor =
    { ref : Maybe OpId
    , side : AnchorSide
    }


{-| Which side of its `ref` character a `MarkAnchor` binds to.

Isomorphic to `Rga.Side` (`Left`/`Right`) — both name a gap either side of an element —
and kept separate on purpose. `Rga.Side` is an **insertion** decision consumed by the
Fugue ordering walk: it says where a new element attaches in the sequence tree, and it is
meaningless outside that algorithm. This is a **range boundary** on a mark op, read long
after insertion to decide expansion, and Peritext marks are deliberately agnostic to how
the sequence is ordered (swapping Fugue for something else must not touch a mark). Sharing
one type would tie the two together and put an ordering-algorithm word (`Left`) where the
domain word is `Before`. They also encode differently on the wire (`"L"`/`"R"` vs
`"b"`/`"a"`), so collapsing them would not even shrink the format.

-}
type AnchorSide
    = Before
    | After


{-| One contribution to a counter: a signed `delta` tagged with the `OpId` of the
increment op that produced it. The counter's value is the sum of all deltas;
keying by `OpId` makes merge a `Dict.union` (each op is unique, so a shared key
carries an identical contribution) — idempotent, commutative, and a proper
PN-counter (concurrent `+1`/`+1` sum to 2, not LWW-collapse to 1).
-}
type alias Increment =
    { stamp : OpId
    , delta : Int
    }


{-| A map entry: a value plus an LWW presence cell. `present = False` is a key
tombstone; `stamp` is the last write to the presence bit.
-}
type alias Entry =
    { value : Node
    , present : Bool
    , stamp : OpId
    }


{-| An RGA whose elements carry `Node` content.
-}
type alias RgaNode =
    Rga Node


{-| An RGA element carrying `Node` content.
-}
type alias Element =
    Rga.Element Node


{-| A last-write-wins register: a primitive value tagged with the `OpId` that
last wrote it.
-}
type alias Register =
    { value : Prim
    , stamp : OpId
    }


{-| The primitive values a register can hold (the leaves of the JSON-like tree).

Spelled out rather than being a `Json.Decode.Value`, for two reasons that both bite:

1.  **`==` is this library's convergence oracle.** Two replicas agree iff their `Node`s
    are structurally equal — that is what every merge law, every delivery-order property
    and `Doc.cacheConsistent` assert. `Value` wraps an opaque JS object, so `==` on it is
    a deep JS comparison Elm makes no promises about (and the compiler cannot warn you):
    the one operator the whole test suite rests on would become untrustworthy.

2.  **JSON cannot tell `3` from `3.0`.** A `Value`-backed register would encode `PFloat 3`
    and `PInt 3` identically and decode both to whichever the reader guesses, so a float
    field would silently fail to round-trip. Tagging the constructor on the wire
    (`{"k":"float","x":3}` — see `Crdt.Json`) is what makes the format lossless, which
    `tests/OpJsonTests.elm` pins.

Arbitrary JSON still round-trips — `Crdt.Schema.Internal.register` (the `Crdt.custom`
escape hatch) stores a value as a `PString` of its serialization, so a user type of any
shape rides in a register while the tagging and the equality both keep working.

-}
type Prim
    = PNull
    | PBool Bool
    | PInt Int
    | PFloat Float
    | PString String



-- CONSTRUCTORS ---------------------------------------------------------------


{-| A register node from a primitive and the stamp that wrote it.
-}
reg : Prim -> OpId -> Node
reg value stamp =
    Reg { value = value, stamp = stamp }


{-| A map entry value with a presence stamp.
-}
entry : OpId -> Bool -> Node -> Entry
entry stamp present value =
    { value = value, present = present, stamp = stamp }


{-| A map from a dict of fully-formed entries.
-}
mapFromEntries : Dict String Entry -> Node
mapFromEntries =
    Map


{-| A sequence node.
-}
seq : RgaNode -> Node
seq =
    Seq


{-| A text node.
-}
txt : Rga String -> Node
txt =
    Txt


{-| A counter node from its per-op contributions.
-}
counter : Dict String Increment -> Node
counter =
    Cnt


{-| A single counter contribution.
-}
increment : OpId -> Int -> Increment
increment stamp delta =
    { stamp = stamp, delta = delta }


{-| A movable-list node.
-}
mov : MovNode -> Node
mov =
    Mov


{-| A movable-tree node.
-}
tree : TreeNode -> Node
tree =
    Tree


{-| A rich-text node.
-}
rich : RichNode -> Node
rich =
    Rich



-- ACCESSORS ------------------------------------------------------------------


{-| Extract a primitive value, if this is a register.
-}
asPrim : Node -> Maybe Prim
asPrim node =
    case node of
        Reg r ->
            Just r.value

        _ ->
            Nothing


{-| Extract the raw entries, if this is a map.
-}
asMap : Node -> Maybe (Dict String Entry)
asMap node =
    case node of
        Map d ->
            Just d

        _ ->
            Nothing


{-| The present (non-tombstoned) key/value pairs of a map, in key order.
-}
presentEntries : Node -> List ( String, Node )
presentEntries node =
    case node of
        Map d ->
            Dict.toList d
                |> List.filter (\( _, e ) -> e.present)
                |> List.map (\( k, e ) -> ( k, e.value ))

        _ ->
            []


{-| Extract the array, if this is a sequence.
-}
asSeq : Node -> Maybe RgaNode
asSeq node =
    case node of
        Seq r ->
            Just r

        _ ->
            Nothing


{-| Extract the array, if this is text.
-}
asTxt : Node -> Maybe (Rga String)
asTxt node =
    case node of
        Txt r ->
            Just r

        _ ->
            Nothing


{-| Extract the movable list, if this is one.
-}
asMov : Node -> Maybe MovNode
asMov node =
    case node of
        Mov ml ->
            Just ml

        _ ->
            Nothing


{-| Extract the movable tree, if this is one.
-}
asTree : Node -> Maybe TreeNode
asTree node =
    case node of
        Tree t ->
            Just t

        _ ->
            Nothing


{-| Extract the rich-text node, if this is one.
-}
asRich : Node -> Maybe RichNode
asRich node =
    case node of
        Rich r ->
            Just r

        _ ->
            Nothing


{-| The counter's current value: the sum of all its contributions. `Nothing` if
this node isn't a counter.
-}
asCounter : Node -> Maybe Int
asCounter node =
    case node of
        Cnt d ->
            Just (Dict.foldl (\_ inc acc -> acc + inc.delta) 0 d)

        _ ->
            Nothing


{-| The **canonical empty skeleton** of a node: the same kind, holding nothing — or
`Nothing` for the kinds that cannot be rebuilt from empty (below).

This exists for the one place a `Node` has to travel inside an op that **two replicas may
emit for the same slot**: a map key's creation (`SetKeyPresence`). A map key is the only
container slot without its own identity — an element/tree node is named by the id of the op
that inserted it, so no two ops ever create the same one, whereas two replicas that both
`setKey "k"` on an absent key are both creating _that_ key. Only the first to be folded
installs its seed (see `Crdt.OpLog.setKeyPresenceAt`), so if the seeds differ the state depends
on **arrival order** — and a merge that folds only the ops it has just added (the
incremental path every merge/decode takes) has no chance to re-decide later. The fix is for
every such op to carry a seed that is a function of the value's _shape_ alone, so whichever
one wins installs the identical thing and the value itself arrives as ordinary ops
afterwards (`Crdt.Doc.Internal.setKey`).

So: registers keep their primitive's _type_ but lose its value, and every collection is
emptied. Stamps become `Id.unwrittenStamp`, which loses to every real write — that is what
lets the follow-up ops fill the skeleton in.

Three kinds answer **`Nothing`** — they cannot be safely emptied: `Mov`, `Tree` and `Rich`.
The edit engine's diff (`Crdt.Doc.Internal.restoreNode`) has no case that rebuilds rich text
at all, and refilling a movable list / tree from empty re-mints every id, so emptying them
would trade a rare convergence edge for certain data loss. Callers ship those whole, as
before; a dict _of_ rich text can therefore still resolve two concurrent creations of one key
by arrival order. Nothing else can.

-}
vacate : Node -> Maybe Node
vacate node =
    case node of
        Reg r ->
            Just (Reg { value = vacatePrim r.value, stamp = Id.unwrittenStamp })

        Map _ ->
            Just (Map Dict.empty)

        Seq _ ->
            Just (Seq Rga.empty)

        Txt _ ->
            Just (Txt Rga.empty)

        Cnt _ ->
            Just (Cnt Dict.empty)

        Mov _ ->
            Nothing

        Tree _ ->
            Nothing

        Rich _ ->
            Nothing


{-| A primitive reduced to its type's zero, so a skeleton register reads as _something_
(rather than failing its schema on kind) until the write that fills it lands.
-}
vacatePrim : Prim -> Prim
vacatePrim prim =
    case prim of
        PNull ->
            PNull

        PBool _ ->
            PBool False

        PInt _ ->
            PInt 0

        PFloat _ ->
            PFloat 0

        PString _ ->
            PString ""


{-| Deep-copy a node with entirely fresh ids/stamps, building new sequence elements.

Used wherever content has to be **re-created rather than resurrected**: `Crdt.Doc`'s
`restoreTo` re-creating a key or element that was deleted since the target version, and
undo reviving a deleted subtree. Tombstones are permanent, so the copy necessarily has new
identity — see `reStampWithMap` when the caller needs the old → new id mapping.

-}
reStamp : Id.Ctx -> Node -> ( Node, Id.Ctx )
reStamp ctx node =
    let
        ( n, ctx1, _ ) =
            reStampWithMap ctx node
    in
    ( n, ctx1 )


{-| Like `reStamp`, but also returns the **old → new id mapping** for every
target-addressable id it re-mints: `Rga` element ids (`Seq`/`Txt`), `MoveList`
value ids, and `Tree` node ids. Register/counter stamps are LWW stamps addressed by
key (not by id), so they are not included.

Undo/redo needs this map: reviving a deleted subtree mints fresh ids, and a later
inverse op may still reference an id _inside_ that subtree (e.g. the character a
follow-up text insert anchored after). Registering the whole mapping — not just the
subtree root — keeps those references resolvable. See `Doc`'s `idRemap`.

-}
reStampWithMap : Id.Ctx -> Node -> ( Node, Id.Ctx, Dict String OpId )
reStampWithMap ctx node =
    case node of
        Reg r ->
            let
                ( stamp, ctx1 ) =
                    Id.nextId ctx
            in
            ( Reg { r | stamp = stamp }, ctx1, Dict.empty )

        Map entries ->
            let
                ( newEntries, ctx1, remap ) =
                    Dict.foldl
                        (\k e ( acc, c, m ) ->
                            let
                                ( v, c1, m1 ) =
                                    reStampWithMap c e.value

                                ( s, c2 ) =
                                    Id.nextId c1
                            in
                            ( Dict.insert k { value = v, present = e.present, stamp = s } acc, c2, mergeRemap m m1 )
                        )
                        ( Dict.empty, ctx, Dict.empty )
                        entries
            in
            ( Map newEntries, ctx1, remap )

        Seq r ->
            reStampRga Seq reStampWithMap ctx r

        Txt r ->
            -- a character carries no ids, so only the element ids are re-minted
            reStampRga Txt reStampInert ctx r

        Cnt d ->
            -- re-stamp each contribution with a fresh id; the sum is preserved.
            let
                ( newCnt, ctx1 ) =
                    Dict.foldl
                        (\_ inc ( acc, c ) ->
                            let
                                ( s, c1 ) =
                                    Id.nextId c
                            in
                            ( Dict.insert (Id.opIdToString s) (increment s inc.delta) acc, c1 )
                        )
                        ( Dict.empty, ctx )
                        d
            in
            ( Cnt newCnt, ctx1, Dict.empty )

        Mov ml ->
            -- rebuild a fresh movable list from the old visible order, minting a
            -- new valueId + cell per item (content deep-restamped too).
            let
                ( rebuilt, ctx1, remap ) =
                    MoveList.toEntries ml
                        |> List.foldl
                            (\( oldVid, childOld ) ( acc, c, ( afterCell, m ) ) ->
                                let
                                    ( child, c1, m1 ) =
                                        reStampWithMap c childOld

                                    ( vid, c2 ) =
                                        Id.nextId c1
                                in
                                ( MoveList.insert vid afterCell child acc
                                , c2
                                , ( Just vid, mergeRemap (Dict.insert (Id.opIdToString oldVid) vid m) m1 )
                                )
                            )
                            ( MoveList.empty, ctx, ( Nothing, Dict.empty ) )
                        |> (\( acc, c, ( _, m ) ) -> ( acc, c, m ))
            in
            ( Mov rebuilt, ctx1, remap )

        Tree t ->
            let
                ( rebuilt, ctx1, remap ) =
                    Tree.reStamp reStampWithMap ctx t
            in
            ( Tree rebuilt, ctx1, remap )

        Rich r ->
            -- re-stamp the char sequence (getting old→new id map), then rebuild the
            -- marks with fresh op ids and anchor refs remapped through that map, so
            -- a revived mark still covers the revived characters.
            let
                ( newText, ctx1, textRemap ) =
                    Rga.reStamp reStampInert ctx r.text

                remapAnchor a =
                    { a | ref = Maybe.map (remapId textRemap) a.ref }

                ( newMarks, ctx2 ) =
                    Dict.foldl
                        (\_ m ( acc, c ) ->
                            let
                                ( newId, c1 ) =
                                    Id.nextId c

                                m1 =
                                    { m | id = newId, start = remapAnchor m.start, end = remapAnchor m.end }
                            in
                            ( Dict.insert (Id.opIdToString newId) m1 acc, c1 )
                        )
                        ( Dict.empty, ctx1 )
                        r.marks
            in
            ( Rich { text = newText, marks = newMarks }, ctx2, textRemap )


{-| Resolve an id through a remap table (used when re-stamping mark anchors).
Falls back to the original id if it isn't in the map.
-}
remapId : Dict String OpId -> OpId -> OpId
remapId table id =
    Dict.get (Id.opIdToString id) table |> Maybe.withDefault id


{-| Union of two id-remap maps (later overrides on key clash; keys are globally
fresh so clashes don't occur in practice).
-}
mergeRemap : Dict String OpId -> Dict String OpId -> Dict String OpId
mergeRemap a b =
    Dict.union b a


{-| `Rga.reStamp` with the result wrapped back into whichever sequence variant it came
from. The walk itself lives in `Crdt.Rga`, next to the ordering rules it depends on
(`compactTombstones` is the same shape); all this module contributes is what the content
re-stamps to.
-}
reStampRga : (Rga c -> Node) -> (Id.Ctx -> c -> ( c, Id.Ctx, Dict String OpId )) -> Id.Ctx -> Rga c -> ( Node, Id.Ctx, Dict String OpId )
reStampRga wrap reStampContent ctx rga =
    let
        ( rebuilt, ctx1, remap ) =
            Rga.reStamp reStampContent ctx rga
    in
    ( wrap rebuilt, ctx1, remap )


{-| A content re-stamper for content that holds **no ids** — a text character, a rich-text
element. There is nothing to re-mint and nothing to add to the remap table; only the element
ids around it change, and `Rga.reStamp` handles those.
-}
reStampInert : Id.Ctx -> c -> ( c, Id.Ctx, Dict String OpId )
reStampInert ctx content =
    ( content, ctx, Dict.empty )



-- CLOCK CATCH-UP -------------------------------------------------------------


{-| The largest Lamport counter referenced anywhere in the tree, used to advance
a replica's clock after a merge so it never re-mints a seen id.
-}
maxCounter : Node -> Int
maxCounter node =
    case node of
        Reg r ->
            Id.opIdCounter r.stamp

        Map d ->
            Dict.foldl
                (\_ e acc -> max acc (max (Id.opIdCounter e.stamp) (maxCounter e.value)))
                0
                d

        Seq rga ->
            rgaMaxCounter maxCounter rga

        Txt rga ->
            -- element ids only: a character has no stamp of its own to clear
            rgaMaxCounter (always 0) rga

        Cnt d ->
            Dict.foldl (\_ inc acc -> max acc (Id.opIdCounter inc.stamp)) 0 d

        Mov ml ->
            MoveList.maxCounter maxCounter ml

        Tree t ->
            Tree.maxCounter maxCounter t

        Rich r ->
            max (rgaMaxCounter (always 0) r.text) (marksMaxCounter r.marks)


{-| Largest counter referenced by a mark set — each op's own id plus its anchor
`ref`s (which point at char ids that may themselves be higher than the op id).
-}
marksMaxCounter : Dict String MarkOp -> Int
marksMaxCounter marks =
    Dict.foldl
        (\_ m acc ->
            let
                anchorMax a =
                    a.ref |> Maybe.map Id.opIdCounter |> Maybe.withDefault 0
            in
            List.foldl max acc [ Id.opIdCounter m.id, anchorMax m.start, anchorMax m.end ]
        )
        0
        marks


rgaMaxCounter : (c -> Int) -> Rga c -> Int
rgaMaxCounter contentMax rga =
    List.foldl
        (\el acc -> max acc (contentMax el.content))
        (Rga.maxCounter rga)
        (Rga.elements rga)


{-| **Physically drop settled tombstones** from the sequence/text RGAs in this tree,
recursing structurally. `Seq`, `Txt`, and rich-text `.text` are rebuilt from their live
elements as a right-spine (see `Rga.compactTombstones`) — the visible value is byte-for-byte
identical, element ids survive (cursors keep resolving), and the dead tombstones are gone.
Nested sequences inside surviving elements (and inside map entries / movable-list values /
tree payloads) are compacted too.

**Only sound below a stable cut** every replica has incorporated — dropping a tombstone an
incoming op still anchors after would dangle it (see `Rga.compactTombstones` /
`design-docs/04-gc.md`). The caller (`Crdt.Doc.compact`) owns that.

Rich text needs more than the RGA pass, because its **marks** anchor to character ids:
see `compactRich`.

v1 scope: it does not yet prune a movable-list's or tree's own tombstone **sets** (deleted
valueIds / nodeIds), only the RGA `.deleted` tombstones of `Seq`/`Txt`/`Rich` — which the
footprint benchmark shows dominate. Nested content is still recursed into.

-}
compactTombstones : Node -> Node
compactTombstones node =
    case node of
        Reg _ ->
            node

        Cnt _ ->
            node

        Map d ->
            Map (Dict.map (\_ e -> { e | value = compactTombstones e.value }) d)

        Seq rga ->
            Seq (Rga.compactTombstones compactTombstones rga)

        Txt rga ->
            -- nothing to recurse into: the content is a character
            Txt (Rga.compactTombstones identity rga)

        Rich r ->
            Rich (compactRich r)

        Mov ml ->
            Mov (MoveList.mapValues compactTombstones ml)

        Tree t ->
            Tree (Tree.mapPayloads compactTombstones t)


{-| Compact a rich-text node: drop the character tombstones **and re-anchor the marks
onto survivors**.

The RGA pass alone is not enough here. A `MarkOp` names its endpoints by character id,
and the cover logic resolves them against the _tombstone-inclusive_ element order
(`Crdt.RichText.Internal.boundaryPos`), so an anchor left pointing at a dropped tombstone
resolves to nothing and its mark stops covering **everything** — including the characters
that are still alive. Bold a word, delete its first letter, compact: the bold would
vanish from the rest of the word.

The rewrite is **coverage-preserving**, so the read model is unchanged. Only an endpoint
that named a _dropped tombstone_ moves; a `Nothing` ref (±∞, the text's own start/end —
this is also what a leading block-type marker uses) and an endpoint naming a survivor are
left exactly as they were, since they already mean the same thing before and after. A
moved start becomes `Before` the first survivor the old boundary covered, a moved end
`After` the last, which is the same survivor set by construction (compaction preserves
the relative order of survivors). A mark that covered no survivor is **dropped**: none of
its characters exist any more, so nothing can consult it again, and keeping it would mean
keeping their tombstones forever.

The one behaviour that does change is for an op that anchors _into_ the dropped region —
a character inserted immediately after a dead tombstone would have been covered by a mark
whose boundary sat there. Such an op is exactly what `compactTombstones`'s stable-cut
requirement already excludes (`design-docs/04-gc.md`).

-}
compactRich : RichNode -> RichNode
compactRich r =
    let
        indexed =
            Rga.toElementsInOrder r.text |> List.indexedMap Tuple.pair

        -- position of every element, tombstones included: the index space the mark
        -- boundaries are expressed in before compaction.
        posOf =
            indexed
                |> List.map (\( i, el ) -> ( Id.opIdToString el.id, i ))
                |> Dict.fromList

        survivors =
            indexed
                |> List.filterMap
                    (\( i, el ) ->
                        if el.deleted then
                            Nothing

                        else
                            Just ( i, el.id )
                    )

        liveIds =
            survivors |> List.map (\( _, id ) -> Id.opIdToString id) |> Set.fromList
    in
    { text = Rga.compactTombstones identity r.text
    , marks =
        Dict.foldl
            (\key m acc ->
                case reAnchorMark posOf liveIds survivors m of
                    Just m1 ->
                        Dict.insert key m1 acc

                    Nothing ->
                        acc
            )
            Dict.empty
            r.marks
    }


{-| Re-anchor one mark for `compactRich`, or `Nothing` to drop it (it covers no
surviving character). A mark neither of whose endpoints named a dropped tombstone comes
back untouched.
-}
reAnchorMark : Dict String Int -> Set String -> List ( Int, OpId ) -> MarkOp -> Maybe MarkOp
reAnchorMark posOf liveIds survivors m =
    let
        -- True only for an endpoint whose `ref` is a tombstone this pass drops. An
        -- absent ref (the character has not arrived yet) is left alone: it may still
        -- turn up, and until it does the mark covers nothing either way.
        moves anchor =
            case anchor.ref of
                Nothing ->
                    False

                Just id ->
                    let
                        key =
                            Id.opIdToString id
                    in
                    Dict.member key posOf && not (Set.member key liveIds)
    in
    if not (moves m.start) && not (moves m.end) then
        Just m

    else
        case ( boundaryOf posOf m.start, boundaryOf posOf m.end ) of
            ( Just s, Just e ) ->
                let
                    covered =
                        survivors |> List.filter (\( i, _ ) -> s < toFloat i && toFloat i < e)
                in
                case ( List.head covered, List.head (List.reverse covered) ) of
                    ( Just ( _, firstId ), Just ( _, lastId ) ) ->
                        Just
                            { m
                                | start =
                                    if moves m.start then
                                        { ref = Just firstId, side = Before }

                                    else
                                        m.start
                                , end =
                                    if moves m.end then
                                        { ref = Just lastId, side = After }

                                    else
                                        m.end
                            }

                    _ ->
                        -- covers no survivor: the mark is dead
                        Nothing

            _ ->
                -- an endpoint we cannot place (its character is not here at all); leave
                -- the mark as it stands rather than guess
                Just m


{-| The boundary position of a mark anchor in the tombstone-inclusive order: just
before/after its `ref` (a half-integer, so no element sits exactly on a boundary), or
±∞ for a `Nothing` ref. Must agree with `Crdt.RichText.Internal.boundaryPos`, which is
the same function over the read path.
-}
boundaryOf : Dict String Int -> MarkAnchor -> Maybe Float
boundaryOf posOf anchor =
    case anchor.ref of
        Nothing ->
            case anchor.side of
                Before ->
                    Just (-1 / 0)

                After ->
                    Just (1 / 0)

        Just id ->
            Dict.get (Id.opIdToString id) posOf
                |> Maybe.map
                    (\i ->
                        case anchor.side of
                            Before ->
                                toFloat i - 0.5

                            After ->
                                toFloat i + 0.5
                    )
