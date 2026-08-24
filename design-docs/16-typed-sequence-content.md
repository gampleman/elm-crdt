# 16 — Typed sequence content: getting `Node` out of text elements

**Status:** ✅ **done.** Raised in code review against `Crdt.Node`'s `Txt RgaNode` ("why is
`Txt` an `Rga Node`? surely that opens the door to odd bugs?"). It did — three of them, all
now closed:

```elm
type Node = … | Seq (Rga Node) | Txt (Rga String) | … | Rich RichNode

type alias RichNode = { text : Rga RichElem, marks : Dict String MarkOp }

type RichElem  = TextChar String | Token BlockToken
type BlockToken = Marker | Nest
```

- a text element can no longer hold anything but a character, so the read cannot silently
  drop one (it used to `filterMap` it away — identically on every replica, so no convergence
  test could see the loss);
- a character no longer carries an `OpId` stamp nothing reads: **−40% on the state wire
  encoding, −21% live heap** (measured — `benchmarks/results/BASELINE.md`);
- `Rich`'s three element kinds are a checked sum type instead of magic `PInt` tags with a
  synthetic `0@""` stamp.

Inserting a block token became its own action, `InsertToken` (wire tag `tok`), since an `ins`
seed is a whole node and a rich element never is. `Crdt.Doc.ReadError` paths, the
single-character check at the decoder edge, and `tests/ReadErrorTests.elm` cover the rest.

**Wire format broke, deliberately** — free before `1.0.0` (see `../ROADMAP.md`, "Decisions
recorded"), and `tests/OpJsonTests.elm`'s byte pin was re-pinned as the record of it.

**Related:** [`09-fugue.md`](09-fugue.md) (the ordering all five containers still share),
[`10-rich-text.md`](10-rich-text.md) and [`11-block-structure.md`](11-block-structure.md)
(where the pressure came from).

## The situation

`Crdt.Rga` is polymorphic in its element content (`Rga c`) — deliberately, and it earns it:
the ordering walk, tombstone rules and `compactTombstones` are the same algorithm whatever
the elements hold. Every container instantiates it at exactly one type — and they used to all be the same one:

```elm
type Node = … | Seq RgaNode | Txt RgaNode | … | Rich RichNode
type alias RgaNode  = Rga Node
type alias RichNode = { text : RgaNode, marks : Dict String MarkOp }
```

For `Seq` that is right: a list element genuinely holds an arbitrary document (a record, a
nested list, another text). For `Txt` and `Rich` it was a lie the type system could not catch.
A text element is supposed to hold **one character**, and the representation of that was
`Reg (PString "a")` plus a stamp — a full LWW register per character.

## Three symptoms, in increasing order of seriousness

All three are fixed; this records what they were and how each was closed.

**1. The read dropped what it did not understand.** `Crdt.Text.toString` used to be:

```elm
Rga.toList rga
    |> List.filterMap (\node -> case Node.asPrim node of
                                    Just (PString s) -> Just s
                                    _               -> Nothing)
    |> String.concat
```

An element whose content is a map, a counter, or a `PInt` was **silently skipped**. So an op
that inserts a non-character into a text container — a schema mismatch, a mangled relay, a
malicious peer, or simply our own bug — produced a document that read as if the op had never
arrived, and read that way *identically on every replica*, so convergence testing could not
see it either. Everywhere else in the library a shape the reader does not expect is a
`ReadError` naming its location (see [`13-migrations.md`](13-migrations.md) and the paths in
`Crdt.Schema.Internal`); text was the one place it was a shrug.

### Where the rejection belongs: the decoder, not the read, and never the fold

This landed in two steps, and the intermediate step is worth recording because the reasoning
that ruled out the fold still holds.

**Not the fold.** `OpLog.applyOp` is total by construction, and by the time it runs the op is
already in the store. A fold-side refusal could therefore only (a) skip the op — the same
silent drop, moved earlier and now invisible to the read as well, or (b) make `applyOp`
fallible, which means every future materialization of that document fails forever because one
peer sent one bad op. Neither is an improvement.

**First: the read.** `Crdt.Text.read` returned a `Result` naming the offending element, and
`RichText.unexpectedElement` did the same over the wider rich vocabulary, both surfacing as a
`Crdt.Doc.ReadError` with the location path [`13`](13-migrations.md) added. Loud, located, and
recoverable — deleting the element fixed it, tombstoned elements were excluded so a peer that
cleaned up healed the document, and the read stayed a pure function of state. This shipped
first because it needed no format change.

**Now: the decoder.** With the content typed, the check moved to where untrusted bytes enter
(`Crdt.Json.charDecoder` and `richElemDecoder`) and both read paths went back to being total:
`Text.read` is `Rga.toList >> String.concat`, and `unexpectedElement` is gone. A node that
cannot hold a non-character cannot be built from bytes that describe one, so the failure
happens once, at the edge, instead of on every read.

The one invariant a `String` content type can still violate is length, so that is what the
decoder checks: **exactly one `Char`**, not merely "a string". One element per character is
what keeps `Doc.Internal.applyCharDiff`'s id array aligned one-to-one with the text, so a
two-character element would silently shift every cursor and diff position after it — a
quieter second bug class a loose check would let through. Counted in code points rather than
`String.length`, so an emoji is one character, matching how every writer splits
(`Text.fromString`, `OpLog.insertTextRun`, `RichText.fromSpans`).

**One hazard found while doing it.** `insertElem`'s "create the container" fallback would have
turned an existing `Txt` into an empty `Seq` when an `ins` op (whose seed is a node) named a
text field — destroying the text rather than ignoring the op. `Txt`/`Rich` now match
explicitly and return the node untouched, and `tests/ReadErrorTests.elm` pins that such an op
is **inert**. The mirror case is gated instead: `InsertToken` aimed at a non-rich container
pends via `canApply` rather than silently vanishing, like `AddMark`.

**2. Every character paid for a stamp it did not use.** `Crdt.Text.charNode` minted a fresh
`OpId` per character purely so the document never held a duplicate id — and then nothing
read it: ordering comes from the *element* id, and a character is never overwritten in
place (an edit is an insert plus a delete). So each char carries a `(counter, replica)` pair
that exists only to satisfy the register shape. The footprint work already found that
per-op `OpId` metadata dominates memory and wire size, and this is a whole redundant id per
character of every text field. On the wire it is a `{"t":"reg","v":{"k":"string","x":"a"},"s":[7,"alice"]}`
per character where `"a"` would do.

**3. `Rich` had outgrown the shape entirely, and was using it as an untyped tagged union.**
A rich-text sequence interleaves three *different kinds* of element: characters,
block-boundary markers, and indent (nest) tokens ([`11`](11-block-structure.md)). With only
`Node` to express that, `Crdt.RichText.Internal` encoded the tags as magic integers in a
register with a dummy stamp:

```elm
markerTag    = 0
nestTokenTag = 1

markerNode    = Node.reg (PInt markerTag)    (Id.opId 0 (Id.replica ""))
nestTokenNode = Node.reg (PInt nestTokenTag) (Id.opId 0 (Id.replica ""))

isMarker    node = Node.asPrim node == Just (PInt markerTag)
isNestToken node = Node.asPrim node == Just (PInt nestTokenTag)
```

That is a sum type, written out by hand, with no exhaustiveness anywhere and a synthetic
`0@""` id standing in for a stamp that has no meaning. `Doc.Internal.charElemOf` then
re-derives "is this a character?" by pattern-matching `PString`, and `cursorIds` exists
purely because *some* elements are not characters while every offset a caller supplies
counts characters only — a distinction the type could have made instead of a comment. The
one bug this shape produced is on record: counting markers as characters drifted the caret one
position per preceding block boundary.

## The fix, as built

Instantiate `Rga` at a content type per container flavour, and let the codec enforce it:

```elm
type Node
    = …
    | Seq  (Rga Node)        -- unchanged: a list element really is any document
    | Txt  (Rga String)      -- one grapheme (today: one Char) per element
    | Rich (RichNode)

type alias RichNode =
    { text : Rga RichElem, marks : Dict String MarkOp }

type RichElem  = TextChar String | Token BlockToken
type BlockToken = Marker | Nest
```

(`Token BlockToken` rather than three flat cases because an op inserting a marker carries the
choice with no character to go with it — see `InsertToken` below.)

What fell out:

- `Text.read` is `Rga.toList >> String.concat` — total by construction, nothing to validate
  and nothing to drop.
- `isMarker`/`isNestToken`/`charOf` are `case` branches the compiler checks; the magic tags
  and the `0@""` stamp are gone.
- `cursorIds` still exists (offsets count characters, and a rich sequence has non-characters)
  but its predicate stopped being a guess over `Prim`.
- `Node.maxCounter`, `reStamp` and `compactTombstones` got *simpler* for these two variants:
  no nested content to recurse into, no stamp to carry (`reStampInert`, `identity`).
- The `txt` element encoding is `"c":"a"` instead of a whole register node.

`Rga`'s own polymorphism is what made this cheap — the ordering code did not change at all.
The same argument does **not** apply to `Mov` or `Tree`: their values genuinely are arbitrary
documents, so `MoveList Node` / `Tree Node` are already right.

**What did not survive the split.** `Doc.Internal.seqRga` used to hand back "the `Rga Node`
inside a `Seq`, `Txt` or `Rich`", and four call sites leaned on it. With three content types
it cannot exist, and unpicking it was most of the work — each caller turned out to want
something different, which is the tell that the shared accessor was hiding a real distinction:

- `orderedIds` wanted **ids only**, so it enumerates the four variants and calls the
  polymorphic `Rga.visibleIds` on each;
- `cursorOffset` wanted the anchor's **character offset**, where rich text counts characters
  and the others count everything — now `offsetOfAnchor`, a `case` instead of a predicate
  built from a runtime type test;
- `elementContent` (undo's re-insert) wanted the element's content as a `Node`, which for text
  does not exist — it returns a typed `ElemContent` (`DocElem`/`CharElem`/`TokenElem`) and
  `applyRev` emits the matching op per kind, so undoing a deleted character or block marker
  still works;
- `navigateTarget`/`walk` wanted to **descend into** an element, which a character has no
  interior for — those cases are gone, and an `Index` path into text is now an error rather
  than a silently wrong resolve.

## What it cost

**The wire format changed** for `txt` and `rich` nodes, plus one new action. Free before
1.0.0 — the format is unversioned, nothing is deployed, and
[`10-rich-text.md`](10-rich-text.md) took a clean break on the same grounds
(see `../ROADMAP.md`, "Decisions recorded"). The practical consequence was one failing test,
`tests/OpJsonTests.elm`'s byte pin, re-pinned on purpose: exactly what that test is for (see
[`02-oplog.md`](02-oplog.md), "The wire format is now pinned").

**`Action` got more specific.** `InsertElem` carries `seed : Node`, so it now belongs to
`Seq`/`Mov` alone; block structure moved to a new `InsertToken` (wire tag `tok`) carrying a
`BlockToken`. `InsertText` — the run form, which is what real typing emits — already carried a
plain `String` and needed no change, which is also why reviving a deleted character on undo
goes through a one-character `InsertText` (`emitTextRun`) rather than an insert-element op.

**Tests.** The `OpJson` byte pin re-pinned; `Helpers`' element fuzzers split per content type
(strictly better — they used to generate arbitrary `Node` content for text elements, i.e. they
fuzzed states the representation now makes unrepresentable, while missing the malformed
*token* arrangements block reads actually have to tolerate); `ReadErrorTests`' vocabulary group
moved from "the read rejects this" to "the codec rejects this".

Two findings fell out of the test pass:

- **`DictTests` had a documented limitation that this fixes.** A text field nested in a record
  was rewritten by the id-matching element diff, so writing `"dark"` over `"dark"` deleted four
  characters and inserted four fresh ones — converging, but on `"darklight"` rather than
  `"light"` when a peer edited concurrently. A `Txt` element holds a character, so there is no
  element diff to fall into: `restoreNode` diffs the **string**, the same way `setText` does.
  The test said to retire it if this ever read `"light"`; it does, and it was.
- **`OpJsonTests`' exhaustiveness test did not work.** It claimed "a new `Action` fails to
  compile in `sampleOps`", which a list of constructor applications never does — `InsertToken`
  duly slipped in unfuzzed and unpinned. There is now a `kindOf : Action -> String` with an
  exhaustive `case` as the actual tripwire.

## Measured

`benchmarks/results/BASELINE.md` has the numbers. Summary: the **state** encoding (the only
payload the change touches — a compacted snapshot, sent to a peer behind our compaction
boundary) drops **38–44%** on text-bearing documents, a per-element 120 → 72 bytes for rich
text and 122 → 67 for plain; live heap for a 2000-character document drops **21%**; `text`
build latency roughly halves and nothing else moves outside run-to-run noise.

The op encoding is **unchanged** — `itxt` already carried a plain string — so ordinary
delta sync sees no difference. `run-wire.js` gained a snapshot column, because the state
encoding was previously unmeasured by the harness entirely.
