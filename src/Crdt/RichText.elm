module Crdt.RichText exposing
    ( Span, MarkValue(..)
    , Block
    )

{-| The values you read out of a **rich (formatted) text** field — the collaborative
analogue of a document with bold, links, headings and lists.

You never construct these yourself. You describe a rich-text field with `Crdt.richText`,
edit it through `Crdt.Edit` (type into it with `Crdt.Edit.setRich` /
`Crdt.Edit.setBlockText`, turn formatting on and off with `Crdt.Edit.mark`/`Crdt.Edit.unmark`,
or restructure it with `Crdt.Edit.splitBlock` and friends), and
read it back as the shapes below:

  - `Crdt.Doc.read` decodes a rich-text field to a flat `List Span` — the text broken into
    runs, each run tagged with the formatting active over it. This is enough to render
    inline formatting (bold, italic, links).
  - `Crdt.Edit.readBlocks` gives you a `List Block` — the same text grouped into the
    document's paragraphs, headings and list items, each with its own spans. Use this
    when your editor has block structure.

Formatting is stored as **marks** anchored to individual characters, not to positions,
so it survives concurrent editing: if one person bolds a word while another inserts
text inside it, the bold still covers the right characters after the two edits merge.
When two people format the same character at the same time, the most recent edit wins
for that character.

@docs Span, MarkValue

@docs Block

-}

import Crdt.Id
import Dict exposing (Dict)


{-| One run of text that shares a single set of active formatting marks.

`marks` maps a mark's name (the string you passed to `Crdt.Edit.mark`, e.g. `"bold"` or
`"link"`) to the value it carries over this run. A name being present means that mark
is _on_ for every character in `text`; a name being absent means it is off.

Rendering a `List Span` is a matter of wrapping each run's `text` in whatever your
view uses for the marks it carries:

    viewSpan : Span -> Html msg
    viewSpan span =
        let
            wrap name value html =
                case name of
                    "bold" ->
                        Html.strong [] [ html ]

                    "link" ->
                        case value of
                            RichText.Value href ->
                                Html.a [ Html.Attributes.href href ] [ html ]

                            RichText.Flag ->
                                html

                    _ ->
                        html
        in
        Dict.foldl wrap (Html.text span.text) span.marks

-}
type alias Span =
    { text : String
    , marks : Dict String MarkValue
    }


{-| What a single mark carries where it is active.

  - `Flag` — an on/off mark that carries no data, like bold or italic. Its presence in
    a span's `marks` is all the information there is.
  - `Value String` — a mark that carries a string, like a link's `href` or a highlight
    colour.

You choose which kind a mark is when you apply it over a range — `Crdt.Edit.mark ref from to
"bold" RichText.Flag doc` versus `Crdt.Edit.mark ref from to "link" (RichText.Value "https://…")
doc`. A mark that is not active over a span is simply absent from its `marks` — there is
no "off" value to match on.

-}
type MarkValue
    = Flag
    | Value String


{-| One block of the document — a paragraph, heading or list item — as produced by
`Crdt.Edit.readBlocks`.

  - `type_` is the app-defined kind of block, the string you set with
    `Crdt.Edit.setBlockType` (for example `"h1"`, `"blockquote"` or `"ul"`). An empty string
    is the default block (a plain paragraph). The library never interprets this value;
    your editor decides what the vocabulary means and how to render it.
  - `depth` is the indent level, starting at `0`, changed with `Crdt.Edit.indentBlock` and
    `Crdt.Edit.outdentBlock`.
  - `spans` is the block's inline content, exactly as `Crdt.Doc.read` gives them — so
    the same `viewSpan` renders both.
  - `marker` is an internal identity for the block (`Nothing` for the leading block,
    which has no marker element), used by the block-editing functions in `Crdt.Edit`.
    You will rarely read it directly.

-}
type alias Block =
    { marker : Maybe Crdt.Id.OpId
    , type_ : String
    , depth : Int
    , spans : List Span
    }
