// A <crdt-richtext> custom element: a TipTap (ProseMirror) editor whose document is
// owned by Elm's CRDT. The element is a *view + input device* only — it never holds
// authoritative state:
//
//   • Elm pushes the current document down as `docBlocks` (a property) and via the
//     `renderRichText` port; the element reconciles its ProseMirror doc to match.
//   • User edits/format/block commands are reported up as `richtext-input`
//     CustomEvents, which index.js forwards to Elm's `richTextInput` port. Elm turns
//     them into CRDT ops (setRich / mark / split / merge / setType / indent /
//     outdent), which converge, then flow back down.
//
// Block model (see docs/11): the CRDT is a flat list of blocks, each { type, depth,
// spans }. We render each block as a SINGLE top-level ProseMirror node (paragraph /
// heading / blockquote, or a paragraph visually styled as a list item), with depth
// applied as CSS indentation via a `data-depth` attribute — deliberately NOT PM's
// genuinely-nested bullet_list/list_item trees. That keeps the PM↔CRDT position
// mapping one-block-to-one-block (no nested-list position arithmetic), which is what
// makes the binding robust; the CRDT fully supports nesting, the editor just renders
// it flat-with-indent. Type/depth are block attributes the CRDT stores opaquely.
//
// Loop avoidance: a doc pushed *down* is applied with a transaction meta flag, so the
// resulting ProseMirror transaction is not echoed back up as user input.

import { Editor } from "@tiptap/core";
import { TextSelection } from "@tiptap/pm/state";
import Document from "@tiptap/extension-document";
import Paragraph from "@tiptap/extension-paragraph";
import Text from "@tiptap/extension-text";
import Bold from "@tiptap/extension-bold";
import Italic from "@tiptap/extension-italic";
import Underline from "@tiptap/extension-underline";
import Strike from "@tiptap/extension-strike";
import Code from "@tiptap/extension-code";
import Link from "@tiptap/extension-link";

// A meta key marking a transaction as "applied from Elm", so onTransaction skips it.
const FROM_ELM = "crdtFromElm";

// The block type strings the CRDT stores (opaque to the library; this is the demo's
// vocabulary). "" = default paragraph.
const BLOCK_PARAGRAPH = "";

// Our single block node: a `block` node holding inline content, carrying `blockType`
// (opaque CRDT string) and `depth` (indent). We render it as the right HTML tag and
// tag it with data-* so CSS can style headings/quotes/list items + indentation, and
// so we can read the type/depth back off the DOM if needed.
const Block = Paragraph.extend({
  name: "block",
  addAttributes() {
    // Parse the attributes back FROM the DOM (data-type / data-depth) as well as
    // rendering them. The very first editor content is built as an HTML string (the
    // schema doesn't exist yet at `new Editor({content})`), so without these parse
    // rules the initial parse would drop blockType/depth to their defaults and the
    // document would load unformatted until the next edit re-rendered through the
    // schema. See `renderHTML` for the matching output.
    return {
      blockType: {
        default: BLOCK_PARAGRAPH,
        parseHTML: (el) => el.getAttribute("data-type") || BLOCK_PARAGRAPH,
        renderHTML: (attrs) => ({ "data-type": attrs.blockType || BLOCK_PARAGRAPH }),
      },
      depth: {
        default: 0,
        parseHTML: (el) => parseInt(el.getAttribute("data-depth") || "0", 10) || 0,
        renderHTML: (attrs) => ({ "data-depth": String(attrs.depth || 0) }),
      },
    };
  },
  parseHTML() {
    return [{ tag: "div.block" }];
  },
  renderHTML({ node }) {
    const t = node.attrs.blockType || BLOCK_PARAGRAPH;
    return [
      "div",
      {
        class: "block",
        "data-type": t,
        "data-depth": String(node.attrs.depth || 0),
      },
      0,
    ];
  },
});

const BlockDocument = Document.extend({ content: "block+" });

class CrdtRichText extends HTMLElement {
  constructor() {
    super();
    this.editor = null;
    this._pendingBlocks = null;
    // The block snapshot we diff the NEXT user transaction against, to derive intents.
    // It must track the editor's state as of the last thing we told Elm — NOT just the
    // last blocks Elm pushed down (`_pendingBlocks`). A local structural edit (Enter/
    // Backspace) changes the editor without any re-render, so if we diffed against
    // `_pendingBlocks` the baseline would go stale: the next keystroke would then see a
    // block-count mismatch and fall back to a non-commuting `reconcile` (which broke
    // concurrent editing). We refresh `_baseline` after every emit and every applied
    // remote change, so each transaction is diffed against the immediately prior state
    // and classifies as the precise, commutative op (split/merge/text).
    this._baseline = null;
    // Where to place the caret after the NEXT block re-render, as a block-relative
    // {blockIndex, charOffset}. Set when we emit a local structural intent (split /
    // merge / indent / setType) whose result Elm pushes back — because that rebuild
    // replaces the whole PM doc and would otherwise drop the caret to the end. Null =
    // no explicit target, so a re-render just preserves the current caret position.
    this._desiredCaret = null;
  }

  connectedCallback() {
    const mount = document.createElement("div");
    mount.className = "pm-mount";
    this.appendChild(mount);

    this.editor = new Editor({
      element: mount,
      extensions: [
        BlockDocument,
        Block,
        Text,
        Bold,
        Italic,
        Underline,
        Strike,
        Code,
        Link.configure({ openOnClick: false }),
      ],
      content: blocksToDoc(this._pendingBlocks || [{ type: "", depth: 0, spans: [] }]),
      onTransaction: ({ editor, transaction }) => {
        if (transaction.getMeta(FROM_ELM)) return; // don't echo Elm-driven changes
        this._emitTransaction(editor, transaction);
      },
    });

    // seed the diff baseline with the initial content (before any user edit)
    this._baseline = currentEditorBlocks(this.editor);

    // block/format keyboard behavior (Enter split, Backspace merge, Tab indent) is
    // installed via a plain DOM keydown handler so we control the exact intents.
    this._keydown = (e) => this._onKeydown(e);
    mount.addEventListener("keydown", this._keydown, true);

    this._toolbar = buildToolbar(this);
    this.insertBefore(this._toolbar, mount);
  }

  disconnectedCallback() {
    if (this.editor) {
      this.editor.destroy();
      this.editor = null;
    }
  }

  set docBlocks(blocks) {
    this._pendingBlocks = blocks;
    if (this.editor) this._applyBlocks(blocks);
  }

  get docBlocks() {
    return this._pendingBlocks;
  }

  // Reconcile the PM doc to `blocks` without echoing back to Elm. Skip when the
  // editor already shows these exact blocks (so we don't stomp the caret on the
  // round-trip of every keystroke). Replacing the doc content drops the selection to
  // the end, so we compute a block-relative caret to restore BEFORE the replace and
  // re-apply it after — using the explicit `_desiredCaret` from a local structural
  // intent when present, else the caret's current block-relative position (for remote
  // edits, so a peer's change doesn't fling our cursor to the end).
  _applyBlocks(blocks) {
    if (!this.editor) return;
    if (blocksEqual(currentEditorBlocks(this.editor), blocks || [])) {
      // nothing to re-render; drop any pending caret target so it can't leak into a
      // later, unrelated re-render.
      this._desiredCaret = null;
      // still refresh the diff baseline — the editor now matches these blocks.
      this._baseline = currentEditorBlocks(this.editor);
      return;
    }

    const { state, view } = this.editor;
    // Only restore/steal the caret when THIS editor is the one being edited — i.e. it
    // has focus (a local structural edit round-tripping through Elm), or we have an
    // explicit `_desiredCaret` from such an edit. For a purely REMOTE update on an
    // unfocused editor we must NOT touch focus or selection: forcing focus + a default
    // block-0 caret onto a peer stole focus and left ProseMirror's internal selection
    // at block 0, so the user's next click didn't take and their Enter split the wrong
    // block (deterministic, even without a race).
    const active = this._desiredCaret != null || view.hasFocus();
    const caret = active ? this._desiredCaret || this._currentBlockCaret() : null;
    this._desiredCaret = null;

    const tr = state.tr.setMeta(FROM_ELM, true);
    const doc = blocksToDoc(blocks, state.schema);
    tr.replaceWith(0, state.doc.content.size, doc.content);

    if (caret) {
      const pos = blockCaretToPos(tr.doc, caret.blockIndex, caret.charOffset);
      if (pos != null) tr.setSelection(TextSelection.create(tr.doc, pos));
    }
    view.dispatch(tr);

    // the editor now reflects `blocks`; that's the baseline the next user edit diffs
    // against (this FROM_ELM transaction is skipped by onTransaction, so it won't set
    // the baseline itself).
    this._baseline = currentEditorBlocks(this.editor);

    // keep focus so the caret is visible after OUR OWN structural edit; never grab
    // focus for a remote update.
    if (caret && !view.hasFocus()) view.focus();
  }

  // The current selection head as a block-relative {blockIndex, charOffset}, or null.
  _currentBlockCaret() {
    const { selection } = this.editor.state;
    const $head = selection.$head;
    return { blockIndex: $head.index(0), charOffset: $head.parentOffset };
  }

  // --- position helpers ----------------------------------------------------
  // A "block index" is the index of a top-level block node; a "char offset" within a
  // block is the number of text characters before a position. Document char offset =
  // sum of block lengths before + offset within.

  _blockIndexAt(pos) {
    // which top-level block contains PM position `pos`
    const $pos = this.editor.state.doc.resolve(pos);
    // depth 1 = the block node; index at depth 0 is the block index
    return $pos.index(0);
  }

  _charOffsetInBlock(pos) {
    const $pos = this.editor.state.doc.resolve(pos);
    // start of this block's content:
    const blockStart = $pos.start(1);
    return pos - blockStart;
  }

  // --- input → intents -----------------------------------------------------

  _onKeydown(e) {
    const { state } = this.editor;
    const { selection } = state;
    const { $from, empty } = selection;
    const blockIndex = $from.index(0);

    // Enter (split) and Backspace-at-block-start (merge) are handled NATIVELY by
    // ProseMirror — we do not intercept them. The resulting block-count change is
    // detected in `_emitTransaction`, which classifies it (via `diffStructural` against
    // the running baseline) into the CRDT's commutative `split`/`merge` primitive, or a
    // `reconcile` for complex edits. Letting PM own the DOM edit (rather than
    // preventDefault + re-render from Elm) avoids a race where a re-render carrying a
    // pre-edit snapshot wiped text typed immediately after a split.
    //
    // Only indent/outdent are intercepted, since ProseMirror has no native notion of
    // our flat depth attribute.
    if (e.key === "Tab") {
      e.preventDefault();
      this._desiredCaret = { blockIndex, charOffset: $from.parentOffset };
      this._emit({ tag: e.shiftKey ? "outdent" : "indent", blockIndex });
    }
    // everything else (typing, Enter, Backspace, arrows) flows through PM →
    // onTransaction, which mirrors text and structural changes to the CRDT.
  }

  // Translate a user PM transaction into text/mark intents. Block-structure edits
  // (split/merge/indent/outdent) come from _onKeydown / the toolbar directly; here we
  // only report text + marks. Text intents are emitted PER BLOCK ({tag,blockIndex,text})
  // so Elm diffs within that block's characters — typed text can't leak across a block
  // boundary (the bug the whole-document diff had). We emit a block's text when it
  // differs from the last blocks Elm pushed down (`_pendingBlocks`); re-emitting an
  // unchanged block is harmless (Elm's per-block diff is a no-op).
  _emitTransaction(editor, transaction) {
    if (!transaction.docChanged) return;

    let textChanged = false;
    const markIntents = [];

    for (const step of transaction.steps) {
      const stepType = step.toJSON().stepType;
      if (stepType === "addMark" || stepType === "removeMark") {
        const type = step.mark.type.name;
        const from = this._docCharOffset(step.from);
        const to = this._docCharOffset(step.to);
        if (to > from) {
          markIntents.push({
            tag: "mark",
            type,
            value:
              stepType === "addMark"
                ? type === "link"
                  ? step.mark.attrs.href
                  : true
                : null,
            from,
            to,
          });
        }
      } else {
        textChanged = true;
      }
    }

    if (textChanged) {
      const current = currentEditorBlocks(editor);
      // Diff against the running `_baseline` (the editor state as of our last emit),
      // NOT `_pendingBlocks` — see the constructor note. This keeps a local split
      // followed by typing classified as `split` + `text` rather than a stale-baseline
      // `reconcile`.
      const prev = this._baseline || this._pendingBlocks || [];

      if (current.length === prev.length) {
        // same block count: per-block text diff (typing within blocks).
        current.forEach((blk, index) => {
          const text = blockText(blk);
          if (text !== blockText(prev[index])) {
            this._emit({ tag: "text", blockIndex: index, text });
          }
        });
      } else {
        // The block count changed. Prefer the PRECISE structural op (split / merge)
        // when it's a single clean Enter / Backspace, because those map to the CRDT's
        // COMMUTATIVE `splitBlock` / `mergeBlock` primitives and so converge under
        // concurrent editing. Only fall back to the whole-structure `reconcile` (which
        // does not commute) for genuinely complex edits — a multi-block paste or
        // selection delete.
        const op = diffStructural(prev, current);
        if (op && op.kind === "split") {
          this._emit({ tag: "split", blockIndex: op.blockIndex, charOffset: op.charOffset });
        } else if (op && op.kind === "merge") {
          this._emit({ tag: "merge", blockIndex: op.blockIndex });
        } else {
          this._emit({
            tag: "reconcile",
            blocks: current.map((b) => ({
              type: b.type || "",
              depth: b.depth || 0,
              text: blockText(b),
            })),
          });
        }
      }
    }
    for (const intent of markIntents) this._emit(intent);

    // The editor's post-transaction blocks are now the baseline for the NEXT user edit.
    this._baseline = currentEditorBlocks(editor);
  }

  // PM position → document-wide character offset (chars in all blocks before it +
  // chars before it within its block). Block boundaries don't count as chars — they
  // match the CRDT's flattened char stream that setRich/mark operate on.
  _docCharOffset(pos) {
    const doc = this.editor.state.doc;
    let offset = 0;
    let found = 0;
    const $pos = doc.resolve(pos);
    const targetIndex = $pos.index(0);
    doc.forEach((blockNode, _blockPmOffset, index) => {
      if (index < targetIndex) offset += blockNode.textContent.length;
    });
    found = offset + $pos.parentOffset;
    return found;
  }

  emitSetType(type) {
    // setType re-renders the block (its type changes) — keep the caret where it is.
    const { $from } = this.editor.state.selection;
    this._desiredCaret = { blockIndex: $from.index(0), charOffset: $from.parentOffset };
    this._emit({ tag: "setType", blockIndex: $from.index(0), type });
  }

  _emit(detail) {
    this.dispatchEvent(
      new CustomEvent("richtext-input", { detail, bubbles: true, composed: true })
    );
  }
}

// --- blocks <-> ProseMirror doc --------------------------------------------

// The demo's block-type vocabulary → PM heading level / node styling. The CRDT stores
// the string opaquely; this map is purely the demo's presentation choice.
function headingLevel(type) {
  return { h1: 1, h2: 2, h3: 3 }[type] || 0;
}

// A block-relative caret ({blockIndex, charOffset}) → an absolute PM position in
// `doc`, clamped to that block's text. Returns null if the block index is out of
// range. Used to restore the caret after a doc rebuild.
function blockCaretToPos(doc, blockIndex, charOffset) {
  const bi = Math.max(0, Math.min(blockIndex, doc.childCount - 1));
  if (bi < 0) return null;
  const blockNode = doc.child(bi);
  // start-of-content position of block `bi` = 1 (open doc) + sum of prior block sizes
  let pos = 1;
  for (let i = 0; i < bi; i++) pos += doc.child(i).nodeSize;
  const maxOffset = blockNode.content.size;
  return pos + Math.max(0, Math.min(charOffset, maxOffset));
}

function crdtMarkToPm(schema, type, value) {
  const m = schema.marks[type];
  if (!m) return null;
  return type === "link" ? m.create({ href: String(value) }) : m.create();
}

function blocksToDoc(blocks, schema) {
  // schema may be omitted on the very first content build (TipTap parses HTML then);
  // in that case fall back to an HTML string.
  if (!schema) return blocksToHtml(blocks);

  const blockNodes = (blocks && blocks.length ? blocks : [{ type: "", depth: 0, spans: [] }]).map(
    (block) => {
      const inline = [];
      for (const span of block.spans || []) {
        if (!span.text) continue;
        const marks = [];
        for (const [type, value] of Object.entries(span.marks || {})) {
          const mk = crdtMarkToPm(schema, type, value);
          if (mk) marks.push(mk);
        }
        inline.push(schema.text(span.text, marks));
      }
      return schema.nodes.block.create(
        { blockType: block.type || "", depth: block.depth || 0 },
        inline
      );
    }
  );
  return schema.nodes.doc.create(null, blockNodes);
}

// Initial content before the editor's schema exists: an HTML string of div.block's.
function blocksToHtml(blocks) {
  const esc = (s) =>
    (s || "").replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  const spanHtml = (span) => {
    let t = esc(span.text);
    const m = span.marks || {};
    if (m.code) t = `<code>${t}</code>`;
    if (m.strike) t = `<s>${t}</s>`;
    if (m.underline) t = `<u>${t}</u>`;
    if (m.italic) t = `<em>${t}</em>`;
    if (m.bold) t = `<strong>${t}</strong>`;
    if (m.link) t = `<a href="${esc(String(m.link))}">${t}</a>`;
    return t;
  };
  const list = blocks && blocks.length ? blocks : [{ type: "", depth: 0, spans: [] }];
  return list
    .map((b) => {
      const inner = (b.spans || []).map(spanHtml).join("") || "";
      return `<div class="block" data-type="${esc(b.type)}" data-depth="${b.depth || 0}">${inner}</div>`;
    })
    .join("");
}

// Serialize the editor's current PM doc back to CRDT-shaped blocks (for the skip
// comparison in _applyBlocks).
function currentEditorBlocks(editor) {
  const blocks = [];
  editor.state.doc.forEach((blockNode) => {
    const spans = [];
    blockNode.forEach((child) => {
      if (!child.isText) return;
      const marks = {};
      for (const mk of child.marks) {
        marks[mk.type.name] = mk.type.name === "link" ? mk.attrs.href : true;
      }
      const last = spans[spans.length - 1];
      if (last && sameMarkObj(last.marks, marks)) last.text += child.text;
      else spans.push({ text: child.text, marks });
    });
    blocks.push({
      type: blockNode.attrs.blockType || "",
      depth: blockNode.attrs.depth || 0,
      spans,
    });
  });
  return blocks;
}

// The plain text of a CRDT-shaped block (concatenated span text). `undefined` blocks
// (e.g. reading past the end of the previous list) read as "".
function blockText(block) {
  if (!block) return "";
  return (block.spans || []).map((s) => s.text || "").join("");
}

// Classify a block-count change between two block lists as a single `split` or `merge`
// when it cleanly is one, else null (→ the caller falls back to a full reconcile).
// A split at block i means prev[i] === cur[i] + cur[i+1] with the blocks before i and
// after the pair unchanged; a merge is the inverse. We scan every candidate index
// rather than "find the first divergence", because an END-of-block split leaves block i
// itself unchanged (cur[i] === prev[i], cur[i+1] === "") — the divergence heuristic
// overshot it and mis-fired a reconcile (which doesn't commute, breaking concurrent
// editing). Returns the block-relative position for the commutative primitive.
function diffStructural(prev, cur) {
  const pt = prev.map(blockTextOf);
  const ct = cur.map(blockTextOf);

  if (ct.length === pt.length + 1) {
    for (let i = 0; i < pt.length; i++) {
      if (
        arrEq(pt.slice(0, i), ct.slice(0, i)) &&
        ct[i] + ct[i + 1] === pt[i] &&
        arrEq(pt.slice(i + 1), ct.slice(i + 2))
      ) {
        return { kind: "split", blockIndex: i, charOffset: ct[i].length };
      }
    }
  } else if (ct.length === pt.length - 1) {
    for (let i = 0; i < ct.length; i++) {
      if (
        arrEq(ct.slice(0, i), pt.slice(0, i)) &&
        ct[i] === pt[i] + pt[i + 1] &&
        arrEq(ct.slice(i + 1), pt.slice(i + 2))
      ) {
        return { kind: "merge", blockIndex: i + 1 };
      }
    }
  }
  return null;
}

function arrEq(a, b) {
  return a.length === b.length && a.every((x, i) => x === b[i]);
}

function blockTextOf(b) {
  return typeof b.text === "string" ? b.text : blockText(b);
}

function sameMarkObj(a, b) {
  const ak = Object.keys(a);
  const bk = Object.keys(b);
  if (ak.length !== bk.length) return false;
  return ak.every((k) => a[k] === b[k]);
}

function blocksEqual(a, b) {
  const norm = (bs) =>
    (bs || []).map((b) => ({
      type: b.type || "",
      depth: b.depth || 0,
      spans: (b.spans || []).filter((s) => s.text && s.text.length).map((s) => ({ text: s.text, marks: s.marks || {} })),
    }));
  const na = norm(a);
  const nb = norm(b);
  if (na.length !== nb.length) return false;
  return na.every((blk, i) => {
    const o = nb[i];
    if (blk.type !== o.type || blk.depth !== o.depth || blk.spans.length !== o.spans.length) return false;
    return blk.spans.every((s, j) => s.text === o.spans[j].text && sameMarkObj(s.marks, o.spans[j].marks));
  });
}

// --- toolbar ---------------------------------------------------------------

function buildToolbar(host) {
  const bar = document.createElement("div");
  bar.className = "pm-toolbar";

  const markBtn = (label, run) => {
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = label;
    b.addEventListener("mousedown", (e) => {
      e.preventDefault();
      run(host.editor.chain().focus());
    });
    return b;
  };

  bar.appendChild(markBtn("B", (c) => c.toggleBold().run()));
  bar.appendChild(markBtn("I", (c) => c.toggleItalic().run()));
  bar.appendChild(markBtn("U", (c) => c.toggleUnderline().run()));
  bar.appendChild(markBtn("S", (c) => c.toggleStrike().run()));
  bar.appendChild(markBtn("</>", (c) => c.toggleCode().run()));
  bar.appendChild(
    markBtn("link", (c) => {
      if (host.editor.isActive("link")) c.unsetLink().run();
      else {
        const href = window.prompt("Link URL:", "https://");
        if (href) c.setLink({ href }).run();
      }
    })
  );

  // block-type buttons: emit a setType intent (toggles back to paragraph if already
  // that type). These do not touch PM directly — the CRDT round-trip re-renders.
  const typeBtn = (label, type) => {
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = label;
    b.addEventListener("mousedown", (e) => {
      e.preventDefault();
      const cur = host.editor.state.selection.$from.parent.attrs.blockType || "";
      host.emitSetType(cur === type ? "" : type);
    });
    return b;
  };
  const sep = document.createElement("span");
  sep.className = "pm-toolbar-sep";
  bar.appendChild(sep);
  bar.appendChild(typeBtn("H1", "h1"));
  bar.appendChild(typeBtn("H2", "h2"));
  bar.appendChild(typeBtn("H3", "h3"));
  bar.appendChild(typeBtn("❝", "blockquote"));
  bar.appendChild(typeBtn("•", "ul"));
  bar.appendChild(typeBtn("1.", "ol"));

  return bar;
}

customElements.define("crdt-richtext", CrdtRichText);
