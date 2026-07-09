// A <crdt-richtext> custom element: a TipTap (ProseMirror) editor whose document is
// owned by Elm's CRDT. The element is a *view + input device* only — it never holds
// authoritative state:
//
//   • Elm pushes the current document down as `docSpans` (a property) and via the
//     `renderRichText` port; the element reconciles its ProseMirror doc to match.
//   • User edits/format commands are reported up as `richtext-input` CustomEvents,
//     which index.js forwards to Elm's `richTextInput` port. Elm turns them into
//     CRDT ops (setRichText / mark / unmark), which converge, then flow back down.
//
// Loop avoidance: a doc pushed *down* is applied with a transaction meta flag, so the
// resulting ProseMirror transaction is not echoed back up as user input.

import { Editor } from "@tiptap/core";
import Document from "@tiptap/extension-document";
import Paragraph from "@tiptap/extension-paragraph";
import Text from "@tiptap/extension-text";
import Bold from "@tiptap/extension-bold";
import Italic from "@tiptap/extension-italic";
import Underline from "@tiptap/extension-underline";
import Strike from "@tiptap/extension-strike";
import Code from "@tiptap/extension-code";
import Link from "@tiptap/extension-link";

// v1 rich text is a SINGLE paragraph of inline-formatted text (see docs/10). Two
// things enforce that so the editor can't drift from the CRDT model:
//   • the document schema allows exactly one paragraph (`content: "paragraph"`), so
//     ProseMirror structurally refuses to split into a second block; and
//   • Enter / Shift-Enter are bound to no-ops, so pressing Enter does nothing rather
//     than attempting (and visibly half-doing) a split that the round-trip undoes.
const SingleParagraphDocument = Document.extend({ content: "paragraph" });

const NoLineBreaks = Paragraph.extend({
  addKeyboardShortcuts() {
    const swallow = () => true; // handled: do nothing
    return { Enter: swallow, "Shift-Enter": swallow, "Mod-Enter": swallow };
  },
});

// TipTap mark name → the mark `type_` string the CRDT uses (they line up here, but
// keeping the map explicit documents the contract with the Elm side).
const MARK_TYPES = ["bold", "italic", "underline", "strike", "code", "link"];

// A meta key marking a transaction as "applied from Elm", so onUpdate skips it.
const FROM_ELM = "crdtFromElm";

class CrdtRichText extends HTMLElement {
  constructor() {
    super();
    this.editor = null;
    this._pendingSpans = null; // spans set before the editor existed
  }

  connectedCallback() {
    const mount = document.createElement("div");
    mount.className = "pm-mount";
    this.appendChild(mount);

    this.editor = new Editor({
      element: mount,
      extensions: [
        SingleParagraphDocument,
        NoLineBreaks,
        Text,
        Bold,
        Italic,
        Underline,
        Strike,
        Code,
        Link.configure({ openOnClick: false }),
      ],
      content: spansToHtml(this._pendingSpans || currentSpansProp(this)),
      onTransaction: ({ editor, transaction }) => {
        if (transaction.getMeta(FROM_ELM)) return; // don't echo Elm-driven changes
        if (!transaction.docChanged) return;
        this._emitTransaction(editor, transaction);
      },
    });

    // format commands come from our own toolbar (below), dispatched as CustomEvents
    // on this element so the binding stays declarative.
    this._toolbar = buildToolbar(this);
    this.insertBefore(this._toolbar, mount);
  }

  disconnectedCallback() {
    if (this.editor) {
      this.editor.destroy();
      this.editor = null;
    }
  }

  // Elm sets this property (via A.property "docSpans") on mount and on every render.
  set docSpans(spans) {
    this._pendingSpans = spans;
    if (this.editor) this._applySpans(spans);
  }

  get docSpans() {
    return this._pendingSpans;
  }

  // Reconcile the ProseMirror doc to `spans` without echoing back to Elm. We skip
  // the replace when the editor already shows these exact spans (compared against
  // the *live* editor doc, not a cache) — otherwise we'd stomp the user's caret on
  // every keystroke's round-trip through Elm.
  _applySpans(spans) {
    if (!this.editor) return;
    if (spansEqual(currentEditorSpans(this.editor), spans || [])) return;

    const { state, view } = this.editor;
    const tr = state.tr.setMeta(FROM_ELM, true);
    const node = spansToDoc(state.schema, spans);
    tr.replaceWith(0, state.doc.content.size, node.content);
    view.dispatch(tr);
  }

  // Translate a user ProseMirror transaction into CRDT intents. Text changes (typing,
  // delete, paste) become a `text` intent carrying the whole paragraph; formatting
  // changes (from the toolbar OR keyboard shortcuts, both of which are real PM
  // transactions) become `mark` intents per mark step. A single transaction can carry
  // both (e.g. paste), so we look at all its steps.
  _emitTransaction(editor, transaction) {
    let textChanged = false;
    const markIntents = [];

    for (const step of transaction.steps) {
      // Discriminate by ProseMirror's registered step type, NOT constructor.name —
      // bundlers (esbuild) rename classes (we saw `_AddMarkStep`), so class-name
      // matching silently breaks. `stepType` is a stable string in the step's JSON.
      const stepType = step.toJSON().stepType;
      if (stepType === "addMark" || stepType === "removeMark") {
        const type = step.mark.type.name;
        // step.from/to are PM positions; the paragraph opens at 1 → char = pos-1.
        const from = Math.max(0, step.from - 1);
        const to = Math.max(from, step.to - 1);
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
        // replace / replaceAround — the text content changed
        textChanged = true;
      }
    }

    // Emit the text change first (so a mark applied to just-typed text lands on
    // characters Elm already knows about), then each mark change.
    if (textChanged) {
      this._emit({ tag: "text", text: editor.getText() });
    }
    for (const intent of markIntents) {
      this._emit(intent);
    }
  }

  _emit(detail) {
    this.dispatchEvent(
      new CustomEvent("richtext-input", {
        detail,
        bubbles: true,
        composed: true,
      })
    );
  }
}

function currentSpansProp(el) {
  return el._pendingSpans || [{ text: "", marks: {} }];
}

// Serialize the editor's current single paragraph as CRDT-shaped spans (adjacent
// characters with equal marks grouped), so we can compare against an incoming render.
function currentEditorSpans(editor) {
  const spans = [];
  const doc = editor.state.doc;
  doc.descendants((node) => {
    if (!node.isText) return true;
    const marks = {};
    for (const m of node.marks) {
      marks[m.type.name] = m.type.name === "link" ? m.attrs.href : true;
    }
    const last = spans[spans.length - 1];
    if (last && sameMarkObj(last.marks, marks)) last.text += node.text;
    else spans.push({ text: node.text, marks });
    return false;
  });
  return spans;
}

function sameMarkObj(a, b) {
  const ak = Object.keys(a);
  const bk = Object.keys(b);
  if (ak.length !== bk.length) return false;
  return ak.every((k) => a[k] === b[k]);
}

// Compare two span lists for text+mark equality (empty spans ignored).
function spansEqual(a, b) {
  const norm = (spans) =>
    (spans || [])
      .filter((s) => s.text && s.text.length)
      .map((s) => ({ text: s.text, marks: s.marks || {} }));
  const na = norm(a);
  const nb = norm(b);
  if (na.length !== nb.length) return false;
  return na.every(
    (s, i) => s.text === nb[i].text && sameMarkObj(s.marks, nb[i].marks)
  );
}

// Build a minimal formatting toolbar. Each button runs the matching TipTap command,
// which produces a real ProseMirror transaction — the same path as the keyboard
// shortcuts — so `onTransaction` picks up the mark change and reports it to Elm.
// (Applying the mark in PM immediately also gives instant local feedback; the CRDT
// round-trip then confirms it and syncs to peers.)
function buildToolbar(host) {
  const bar = document.createElement("div");
  bar.className = "pm-toolbar";
  const btn = (label, run) => {
    const b = document.createElement("button");
    b.type = "button";
    b.textContent = label;
    b.addEventListener("mousedown", (e) => {
      e.preventDefault(); // keep the editor selection
      run(host.editor.chain().focus());
    });
    return b;
  };
  bar.appendChild(btn("B", (c) => c.toggleBold().run()));
  bar.appendChild(btn("I", (c) => c.toggleItalic().run()));
  bar.appendChild(btn("U", (c) => c.toggleUnderline().run()));
  bar.appendChild(btn("S", (c) => c.toggleStrike().run()));
  bar.appendChild(btn("</>", (c) => c.toggleCode().run()));
  bar.appendChild(
    btn("link", (c) => {
      if (host.editor.isActive("link")) {
        c.unsetLink().run();
      } else {
        const href = window.prompt("Link URL:", "https://");
        if (href) c.setLink({ href }).run();
      }
    })
  );
  return bar;
}

// ---- span <-> ProseMirror conversions -------------------------------------

// Marks the CRDT knows → TipTap mark spec name (+ attrs for value marks).
function crdtMarkToPm(schema, type, value) {
  switch (type) {
    case "link":
      return schema.marks.link.create({ href: String(value) });
    case "bold":
      return schema.marks.bold.create();
    case "italic":
      return schema.marks.italic.create();
    case "underline":
      return schema.marks.underline.create();
    case "strike":
      return schema.marks.strike.create();
    case "code":
      return schema.marks.code.create();
    default:
      return null;
  }
}

// Build a ProseMirror doc node (one paragraph) from CRDT spans.
function spansToDoc(schema, spans) {
  const inline = [];
  for (const span of spans || []) {
    if (!span.text) continue;
    const marks = [];
    for (const [type, value] of Object.entries(span.marks || {})) {
      const m = crdtMarkToPm(schema, type, value);
      if (m) marks.push(m);
    }
    inline.push(schema.text(span.text, marks));
  }
  const paragraph = schema.nodes.paragraph.create(null, inline);
  return schema.nodes.doc.create(null, [paragraph]);
}

// Initial HTML content for the editor before the first port render.
function spansToHtml(spans) {
  const esc = (s) =>
    s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  let html = "";
  for (const span of spans || []) {
    let t = esc(span.text || "");
    const marks = span.marks || {};
    if (marks.code) t = `<code>${t}</code>`;
    if (marks.strike) t = `<s>${t}</s>`;
    if (marks.underline) t = `<u>${t}</u>`;
    if (marks.italic) t = `<em>${t}</em>`;
    if (marks.bold) t = `<strong>${t}</strong>`;
    if (marks.link) t = `<a href="${esc(String(marks.link))}">${t}</a>`;
    html += t;
  }
  return `<p>${html}</p>`;
}

customElements.define("crdt-richtext", CrdtRichText);

export { MARK_TYPES };
