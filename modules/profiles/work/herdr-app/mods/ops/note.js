// A task note viewer pane (props.role = "note", props.file = absolute path). The board
// opens one beside the focused pane, then points it at other notes through shared
// state instead of splitting again. Frontmatter shows as pills; wikilinks open in place.

import { div } from "gpui-kit";
import { h_flex, v_flex, TextView } from "gpui-base";
import { readTextFile, watchFolder, theme, props, shared, setShared, exec, run, panes, focused, window as win } from "herdr";
import { frontmatter } from "./parse.js";
import { vaultDir } from "./config.js";

export const NOTE_KEY = "ops.note"; // { file, seq }: the note the board asked to show
const REFRESH_MS = 15000;

// One note pane per tab: reuse it (it follows NOTE_KEY), else split one beside the
// focused pane.
export function openNote(file) {
  if (!file) return;
  setShared(NOTE_KEY, { file, seq: Date.now() });
  const tab = focused()?.tab;
  const open = panes().some((p) => p.mod === "ops" && p.tab === tab);
  if (!open) run("pane.split", { direction: "right", mod: "ops", props: { role: "note", file } });
}

const dirOf = (p) => p.replace(/\/[^/]*$/, "") || "/";

// [[target|alias]] -> [alias](wikilink:target), so a click can resolve it in the vault.
function links(md) {
  return md.replace(/\[\[([^\]|#]+)(#[^\]|]*)?(?:\|([^\]]+))?\]\]/g, (_m, target, _h, alias) =>
    `[${(alias ?? target.split("/").pop()).replace(/[[\]]/g, "")}](wikilink:${encodeURIComponent(target.trim())})`);
}

export class Note {
  init(cx) {
    this.file = props()?.file ?? null;
    this.seq = shared(NOTE_KEY)?.seq ?? 0; // only later requests move this pane
    this.text = null;
    this.error = null;
    this.load(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.load(cx));
    // Follow the board's requests (render can't load: it has no async context).
    cx.timer.every(400, (cx) => {
      const req = shared(NOTE_KEY);
      if (req && req.seq > this.seq) { this.seq = req.seq; this.show(req.file, cx); }
    });
    this.watch(cx);
  }

  watch(cx) {
    if (!this.file) return;
    const dir = dirOf(this.file);
    this.watching = dir;
    cx.spawn(async (cx) => {
      while (this.watching === dir) {
        const changed = await watchFolder(dir);
        if (changed.some((p) => p === this.file || p === dir)) this.load(cx);
      }
    });
  }

  show(file, cx) {
    if (!file || file === this.file) return;
    this.file = file;
    this.text = null;
    this.load(cx);
    this.watch(cx);
  }

  async load(cx) {
    if (!this.file) { this.error = "No note given (props.file)."; cx.notify(); return; }
    try {
      const text = await readTextFile(this.file);
      this.text = text;
      this.error = text === null ? "The note isn't there any more." : null;
    } catch (err) {
      this.error = String(err?.message ?? err);
    }
    cx.notify();
  }

  openLink(url, cx) {
    if (url.startsWith("wikilink:")) {
      const target = decodeURIComponent(url.slice("wikilink:".length));
      this.show(`${vaultDir()}/${target.replace(/\.md$/i, "")}.md`, cx);
      return;
    }
    exec(win()?.platform === "macos" ? "open" : "xdg-open", [url]).catch(() => {});
  }

  render() {
    const t = theme();
    const { fm, body } = frontmatter(this.text ?? "");
    const pills = ["status", "owner", "jira", "updated"].filter((k) => fm[k]).map((k) =>
      h_flex().h(18).px(7).flex_none().items_center().rounded_full().bg(t.raised).border(1).border_color(t.border)
        .text_size(10).text_color(k === "status" ? t.accent : t.muted).child(`${k} ${fm[k]}`));
    const head = v_flex().flex_none().px(12).pt(8).pb(8).gap(6).border_b(1).border_color(t.border)
      .child(div().text_size(10.5).text_color(t.dim).truncate().child(this.file ? this.file.replace(vaultDir() + "/", "") : "Note"))
      .when(pills.length > 0, (el) => el.child(h_flex().gap(4).flex_wrap().children(pills)));
    const content = this.error
      ? div().p(12).text_size(12).text_color(t.blocked).child(this.error)
      : this.text === null
        ? div().p(12).text_size(12).text_color(t.dim).child("Loading…")
        : div().id("ops-note-body").flex_1().min_h(0).overflow_y_scrollbar().px(14).py(10)
          .child(TextView.markdown("ops-note-md", links(body)).selectable(true)
            .on_link_click((url, cx) => cx.spawn(async (cx) => this.openLink(url, cx))));
    return v_flex().size_full().min_w(0).bg(t.surface).text_color(t.foreground).child(head).child(content);
  }
}
