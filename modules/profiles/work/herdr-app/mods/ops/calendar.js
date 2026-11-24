// The calendar window (props.role = "calendar"): the Due calendar on its own, opened
// with the ops.calendar command or by clicking the daily due toast.

import { div } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { watchFolder, theme, props, closeOverlay, setShared } from "herdr";
import { vaultDir } from "./config.js";
import { loadOps } from "./data.js";
import { Calendar, dueItems, dueSummary, summaryText } from "./due.js";
import { openNote } from "./note.js";
import { calendarClosed } from "./actions.js";

const REFRESH_MS = 60000;

export class CalendarView {
  init(cx) {
    this.calendar = new Calendar();
    this.data = null;
    this.vault = vaultDir();
    this.load(cx);
    cx.timer.every(REFRESH_MS, (cx) => this.load(cx));
    cx.spawn(async (cx) => {
      for (;;) {
        try { await watchFolder(`${this.vault}/ops`); } catch { return; }
        await this.load(cx);
      }
    });
  }

  close() { calendarClosed(props()?.overlay); }

  async load(cx) {
    try { this.data = await loadOps(this.vault); } catch {}
    cx.notify();
  }

  render() {
    const t = theme();
    const items = dueItems(this.data, this.vault);
    const text = summaryText(dueSummary(items)) || (items.length ? "Nothing due today" : "No due dates yet");
    const overlay = props()?.overlay;
    return v_flex().w(300).p(10).gap(8).bg(t.surface).text_color(t.foreground)
      .child(h_flex().gap(6).items_center()
        .child(v_flex().flex_1().min_w(0)
          .child(div().text_size(13).font_semibold().child("Due"))
          .child(div().text_size(10).text_color(t.dim).child(text)))
        .when(Boolean(overlay), (el) => el.child(h_flex().id("ops-cal-close").size(22).flex_none().items_center()
          .justify_center().rounded_full().text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
          .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
          .on_click(() => closeOverlay(overlay)).child(div().text_size(13).child("×")))))
      .child(this.calendar.render(t, items, {
        open: (i) => openNote(i.note.file),
        highlight: (i) => setShared("ops.highlight", { id: i.id, at: Date.now() }),
      }, "ops-calw"));
  }
}
