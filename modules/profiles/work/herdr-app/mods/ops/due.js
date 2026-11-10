// Due dates: tasks with "due: YYYY-MM-DD" on their board line (or, failing that, in
// their note's frontmatter; the board line wins), and a month calendar of them.
// Overdue = due before today and not done. Used by the board's Due section, the
// calendar window (props.role = "calendar") and the watcher's daily toast.

import { div } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { plain } from "./parse.js";
import { noteFor } from "./data.js";

const DATE = /\b(\d{4})-(\d{2})-(\d{2})\b/;
const dateOf = (s) => DATE.exec(String(s ?? ""))?.[0] ?? null;
const pad = (n) => String(n).padStart(2, "0");
export const ymd = (y, m, d) => `${y}-${pad(m + 1)}-${pad(d)}`; // m: 0-11
export function today() {
  const d = new Date();
  return ymd(d.getFullYear(), d.getMonth(), d.getDate());
}

const MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
const DAYS = ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"];

// [{ id, title, due, lane, done, note }], soonest first.
export function dueItems(data, vault) {
  const out = [];
  const seen = new Set();
  for (const [lane, list] of Object.entries(data?.tasks?.lanes ?? {})) {
    for (const task of list) {
      seen.add(task.id);
      const note = noteFor(data, vault, task);
      const due = dateOf(task.fields.due) ?? dateOf(note?.fm?.due);
      if (due) out.push({ id: task.id, title: task.title, due, lane, done: lane === "done", note });
    }
  }
  // Notes whose task isn't on the board (yet, or any more).
  for (const [id, note] of Object.entries(data?.notes ?? {})) {
    const due = seen.has(id) ? null : dateOf(note.fm?.due);
    if (!due) continue;
    const st = note.fm.status ?? "";
    out.push({ id, title: plain(note.fm.title ?? "") || id, due, lane: st || "note", done: st === "done" || st === "dropped", note });
  }
  return out.sort((a, b) => a.due.localeCompare(b.due) || a.id.localeCompare(b.id));
}

export function dueSummary(items, day = today()) {
  const open = items.filter((i) => !i.done);
  return { today: open.filter((i) => i.due === day), overdue: open.filter((i) => i.due < day) };
}

export const summaryText = ({ today: t, overdue: o }) =>
  [t.length ? `${t.length} due today` : null, o.length ? `${o.length} overdue` : null].filter(Boolean).join(" · ");

// The calendar's own state; render() is a plain function of it plus the items.
export class Calendar {
  constructor() {
    const d = new Date();
    this.y = d.getFullYear();
    this.m = d.getMonth();
    this.selected = today();
  }

  shift(n) {
    const d = new Date(this.y, this.m + n, 1);
    this.y = d.getFullYear();
    this.m = d.getMonth();
  }

  dotColor(t, day, list, now) {
    const open = list.filter((i) => !i.done);
    if (!open.length) return null;
    return day < now ? t.blocked : day === now ? t.accent : t.muted;
  }

  // on: { open(item), highlight(item) }
  render(t, items, on, idp = "ops-cal") {
    const now = today();
    const byDay = new Map();
    for (const i of items) byDay.set(i.due, [...(byDay.get(i.due) ?? []), i]);
    const first = (new Date(this.y, this.m, 1).getDay() + 6) % 7; // Monday first
    const days = new Date(this.y, this.m + 1, 0).getDate();
    const cells = [...Array(first).fill(null), ...Array.from({ length: days }, (_, i) => i + 1)];
    while (cells.length % 7) cells.push(null);
    const weeks = [];
    for (let i = 0; i < cells.length; i += 7) weeks.push(cells.slice(i, i + 7));

    const nav = (id, label, fn) => div().id(`${idp}-${id}`).w(22).h(20).flex().items_center().justify_center()
      .rounded(5).cursor_pointer().text_size(12).text_color(t.muted).hover((s) => s.bg(t.hover).text_color(t.foreground))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click((_e, cx) => { fn(); cx.notify(); }).child(label);

    const cell = (d, wi, di) => {
      if (d === null) return div().id(`${idp}-e${wi}-${di}`).flex_1().h(28);
      const day = ymd(this.y, this.m, d);
      const dot = this.dotColor(t, day, byDay.get(day) ?? [], now);
      const isToday = day === now;
      const sel = day === this.selected;
      return v_flex().id(`${idp}-d${day}`).flex_1().h(28).items_center().justify_center().gap(1).rounded(6)
        .cursor_pointer().hover((s) => s.bg(t.hover))
        .when(sel, (el) => el.bg(t.selected ?? t.raised))
        .when(isToday, (el) => el.border(1).border_color(t.accent))
        .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click((_e, cx) => { this.selected = day; cx.notify(); })
        .child(div().text_size(11).text_color(isToday ? t.accent : t.foreground).when(isToday, (el) => el.font_semibold()).child(String(d)))
        .child(div().size(4).rounded_full().bg(dot ?? "#00000000"));
    };

    const list = byDay.get(this.selected) ?? [];
    const row = (i) => h_flex().id(`${idp}-i-${i.id}`).px(6).py(4).gap(6).items_center().rounded(6).cursor_pointer()
      .hover((s) => s.bg(t.hover))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click(() => (i.note ? on.open(i) : on.highlight(i)))
      .child(div().size(6).flex_none().rounded_full().bg(i.done ? t.dim : i.due < now ? t.blocked : i.due === now ? t.accent : t.muted))
      .child(div().flex_none().text_size(10.5).font_semibold().text_color(t.accent).child(i.id))
      .child(div().flex_1().min_w(0).truncate().text_size(11.5).text_color(i.done ? t.dim : t.foreground).child(i.title))
      .child(div().flex_none().text_size(9.5).text_color(t.dim).child(i.lane));

    return v_flex().gap(4)
      .child(h_flex().px(2).gap(4).items_center()
        .child(nav("prev", "‹", () => this.shift(-1)))
        .child(div().flex_1().text_size(11.5).font_semibold().text_color(t.foreground).child(`${MONTHS[this.m]} ${this.y}`))
        .child(div().id(`${idp}-today`).px(6).h(20).flex().items_center().rounded(5).cursor_pointer().text_size(10).text_color(t.dim)
          .hover((s) => s.bg(t.hover).text_color(t.foreground))
          .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
          .on_click((_e, cx) => {
            const d = new Date();
            this.y = d.getFullYear(); this.m = d.getMonth(); this.selected = today();
            cx.notify();
          }).child("Today"))
        .child(nav("next", "›", () => this.shift(1))))
      .child(h_flex().children(DAYS.map((d) => div().flex_1().text_size(9.5).text_color(t.dim).flex().justify_center().child(d))))
      .children(weeks.map((w, wi) => h_flex().gap(1).children(w.map((d, di) => cell(d, wi, di)))))
      .child(v_flex().pt(2).gap(1)
        .child(div().px(6).text_size(10).text_color(t.dim).child(list.length ? `Due ${this.selected}` : `Nothing due ${this.selected}`))
        .children(list.map(row)));
  }
}
