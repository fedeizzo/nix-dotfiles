// The ops board (the default role): the vault's task lanes and delegations, joined
// with live hc status through the terminals' labels. Reads ops/Tasks.md,
// ops/Delegations.md and ops/tasks/*.md; refreshes on file changes (watchFolder) and
// on a slow timer as a fallback. A task opens its note beside the focused pane; a
// delegation focuses its terminal. Keys while focused: r refreshes.

import { div, svg } from "gpui-kit";
import { h_flex, v_flex } from "gpui-base";
import { watchFolder, theme, terminals, shared, setShared, nextCommand, props, closeOverlay } from "herdr";
import { config, vaultDir } from "./config.js";
import { loadOps, noteFor } from "./data.js";
import { Calendar, dueItems, dueSummary, summaryText, today } from "./due.js";
import { roleOf, taskOf, findByName, orchestrator, focusTerminal, goOrchestrator, goStanding, command } from "./actions.js";
import { openNote } from "./note.js";
import { watchers } from "./watch.js";

export const HIGHLIGHT_KEY = "ops.highlight"; // { id, at }: a board line to point at

const REFRESH_MS = 30000;
const LANES = [["now", "Now"], ["next", "Next"], ["waiting", "Waiting"], ["inbox", "Inbox"]]; // Next only when it has items
const ROLES = new Set(["delegate", "standing"]);

const ago = (ms) => {
  const s = Math.max(0, Math.round((Date.now() - ms) / 1000));
  return s < 60 ? `${s}s` : s < 3600 ? `${Math.floor(s / 60)}m` : `${Math.floor(s / 3600)}h`;
};

// #rrggbb + alpha, for tinted pills.
const tint = (c, a) => (/^#[0-9a-f]{6}$/i.test(c ?? "") ? c + Math.round(a * 255).toString(16).padStart(2, "0") : c);

export class Board {
  init(cx) {
    this.tasks = null;
    this.dels = null;
    this.notes = {}; // task id -> { file, fm }
    this.errors = [];
    this.loadedAt = 0;
    this.loading = false;
    this.vault = vaultDir();
    this.calendar = new Calendar();
    try { this.folded = JSON.parse(localStorage.getItem("folded") ?? "{}"); } catch { this.folded = {}; }
    this.handle = cx.focus_handle();
    this.load(cx);
    this.watch(cx);
    cx.timer.every(REFRESH_MS, (cx) => {
      if (vaultDir() !== this.vault) { this.vault = vaultDir(); this.watch(cx); }
      this.load(cx);
    });
    // Keys reach every shown copy; the watcher answers them when it runs, the board
    // only stands in when no watcher does (a layout without the floating block).
    cx.spawn(async (cx) => {
      for (;;) {
        const c = String(await nextCommand());
        if (c === "focus") { this.handle.focus(); continue; }
        if (!watchers.length) await command(c, cx);
      }
    });
  }

  watch(cx) {
    const dir = `${this.vault}/ops`;
    this.watching = dir;
    cx.spawn(async (cx) => {
      while (this.watching === dir) {
        try { await watchFolder(dir); } catch { return; } // the timer still refreshes
        if (this.watching === dir) await this.load(cx);
      }
    });
  }

  async load(cx) {
    if (this.loading) return;
    this.loading = true;
    try {
      const data = await loadOps(this.vault);
      this.data = data;
      this.tasks = data.tasks;
      this.dels = data.dels;
      this.notes = data.notes;
      this.errors = data.errors;
      this.loadedAt = Date.now();
    } catch (err) {
      this.errors = [String(err?.message ?? err)];
    } finally {
      this.loading = false;
      cx.notify();
    }
  }

  noteFor(task) { return noteFor(this.data, this.vault, task); }

  openNote(file) { openNote(file); }

  // Where a calendar item without a note points: its board line, unfolded.
  highlight(id) { setShared(HIGHLIGHT_KEY, { id, at: Date.now() }); }

  // Live terminal for a registry row: task= label first, then agent= label or name,
  // then the address the registry noted (only if nothing claims another task there).
  liveFor(row) {
    const live = terminals().filter((t) => t.state === "live");
    return (row.task && live.find((t) => taskOf(t) === row.task && ROLES.has(roleOf(t))))
      ?? (row.task && live.find((t) => taskOf(t) === row.task))
      ?? findByName(row.agent)
      ?? (row.hint && live.find((t) => t.address === row.hint && (!taskOf(t) || taskOf(t) === row.task)))
      ?? null;
  }

  dot(t, status, size = 8) {
    if (status === "working") return svg("mods/workspace-chrome/spinner.svg").size(size + 4).flex_none().text_color(t.working).spin(900);
    const color = { blocked: t.blocked, error: t.blocked, done: t.done, idle: t.idle }[status];
    if (!color) return div().size(size).flex_none().rounded_full().border(1.5).border_color(t.dim);
    if (status === "idle") return div().size(size).flex_none().rounded_full().border(1.5).border_color(color);
    return div().size(size).flex_none().rounded_full().bg(color);
  }

  pill(t, text, color) {
    const c = color ?? t.muted;
    return h_flex().h(17).px(6).flex_none().items_center().rounded_full()
      .bg(tint(c, 0.14) ?? t.raised).border(1).border_color(tint(c, 0.45) ?? t.border)
      .text_size(10).text_color(c).child(text);
  }

  section(t, key, title, count, rows, extra, open = false, foldByDefault = false) {
    const folded = (this.folded[key] ?? foldByDefault) && !open;
    return v_flex().flex_none().gap(2)
      .child(h_flex().id(`ops-sec-${key}`).h(24).px(8).gap(6).items_center().rounded(6).cursor_pointer()
        .hover((s) => s.bg(t.hover))
        .on_click((_e, cx) => {
          this.folded[key] = !folded;
          try { localStorage.setItem("folded", JSON.stringify(this.folded)); } catch {}
          cx.notify();
        })
        .child(div().w(10).text_size(9).text_color(t.dim).child(folded ? "▸" : "▾"))
        .child(div().text_size(10.5).font_semibold().text_color(t.muted).child(title.toUpperCase()))
        .child(div().text_size(10.5).text_color(count ? t.foreground : t.dim).child(String(count)))
        .child(div().flex_1())
        .when(Boolean(extra), (el) => el.child(extra)))
      .when(!folded, (el) => el.children(rows.length ? rows
        : [div().px(26).pb(4).text_size(11).text_color(t.dim).child("Empty")]));
  }

  taskRow(t, task, liveByTask, hl) {
    const note = this.noteFor(task);
    const due = task.fields.due ?? note?.fm?.due ?? null;
    const now = today();
    const term = liveByTask.get(task.id);
    const owner = task.fields.owner ?? "";
    const stale = note?.fm?.status && note.fm.status !== task.lane && !(task.lane === "waiting" && note.fm.status === "waiting");
    return v_flex().id(`ops-task-${task.id}`).mx(4).px(8).py(5).gap(2).rounded(7).flex_none()
      .when(hl === task.id, (el) => el.bg(tint(t.accent, 0.16) ?? t.selected).border(1).border_color(t.accent))
      .when(Boolean(note), (el) => el.cursor_pointer().hover((s) => s.bg(t.hover)).on_click(() => this.openNote(note.file)))
      .child(h_flex().gap(6).items_center()
        .child(div().flex_none().text_size(10.5).font_semibold().text_color(t.accent).child(task.id))
        .child(div().flex_1().min_w(0).text_size(12).truncate().text_color(t.foreground).child(task.title))
        .when(Boolean(term), (el) => el.child(this.dot(t, term.status))))
      .when(Boolean(owner || note || task.date), (el) => el.child(h_flex().pl(1).gap(5).items_center().text_size(10).text_color(t.dim)
        .when(Boolean(owner), (x) => x.child(div().truncate().child(owner)))
        .when(Boolean(task.date), (x) => x.child(div().flex_none().child(task.date)))
        .when(Boolean(note), (x) => x.child(div().flex_none().child("· note")))
        .when(Boolean(due) && task.lane !== "done", (x) => x.child(this.pill(t, `due ${due}`,
          due < now ? t.blocked : due === now ? t.accent : t.muted)))
        .when(Boolean(stale), (x) => x.child(this.pill(t, `note: ${note.fm.status}`, t.blocked)))));
  }

  delegationRow(t, row) {
    const term = row.live;
    const status = term ? (term.status ?? "idle") : null;
    const note = row.task ? this.noteFor({ id: row.task }) : null;
    const click = term ? () => focusTerminal(term.address) : note ? () => this.openNote(note.file) : null;
    return v_flex().id(`ops-del-${row.kind}-${row.agent}`).mx(4).px(8).py(5).gap(2).rounded(7).flex_none()
      .when(Boolean(click), (el) => el.cursor_pointer().hover((s) => s.bg(t.hover)).on_click(click))
      .child(h_flex().gap(6).items_center()
        .child(term ? this.dot(t, status) : this.dot(t, null))
        .child(div().flex_1().min_w(0).text_size(12).truncate().text_color(term ? t.foreground : t.muted).child(row.agent))
        .when(Boolean(row.task), (el) => el.child(div().flex_none().text_size(10.5).font_semibold().text_color(t.accent).child(row.task)))
        .when(row.kind === "standing", (el) => el.child(this.pill(t, "standing", t.muted)))
        .when(row.kind === "unregistered", (el) => el.child(this.pill(t, "not in registry", t.blocked))))
      .child(h_flex().pl(14).gap(5).items_center().text_size(10).text_color(t.dim)
        .child(div().flex_none().text_color(status === "blocked" || status === "error" ? t.blocked : t.dim)
          .child(term ? `${term.address} · ${status}` : "not running"))
        .when(Boolean(row.status) && row.status !== status, (x) => x.child(div().min_w(0).truncate().child(`· ${row.status}`))));
  }

  // Registry rows (active + standing) joined with live terminals, plus labeled
  // delegates/standing agents the registry doesn't list.
  delegations() {
    const rows = [
      ...(this.dels?.active ?? []).map((r) => ({ ...r, kind: "active" })),
      ...(this.dels?.standing ?? []).map((r) => ({ ...r, kind: "standing" })),
    ].map((r) => ({ ...r, live: this.liveFor(r) }));
    const claimed = new Set(rows.map((r) => r.live?.address).filter(Boolean));
    for (const term of terminals()) {
      if (term.state !== "live" || !ROLES.has(roleOf(term)) || claimed.has(term.address)) continue;
      rows.push({ agent: term.labels?.agent ?? term.name ?? term.address, task: taskOf(term), status: "", kind: "unregistered", live: term });
    }
    return rows;
  }

  actionButton(t, id, label, term, onClick) {
    return h_flex().id(id).h(24).px(9).gap(6).flex_none().items_center().rounded(7).cursor_pointer()
      .bg(t.raised).border(1).border_color(t.border).hover((s) => s.bg(t.hover))
      .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
      .on_click(onClick)
      .child(term ? this.dot(t, term.status ?? "idle", 7) : this.dot(t, null, 7))
      .child(div().text_size(11).text_color(t.foreground).child(label));
  }

  render() {
    const t = theme();
    const cfg = config();
    const lanes = this.tasks?.lanes ?? {};
    const dels = this.delegations();
    const liveByTask = new Map();
    for (const term of terminals()) if (term.state === "live" && taskOf(term)) liveByTask.set(taskOf(term), term);

    const actions = h_flex().px(8).pb(6).gap(6).flex_wrap()
      .child(this.actionButton(t, "ops-act-orch", "Orchestrator", orchestrator(),
        (_e, cx) => cx.spawn(async (cx) => goOrchestrator(cx))))
      .children(Object.entries(cfg.standing ?? {}).map(([key, spec]) =>
        this.actionButton(t, `ops-act-${key}`, spec.title ?? key, findByName(spec.name),
          (_e, cx) => cx.spawn(async (cx) => goStanding(key, cx)))));

    const head = h_flex().flex_none().px(10).pt(8).pb(6).gap(8).items_center()
      .child(v_flex().flex_1().min_w(0)
        .child(div().text_size(13).font_semibold().child("Ops board"))
        .child(div().text_size(10).text_color(t.dim).truncate().child(this.vault.replace(/^.*\//, "") || this.vault)))
      .child(div().id("ops-open-tasks").h(22).px(7).flex_none().flex().items_center().rounded(6).cursor_pointer()
        .text_size(10.5).text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
        .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click(() => this.openNote(`${this.vault}/ops/Tasks.md`)).child("Tasks.md"))
      .child(h_flex().id("ops-refresh").size(22).flex_none().items_center().justify_center().rounded_full()
        .text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
        .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click((_e, cx) => cx.spawn(async (cx) => this.load(cx)))
        .child(this.loading ? svg("mods/workspace-chrome/spinner.svg").size(12).text_color(t.working).spin(900)
          : div().text_size(13).child("⟳")))
      // Opened as a window (mod.open): a way out besides the key.
      .when(Boolean(props()?.overlay), (el) => el.child(h_flex().id("ops-close").size(22).flex_none().items_center()
        .justify_center().rounded_full().text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
        .on_mouse_down("left", (_e, cx) => cx.stop_propagation())
        .on_click(() => closeOverlay(props().overlay)).child(div().text_size(13).child("×"))));

    const items = dueItems(this.data, this.vault);
    const sum = dueSummary(items);
    const h = shared(HIGHLIGHT_KEY);
    const hl = h && Date.now() - (h.at ?? 0) < 15000 ? h.id : null;
    const hlLane = hl ? Object.entries(lanes).find(([, l]) => l.some((x) => x.id === hl))?.[0] : null;
    const dueRows = [div().px(8).child(this.calendar.render(t, items, {
      open: (i) => this.openNote(i.note.file),
      highlight: (i) => this.highlight(i.id),
    }))];
    if (sum.overdue.length) {
      dueRows.push(v_flex().mx(4).px(8).gap(2)
        .child(div().text_size(10).text_color(t.blocked).child("Overdue"))
        .children(sum.overdue.map((i) => h_flex().id(`ops-over-${i.id}`).gap(6).py(2).items_center().rounded(6).cursor_pointer()
          .hover((s) => s.bg(t.hover))
          .on_click(() => (i.note ? this.openNote(i.note.file) : this.highlight(i.id)))
          .child(div().flex_none().text_size(10.5).font_semibold().text_color(t.accent).child(i.id))
          .child(div().flex_1().min_w(0).truncate().text_size(11.5).child(i.title))
          .child(div().flex_none().text_size(10).text_color(t.blocked).child(i.due)))));
    }
    const dueBadge = summaryText(sum) ? this.pill(t, summaryText(sum), sum.overdue.length ? t.blocked : t.accent) : null;

    const body = div().id("ops-body").flex_1().min_h(0).overflow_y_scrollbar().pb(8)
      .child(v_flex().gap(6)
        .when(this.errors.length > 0, (el) => el.child(v_flex().mx(10).gap(2)
          .children(this.errors.map((e) => div().text_size(11).text_color(t.blocked).child(e)))))
        .child(this.section(t, "due", "Due", items.filter((i) => !i.done).length, dueRows, dueBadge, false, !items.length))
        .child(this.section(t, "delegations", "Delegations", dels.length, dels.map((r) => this.delegationRow(t, r))))
        .children(LANES.filter(([key]) => key !== "next" || (lanes.next ?? []).length).map(([key, title]) => {
          const list = lanes[key] ?? [];
          return this.section(t, key, title, list.length, list.map((task) => this.taskRow(t, task, liveByTask, hl)), null, key === hlLane);
        })));

    const doneN = lanes.done?.length ?? 0;
    const foot = h_flex().flex_none().px(10).h(24).gap(6).items_center().border_t(1).border_color(t.border)
      .text_size(10).text_color(t.dim)
      .child(div().child(`Done ${doneN}`))
      .when(Boolean(this.tasks?.nextId), (el) => el.child(div().child(`· next id ${this.tasks.nextId}`)))
      .child(div().flex_1())
      .child(div().child(this.loadedAt ? `read ${ago(this.loadedAt)} ago` : ""));

    return v_flex().id("ops-board").size_full().min_w(0).bg(t.surface).text_color(t.foreground)
      .track_focus(this.handle)
      .on_key_down((e, cx) => {
        const k = String(e.key ?? "").toLowerCase();
        if (k === "r" && !(e.modifiers?.control || e.modifiers?.platform || e.modifiers?.alt)) {
          cx.spawn(async (cx) => this.load(cx));
          cx.stop_propagation();
        }
      })
      .child(head).child(actions).child(body).child(foot);
  }
}
