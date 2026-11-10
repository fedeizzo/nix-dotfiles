// The watcher: an invisible floating copy (props.role = "watch") that toasts when a
// terminal labeled role=delegate|standing needs attention, and answers key commands.
//
// A toast fires once per state change, after the state held for STABLE_MS (a blocked
// that clears within that never shows), never twice for the same state, and at most
// once per COOLDOWN_MS per terminal and kind. Kinds:
//   blocked  status "blocked" (a question or a permission)
//   error    status "error"
//   done     "done"/"idle" after it was working (or its turn counter moved)
//   exited   its program ended, or the terminal went away
// Nothing is toasted for what was already true when the watcher started, nor while
// the person is looking at that terminal. Clicking a toast focuses the terminal.
//
// Once a day (at start, then after midnight) it also reads the vault and toasts
// "N due today · M overdue" when there is any; clicking opens the calendar. The day
// it last did so is kept in localStorage (and shared, for the other layouts' copy).

import { div } from "gpui-kit";
import { terminals, focused, notify, playSound, props, nextCommand, shared, setShared, window as win } from "herdr";
import { config, publish, vaultDir } from "./config.js";
import { loadOps } from "./data.js";
import { dueItems, dueSummary, summaryText, today } from "./due.js";
import { roleOf, taskOf, agentOf, focusAction, command } from "./actions.js";

const TICK_MS = 1000;
const STABLE_MS = 2000;
const COOLDOWN_MS = 10000;
const ROLES = new Set(["delegate", "standing"]);
const DUE_CHECK_MS = 10 * 60 * 1000; // until today's due toast is out
const DUE_KEY = "ops.due.toasted";

// Live watcher copies, newest last. Only the newest toasts and runs commands, so a
// second copy (another layout's, a reload's overlap) never doubles anything.
export const watchers = [];

const wordOf = (t) => (t.state !== "live" ? "exited" : String(t.status ?? "none"));

function kindOf(word, e) {
  if (word === "blocked" || word === "error" || word === "exited") return word;
  if ((word === "done" || word === "idle") && e.worked) return "done";
  return null;
}

const LEVEL = { blocked: "warning", error: "error", done: "success", exited: "info" };

export class Watch {
  constructor() {
    this.token = {};
    this.seen = new Map(); // address -> { word, since, notified, worked, turn, last: {kind: ms}, info }
  }

  init(cx) {
    publish(props() ?? {});
    watchers.push(this.token);
    this.tick(true);
    cx.timer.every(TICK_MS, () => this.tick(false));
    this.dueCheckedAt = 0;
    cx.timer.after(3000, (cx) => this.dueToast(cx)); // let the window settle first
    cx.timer.every(60000, (cx) => this.dueToast(cx));
    cx.spawn(async (cx) => {
      for (;;) {
        const c = await nextCommand();
        if (this.leader()) await command(String(c), cx);
      }
    });
  }

  close() {
    const i = watchers.indexOf(this.token);
    if (i >= 0) watchers.splice(i, 1);
  }

  leader() { return watchers[watchers.length - 1] === this.token; }

  async dueToast(cx) {
    const day = today();
    const vault = vaultDir();
    const mark = `${day} ${vault}`; // per vault: pointing at another vault toasts again
    let last = null;
    try { last = localStorage.getItem("dueToast"); } catch {}
    if (last === mark || shared(DUE_KEY) === mark || !this.leader()) return;
    if (this.dueCheckedDay === day && Date.now() - this.dueCheckedAt < DUE_CHECK_MS) return;
    this.dueCheckedDay = day;
    this.dueCheckedAt = Date.now();
    let data;
    try { data = await loadOps(vault); } catch { return; }
    const sum = dueSummary(dueItems(data, vault), day);
    const text = summaryText(sum);
    if (!text) return; // nothing due: look again later today
    const list = [...sum.overdue, ...sum.today].slice(0, 4).map((i) => `${i.id} ${i.title}`).join("\n");
    notify({ title: text, body: list, level: sum.overdue.length ? "warning" : "info", timeout: 15000,
      action: { name: "ops.calendar-open" } });
    try { localStorage.setItem("dueToast", mark); } catch {}
    setShared(DUE_KEY, mark);
  }

  tick(first) {
    const now = Date.now();
    const present = new Set();
    for (const t of terminals()) {
      if (!ROLES.has(roleOf(t))) continue;
      present.add(t.address);
      const word = wordOf(t);
      const turn = typeof t.turn === "number" ? t.turn : null;
      const info = { role: roleOf(t), task: taskOf(t), agent: agentOf(t), code: t.exit_code ?? t.exit?.code ?? null };
      let e = this.seen.get(t.address);
      if (!e) {
        // At startup, whatever is true now counts as already told; a terminal that
        // shows up later starts fresh (a new delegate stuck on a trust prompt toasts).
        e = { word, since: now, notified: first ? kindOf(word, { worked: false }) : null, worked: false, turn, last: {}, info };
        this.seen.set(t.address, e);
        if (first) continue;
      }
      e.info = info;
      if (turn !== null && e.turn !== null && turn > e.turn) e.worked = true;
      e.turn = turn;
      if (word !== e.word) { e.word = word; e.since = now; }
      if (word === "working") { e.worked = true; e.notified = null; continue; }
      const kind = kindOf(word, e);
      if (!kind || e.notified === kind) continue;
      if (kind !== "exited" && now - e.since < STABLE_MS) continue;
      e.notified = kind;
      if (kind === "done") e.worked = false;
      this.toast(t.address, kind, e, now);
    }
    // Gone from the core (removed after exiting, or killed and removed): an exit.
    for (const [address, e] of this.seen) {
      if (present.has(address)) continue;
      if (e.word !== "exited") this.toast(address, "exited", e, now);
      this.seen.delete(address);
    }
  }

  toast(address, kind, e, now) {
    if (!this.leader()) return;
    if (now - (e.last[kind] ?? 0) < COOLDOWN_MS) return;
    e.last[kind] = now;
    // The person is looking at it: they know.
    const f = focused();
    if (kind !== "exited" && f?.terminal === address && win()?.active) return;
    const who = [e.info.task, e.info.agent && e.info.agent !== address ? e.info.agent : null].filter(Boolean).join(" · ") || address;
    const title = {
      blocked: `${who} needs you`,
      error: `${who} hit an error`,
      done: `${who} finished`,
      exited: `${who} exited`,
    }[kind];
    const code = e.info.code;
    const body = [e.info.role, address, kind === "exited" && code !== null && code !== undefined ? `exit code ${code}` : null]
      .filter(Boolean).join(" · ");
    const n = { title, body, level: kind === "exited" && code ? "error" : LEVEL[kind], timeout: kind === "blocked" || kind === "error" ? 15000 : 8000 };
    if (kind !== "exited") n.action = focusAction(address);
    notify(n);
    const sound = config().sound;
    if (sound === true || (Array.isArray(sound) && sound.includes(kind))) {
      try { playSound(kind === "blocked" || kind === "error" ? "blocked" : "done"); } catch {}
    }
  }

  // Nothing to see: the floating block sizes to its content.
  render() { return div(); }
}
