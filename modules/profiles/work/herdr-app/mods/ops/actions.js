// Quick actions: focus a terminal, focus (or start) the orchestrator, start or reuse a
// standing agent. Everything keys on hc labels (role=, agent=, task=) and falls back
// to terminal names when labels are missing.

import { hc, run, notify, terminals, panes, spaces, openOverlay, closeOverlay } from "herdr";
import { config, expand, vaultDir } from "./config.js";

const live = () => terminals().filter((t) => t.state === "live");
const labels = (t) => (t && typeof t.labels === "object" && t.labels) || {};
export const roleOf = (t) => labels(t).role ?? null;
export const taskOf = (t) => labels(t).task ?? null;
export const agentOf = (t) => labels(t).agent ?? t?.name ?? null;

// A terminal with a pane: show it there. A sub-agent without one: peek at it.
export function focusTerminal(address) {
  if (!address) return false;
  if (panes().some((p) => p.terminal === address)) run("pane.focus", { terminal: address });
  else run("terminal.peek", { terminal: address });
  return true;
}

// The same, as a toast action (decided now; the toast runs it later).
export function focusAction(address) {
  return panes().some((p) => p.terminal === address)
    ? { name: "pane.focus", args: { terminal: address } }
    : { name: "terminal.peek", args: { terminal: address } };
}

export const orchestrator = () => live().find((t) => roleOf(t) === "orchestrator") ?? null;

export function findByName(name) {
  if (!name) return null;
  return live().find((t) => labels(t).agent === name) ?? live().find((t) => t.name === name) ?? null;
}

// A space on this machine for new terminals: the shown one if it's local, else the
// first local one. Labels use local ids (no "machine/" prefix).
function localSpace() {
  const all = spaces();
  const s = all.find((x) => x.active && !x.machine) ?? all.find((x) => !x.machine);
  return s?.id ?? null;
}

// Starts argv in cwd through hc, labeled so Herdr shows it in the current space,
// then focuses it once its pane exists. Returns the address or null.
async function start(cx, { name, cwd, argv, labels: extra }) {
  if (!Array.isArray(argv) || !argv.length) throw new Error("no argv configured");
  const args = ["run", "--bg", "--json", "--outlive-parent"];
  if (name) args.push("--name", name);
  if (cwd) args.push("--cwd", expand(cwd));
  for (const [k, v] of Object.entries(extra ?? {})) args.push("--label", `${k}=${v}`);
  const space = localSpace();
  if (space) args.push("--label", `herdr.space=${space}`);
  args.push("--", ...argv.map(String));
  const r = await hc(args);
  if (r.code !== 0) throw new Error((r.stderr || r.stdout).trim().split("\n")[0] || `hc exited ${r.code}`);
  let address = null;
  try { address = JSON.parse(r.stdout)?.address ?? null; } catch {}
  address ??= /\bt\d+\b/.exec(r.stdout)?.[0] ?? null;
  // Herdr adopts the labeled terminal into a tab within a moment; wait for its pane.
  for (let i = 0; i < 20 && address; i++) {
    if (panes().some((p) => p.terminal === address)) break;
    await cx.sleep(150);
  }
  if (address) focusTerminal(address);
  return address;
}

const busy = new Set(); // action names in flight: a double press doesn't start two agents

async function guarded(key, body) {
  if (busy.has(key)) return;
  busy.add(key);
  try { await body(); } catch (err) {
    notify({ title: `Ops: ${key} failed`, body: String(err?.message ?? err).slice(0, 300), level: "error", timeout: 6000 });
  } finally { busy.delete(key); }
}

export function goOrchestrator(cx) {
  return guarded("orchestrator", async () => {
    const o = orchestrator();
    if (o) { focusTerminal(o.address); return; }
    const cfg = config();
    const spec = cfg.orchestrator ?? {};
    await start(cx, {
      name: spec.name ?? null,
      cwd: spec.cwd ? expand(spec.cwd) : vaultDir(cfg),
      argv: spec.argv ?? ["pi"],
      labels: { ...(spec.labels ?? {}), role: "orchestrator" },
    });
  });
}

// cfg.standing[key] = { title?, name, cwd, argv, labels? }: reuse the live one by name
// (agent= label or terminal name), else start it with role=standing and agent=<name>.
export function goStanding(key, cx) {
  return guarded(key, async () => {
    const spec = (config().standing ?? {})[key];
    if (!spec) throw new Error(`no standing agent "${key}" configured (props.standing.${key})`);
    const existing = findByName(spec.name);
    if (existing) { focusTerminal(existing.address); return; }
    await start(cx, {
      name: spec.name,
      cwd: spec.cwd,
      argv: spec.argv,
      labels: { role: "standing", ...(spec.name ? { agent: spec.name } : {}), ...(spec.labels ?? {}) },
    });
  });
}

export const standingKeys = () => Object.keys(config().standing ?? {});

// The calendar window: "calendar" toggles it, "calendar-open" only opens it.
let calendarOverlay = null;
export function calendarClosed(id) { if (!id || id === calendarOverlay) calendarOverlay = null; }
function calendar(toggle) {
  if (calendarOverlay) {
    if (toggle) { closeOverlay(calendarOverlay); calendarOverlay = null; }
    return;
  }
  calendarOverlay = openOverlay({ mod: "ops", kind: "window", key: "ops-calendar", props: { role: "calendar" } });
}

// The commands this mod answers to (keys.json: "<key>": "ops.<command>").
export function command(name, cx) {
  if (name === "orchestrator") return goOrchestrator(cx);
  if (name === "board") { run("mod.open", { name: "ops" }); return; }
  if (name === "calendar" || name === "calendar-open") { calendar(name === "calendar"); return; }
  if ((config().standing ?? {})[name]) return goStanding(name, cx);
  if (name !== "focus") notify({ title: `Ops: unknown command "${name}"`, level: "warning", timeout: 4000 });
}

