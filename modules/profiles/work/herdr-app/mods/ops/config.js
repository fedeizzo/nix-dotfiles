// Where the ops mod finds its settings. One place: the watcher copy gets them as
// layout props (the work profile's extras.json puts them on its floating block) and
// publishes them through shared state, so a board opened anywhere else uses the same.
// Precedence: this copy's own props > shared("ops.config") > DEFAULTS.

import { home, props, shared, setShared } from "herdr";

export const CONFIG_KEY = "ops.config";

export const DEFAULTS = {
  // The markdown vault holding ops/Tasks.md, ops/Delegations.md and ops/tasks/*.md.
  vault: "~/notes/notes",
  // Sound on attention toasts: false, true (every kind), or a list of kinds
  // ("blocked", "error", "done", "exited").
  sound: false,
  // Quick actions. `orchestrator` is focused by its role=orchestrator label and, when
  // none runs, started with this argv in its cwd (default: the vault).
  // `standing` agents are started or reused by name: { name: { cwd, argv, labels } }.
  orchestrator: { argv: ["pi"], cwd: null },
  standing: {},
};

const PICK = ["vault", "sound", "orchestrator", "standing"];

function pick(obj) {
  const out = {};
  for (const k of PICK) if (obj && obj[k] !== undefined && obj[k] !== null) out[k] = obj[k];
  return out;
}

export function config() {
  const sh = shared(CONFIG_KEY);
  return { ...DEFAULTS, ...pick(sh && typeof sh === "object" ? sh : {}), ...pick(props() ?? {}) };
}

// The watcher publishes its props; only rewrite when they changed (setShared redraws
// every mod).
export function publish(own) {
  const next = pick(own);
  const cur = shared(CONFIG_KEY);
  if (JSON.stringify(cur ?? null) !== JSON.stringify(next)) setShared(CONFIG_KEY, next);
}

export function expand(path) {
  if (!path) return path;
  const p = String(path);
  if (p === "~") return home();
  if (p.startsWith("~/")) return home().replace(/\/$/, "") + p.slice(1);
  return p;
}

export const vaultDir = (cfg = config()) => expand(cfg.vault || DEFAULTS.vault).replace(/\/$/, "");
