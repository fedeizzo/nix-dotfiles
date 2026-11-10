// Parsers for the vault's ops files. The formats are a contract with the notes:
//   ops/Tasks.md        "## <lane>" headings; one task per bullet:
//                       "- **T-NNN** · title · field · field"; footer "Next task ID: **T-NNN**".
//   ops/Delegations.md  markdown tables under "## <section>"; the first column holds
//                       the agent name in backticks.
//   ops/tasks/*.md      frontmatter: task, status, owner, jira, updated.
// Anything that doesn't match is skipped, never fatal: a half-edited file shows less.

const LANES = [
  ["inbox", /^inbox/i],
  ["now", /^now\b/i],
  ["next", /^next\b/i],
  ["waiting", /^(waiting|blocked)/i],
  ["done", /^done/i],
];

export const laneOf = (heading) => (LANES.find(([, re]) => re.test(heading.trim())) ?? [null])[0];

// "[[a/b|c]]" -> c, "[[a/b]]" -> b, "[t](u)" -> t, emphasis and code marks dropped.
export function plain(s) {
  return String(s ?? "")
    .replace(/\[\[([^\]|]+)\|([^\]]+)\]\]/g, "$2")
    .replace(/\[\[([^\]]+)\]\]/g, (_m, t) => t.split("/").pop())
    .replace(/\[([^\]]+)\]\([^)]*\)/g, "$1")
    .replace(/\*\*([^*]+)\*\*/g, "$1")
    .replace(/(^|[\s(])_([^_]+)_(?=$|[\s).,;:])/g, "$1$2")
    .replace(/`([^`]+)`/g, "$1")
    .trim();
}

// The first wikilink target in a string, without alias or heading.
export function wikiTarget(s) {
  const m = /\[\[([^\]|#]+)(?:#[^\]|]*)?(?:\|[^\]]*)?\]\]/.exec(String(s ?? ""));
  return m ? m[1].trim() : null;
}

export function parseTasks(text) {
  const out = { lanes: { inbox: [], now: [], next: [], waiting: [], done: [] }, nextId: null };
  let lane = null;
  for (const raw of String(text ?? "").split(/\r?\n/)) {
    const h = /^##\s+(.+?)\s*$/.exec(raw);
    if (h) { lane = laneOf(h[1]); continue; }
    const n = /Next task ID:\s*\*\*(T-\d+)\*\*/i.exec(raw);
    if (n) { out.nextId = n[1]; continue; }
    if (!lane) continue;
    const m = /^\s*[-*]\s+\*\*(T-\d+)\*\*\s*(?:·\s*)?(.*)$/.exec(raw);
    if (!m) continue;
    const parts = m[2].split(/\s+·\s+/).map((s) => s.trim()).filter(Boolean);
    const task = { id: m[1], lane, title: plain(parts[0] ?? ""), fields: {}, note: null, date: null, raw };
    for (const part of parts.slice(1)) {
      const kv = /^([a-z][a-z _-]{0,20}):\s*(.*)$/i.exec(part);
      if (kv) {
        const key = kv[1].trim().toLowerCase();
        task.fields[key] = plain(kv[2]);
        if (key === "note") task.note = wikiTarget(kv[2]);
      } else if (/^\d{4}-\d{2}-\d{2}$/.test(part)) task.date = part;
      if (!task.note && /\[\[ops\/tasks\//.test(part)) task.note = wikiTarget(part);
    }
    out.lanes[lane].push(task);
  }
  return out;
}

// Splits a table row on "|", but not inside [[...]] (wikilink aliases) or after "\".
function cells(row) {
  const out = [];
  let cur = "";
  let depth = 0;
  const s = row.trim().replace(/^\|/, "").replace(/\|$/, "");
  for (let i = 0; i < s.length; i++) {
    const c = s[i];
    if (c === "\\" && s[i + 1] === "|") { cur += "|"; i++; continue; }
    if (c === "[" && s[i + 1] === "[") { depth++; cur += "[["; i++; continue; }
    if (c === "]" && s[i + 1] === "]" && depth > 0) { depth--; cur += "]]"; i++; continue; }
    if (c === "|" && depth === 0) { out.push(cur.trim()); cur = ""; continue; }
    cur += c;
  }
  out.push(cur.trim());
  return out;
}

const SECTIONS = [
  ["active", /^active/i],
  ["standing", /^standing/i],
  ["closed", /^closed/i],
  ["unmanaged", /^not managed/i],
];

// { active: [row], standing: [row], closed: [row], unmanaged: [row] }; a row is
// { agent, task, status, hint, cols: {header: text} }.
export function parseDelegations(text) {
  const out = { active: [], standing: [], closed: [], unmanaged: [] };
  let section = null;
  let header = null;
  for (const raw of String(text ?? "").split(/\r?\n/)) {
    const h = /^##\s+(.+?)\s*$/.exec(raw);
    if (h) { section = (SECTIONS.find(([, re]) => re.test(h[1])) ?? [null])[0]; header = null; continue; }
    if (!/^\s*\|/.test(raw)) { header = null; continue; } // a table ends at any other line
    if (!section) continue;
    const row = cells(raw);
    if (row.every((c) => /^:?-{2,}:?$/.test(c) || c === "")) continue; // the --- line
    if (!header) { header = row.map((c) => plain(c).toLowerCase()); continue; }
    const cols = {};
    header.forEach((name, i) => { cols[name] = row[i] ?? ""; });
    const first = row[0] ?? "";
    const tick = /`([^`]+)`/.exec(first);
    const agent = (tick ? tick[1] : plain(first)).trim();
    if (!agent || /^_?none_?$/i.test(agent)) continue;
    const col = (re) => { const k = header.find((name) => re.test(name)); return k ? cols[k] : ""; };
    const task = /\bT-\d+\b/.exec(col(/^task/) || raw)?.[0] ?? null;
    // "hc **t7**", "hc t7": a terminal address the registry noted (may be stale).
    const where = col(/hc|terminal|pane/);
    const hint = /\bhc\s+\**([a-z0-9_-]+\/)?(t\d+)\b/i.exec(where);
    out[section].push({
      agent,
      task,
      status: plain(col(/^status/)),
      lastCheck: plain(col(/last (check|run)/)),
      hint: hint ? (hint[1] ?? "") + hint[2] : null,
      noteTarget: wikiTarget(col(/^task/)),
      cols,
    });
  }
  return out;
}

// Frontmatter of a task note, as flat strings.
export function frontmatter(text) {
  const m = /^---\r?\n([\s\S]*?)\r?\n---\r?\n?/.exec(String(text ?? ""));
  const fm = {};
  if (!m) return { fm, body: String(text ?? "") };
  for (const line of m[1].split(/\r?\n/)) {
    const kv = /^([A-Za-z_][\w-]*):\s*(.*)$/.exec(line);
    if (!kv) continue;
    fm[kv[1]] = kv[2].trim().replace(/^["'](.*)["']$/, "$1");
  }
  return { fm, body: String(text).slice(m[0].length) };
}

export const taskIdOf = (filename) => /^(T-\d+)\b/.exec(filename)?.[1] ?? null;
