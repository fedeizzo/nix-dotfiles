// GitHub Issues panel: the issues of the repo the focused pane is in, through `gh`.
// Rich rows: state, avatar, #number, issue type, labels, milestone, assignees — every
// colored pill is also a filter; a search bar filters the fetched list client-side.
// Keys (when the panel has focus): / search, j/k or up/down move, o or Enter opens,
// r refreshes, s toggles open/closed, Esc clears the search.
// Repo auto-detected from the focused pane's folder; pin one with props {repo:
// "owner/name"} or props {folder: "/path"} in the layout block.

import { View, div, svg } from "gpui-kit";
import { h_flex, v_flex, InputState, Input, Avatar, AvatarImage, AvatarFallback } from "gpui-base";
import { exec, theme, run, focused, panes, spaces, props, appHome, home, shared, setShared, openOverlay, closeOverlay } from "herdr";

const REFRESH_MS = 60000; // stale after this: re-fetch on the next tick
const TICK_MS = 5000;     // look for a folder change / staleness this often
const LIMIT = "100";
const AVATAR_DIR = "data/gh-issues/avatars"; // under appHome, cached across starts
const AVA_BAKED = 2; // have[login]: 1 = downloaded square, 2 = baked round
const FKEY = "gh-issues.filters"; // shared mirror of the filter set: menus write, the panel reads
// The triage prompt for the card button. {number} {title} {url} {repo} get replaced.
const TRIAGE = "Let\'s triage this GitHub issue: #{number} \"{title}\" ({url}) in {repo}. Read it and explore the related code if needed. We have to find labels, the issue type, and the issue-type fields. Then check for duplicates of this issue. Present the triage/summary, but do not do anything (no comments, no labels, no edits) without the user\'s approval.";

const FACETS = [["people", "authors"], ["types", "type"], ["labels", "labels"], ["fields", "fields"]];

// GitHub's issue-type and label colors arrive as CRAYOLA names.
const CRAYOLA = {
  BLACK: "#1b1f23", BLUE: "#54aeff", CYAN: "#39c5cf", ELF_SKIN: "#f3b7a0", GREEN: "#4ac26b",
  GREY: "#8b949e", HOT_PINK: "#ff80ed", LIME: "#b8e44d", NAVY: "#1b4b8f", ORANGE: "#fb8c44",
  PINK: "#f9a8d3", PURPLE: "#a371f7", RED: "#e5534b", TEAL: "#33b3ae", VIOLET: "#8b7cf6", YELLOW: "#d4c72b",
};

const HUES = ["#22a99a", "#e3814f", "#5b8def", "#8b7cf6", "#3fae6a", "#d9649a"];

function hexOf(color, fallback) {
  if (!color) return fallback;
  const c = String(color);
  if (/^#?[0-9a-f]{6}$/i.test(c)) return c.startsWith("#") ? c : "#" + c;
  return CRAYOLA[c.toUpperCase()] ?? fallback;
}

// A color at some opacity, for tinted pill backgrounds (#rgb, #rrggbb, #rrggbbaa).
function alpha(color, a) {
  const hex = /^#([0-9a-f]{3})$/i.test(color ?? "") ? "#" + [...color.slice(1)].map((c) => c + c).join("") : color;
  if (!/^#[0-9a-f]{6}([0-9a-f]{2})?$/i.test(hex ?? "")) return color;
  return hex.slice(0, 7) + Math.round(a * 255).toString(16).padStart(2, "0");
}

function ago(iso) {
  const m = (Date.now() - Date.parse(iso)) / 60000;
  if (!(m >= 0)) return "";
  if (m < 60) return `${Math.max(1, Math.floor(m))}m`;
  const h = m / 60;
  if (h < 24) return `${Math.floor(h)}h`;
  const d = h / 24;
  if (d < 30) return `${Math.floor(d)}d`;
  return `${Math.floor(d / 30)}mo`;
}

const firstLine = (s, n = 110) => ((s ?? "").trim().split("\n")[0] || "").slice(0, n);
const hue = (login) => HUES[[...String(login ?? "")].reduce((a, c) => a + c.charCodeAt(0), 0) % HUES.length];
const two = (login) => String(login ?? "?").replace(/[^a-zA-Z0-9]/g, "").slice(0, 2).toUpperCase();

export default class GhIssues extends View {
  init(_props, cx) {
    this.p = props() ?? {};
    if (this.p.facet) return; // a popup facet menu opened by openFacet: it only draws its list
    this.repo = null;
    this.cwd = null;
    this.issues = null;
    this.fields = {}; // issue number -> [{name, value, color}] from the org issue-type fields
    this.loading = false;
    this.error = null;
    this.fetchedAt = 0;
    this.at = -1;
    this.typing = false;
    this.state = "open";
    this.filters = { labels: [], types: [], people: [], fields: [] };
    try {
      this.state = localStorage.getItem("state") === "closed" ? "closed" : "open";
      Object.assign(this.filters, JSON.parse(localStorage.getItem("filters") ?? "{}"));
    } catch {}
    this.have = {};
    try { this.have = JSON.parse(localStorage.getItem("avatars") ?? "{}"); } catch {}
    this.handle = cx.focus_handle();
    this.input = InputState.new({ placeholder: "Search issues" });
    this.input.on("change", (_e, cx) => { this.at = -1; cx.notify(); });
    this.input.on("focus", (_e, cx) => { this.typing = true; cx.notify(); });
    this.input.on("blur", (_e, cx) => { this.typing = false; cx.notify(); });
    // Ask the tabs bar for our button (tabs-jj draws it when the list includes us).
    const panels = shared("workspace.panels") ?? ["files", "changes"];
    if (!panels.includes("gh-issues")) setShared("workspace.panels", [...panels, "gh-issues"]);
    this.load(cx);
    cx.timer.every(TICK_MS, (cx) => {
      const cwd = this.targetCwd();
      if (cwd !== this.cwd || Date.now() - this.fetchedAt > REFRESH_MS) this.load(cx);
    });
  }

  saveFilters() {
    try { localStorage.setItem("filters", JSON.stringify(this.filters)); } catch {}
  }

  targetCwd() {
    if (this.p.folder) return this.p.folder;
    const f = focused();
    const pane = panes().find((x) => x.focused);
    const space = spaces().find((s) => s.active);
    return f?.cwd || pane?.cwd || space?.folder || null;
  }

  machine() { return spaces().find((s) => s.active)?.machine ?? null; }

  async load(cx) {
    if (this.loading) return;
    const cwd = this.targetCwd();
    this.loading = true;
    this.error = null;
    cx.notify();
    try {
      if (!cwd) { this.repo = null; this.issues = null; this.error = "No folder to look for a repo in."; return; }
      const on = this.machine();
      let repo = this.p.repo ?? null;
      if (!repo) {
        const r = await exec("gh", ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"], { cwd, on });
        if (r.code !== 0) { this.repo = null; this.issues = null; this.error = firstLine(r.stderr) || "Not a repo with a GitHub remote."; return; }
        repo = r.stdout.trim();
      }
      this.repo = repo;
      this.cwd = cwd;
      const r2 = await exec("gh", [
        "issue", "list", "--repo", repo, "--state", this.state, "--limit", LIMIT,
        "--json", "number,title,url,updatedAt,labels,author,assignees,milestone,issueType,stateReason",
      ], { cwd, on });
      if (r2.code !== 0) { this.issues = null; this.error = firstLine(r2.stderr) || "gh issue list failed."; return; }
      this.issues = JSON.parse(r2.stdout);
      this.fetchedAt = Date.now();
      this.fields = {};
      // Paint the list now; org fields and avatars enrich the cards in the background.
      this.loading = false;
      cx.notify();
      this.enrich(repo, cwd, on, cx);
    } catch (err) {
      this.issues = null;
      this.error = `gh could not run: ${err?.message ?? err}`;
    } finally {
      this.loading = false;
      cx.notify();
    }
  }

  // Slow extras after the list is on screen. Issue-type custom fields are not in
  // `gh issue list --json`: read them through GraphQL, batched over the same search
  // scope. Repos without org fields show none.
  async enrich(repo, cwd, on, cx) {
    try {
      const gq = "query($q: String!) { search(query: $q, type: ISSUE, first: " + LIMIT + ") { nodes { ... on Issue { number issueFieldValues(first: 20) { nodes { __typename ... on IssueFieldTextValue { value field { ... on IssueFieldText { name } } } ... on IssueFieldNumberValue { value field { ... on IssueFieldNumber { name } } } ... on IssueFieldDateValue { value field { ... on IssueFieldDate { name } } } ... on IssueFieldSingleSelectValue { value color field { ... on IssueFieldSingleSelect { name } } } ... on IssueFieldMultiSelectValue { value field { ... on IssueFieldMultiSelect { name } } } } } } } } }";
      const r3 = await exec("gh", ["api", "graphql", "-f", "q=repo:" + repo + " is:issue is:" + this.state, "-f", "query=" + gq], { cwd, on });
      if (r3.code === 0) {
        for (const n of JSON.parse(r3.stdout)?.data?.search?.nodes ?? []) {
          const vals = (n.issueFieldValues?.nodes ?? []).map((v) => ({
            name: v.field?.name, value: v.value, color: hexOf(v.color, null),
          })).filter((v) => v.name && String(v.value ?? "") !== "");
          if (vals.length) this.fields[n.number] = vals;
        }
        cx.notify();
      }
    } catch {}
    const top = this.shown().slice(0, 12);
    const logins = top.flatMap((i) => [i.author?.login, ...(i.assignees ?? []).map((a) => a.login)]);
    this.loadAvatars(logins.filter(Boolean), cx);
  }

  // GitHub serves a picture per login; keep it under appHome already round (a GPUI image
  // ignores a corner radius, so ffmpeg draws the circle into the file), fall back to a
  // colored initials bubble. What is baked is never fetched again.
  loadAvatars(logins, cx) {
    const uniq = [...new Set(logins)].filter((l) => /^[a-zA-Z0-9-]+$/.test(l) && this.have[l] !== AVA_BAKED).slice(0, 10);
    if (!uniq.length) return;
    exec("mkdir", ["-p", `${appHome()}/${AVATAR_DIR}`]).catch(() => {});
    const save = () => { try { localStorage.setItem("avatars", JSON.stringify(this.have)); } catch {} };
    for (const login of uniq) {
      const p = `${appHome()}/${AVATAR_DIR}/${login}.png`;
      (async () => {
        if (this.have[login] !== 1) {
          const r = await exec("curl", ["-fsL", "--max-time", "15", "-o", p, `https://github.com/${login}.png?size=48`]);
          if (r.code !== 0) return;
          this.have[login] = 1;
          save();
        }
        const vf = "scale=64:64,format=rgba,geq=r='r(X,Y)':g='g(X,Y)':b='b(X,Y)':a='if(lte(hypot(X-W/2,Y-H/2),min(W,H)/2),255,0)'";
        const q = p + ".r.png"; // a real .png suffix, so ffmpeg picks the PNG muxer
        // The app's PATH may miss ~/.cargo/bin, where nix-less ffmpeg often lives.
        const tries = ["ffmpeg", `${home()}/.cargo/bin/ffmpeg`, "/run/current-system/sw/bin/ffmpeg", "/usr/bin/ffmpeg"];
        const args = ["-hide_banner", "-loglevel", "error", "-y", "-i", p, "-vf", vf, "-frames:v", "1", q];
        let b = { code: 1 };
        for (const ffx of tries) {
          try { b = await exec(ffx, args); } catch { b = { code: 1 }; }
          if (b.code === 0) break;
        }
        if (b.code !== 0) { exec("rm", ["-f", q]).catch(() => {}); return; }
        const m = await exec("mv", ["-f", q, p]);
        if (m.code === 0) {
          this.have[login] = AVA_BAKED;
          save();
          cx.notify();
        }
      })().catch(() => {});
    }
  }

  openIssue(iss) {
    if (iss?.url) exec("xdg-open", [iss.url]).catch(() => {});
  }

  toggleState(cx) {
    this.state = this.state === "open" ? "closed" : "open";
    try { localStorage.setItem("state", this.state); } catch {}
    this.issues = null;
    this.at = -1;
    this.load(cx);
  }

  toggleFilter(kind, value, cx) {
    const list = this.filters[kind];
    const at = list.indexOf(value);
    if (at >= 0) list.splice(at, 1);
    else list.push(value);
    this.saveFilters();
    setShared(FKEY, JSON.parse(JSON.stringify(this.filters)));
    this.at = -1;
    cx.notify();
  }

  clearFilters(cx) {
    this.filters = { labels: [], types: [], people: [], fields: [] };
    this.input.set_value("");
    this.saveFilters();
    setShared(FKEY, JSON.parse(JSON.stringify(this.filters)));
    cx.notify();
  }

  // A pi on the issue, in a pane split to the right of the focused terminal, asked to
  // triage without acting. It starts in the panel's repo folder.
  triage(iss, cx) {
    const text = TRIAGE.split("{number}").join(String(iss.number)).split("{title}").join(iss.title ?? "")
      .split("{url}").join(iss.url ?? "").split("{repo}").join(this.repo ?? "");
    const args = { argv: ["pi", text], place: "right" };
    if (this.cwd) args.folder = this.cwd;
    run("terminal.open", args);
  }

  shown() {
    const list = this.issues ?? [];
    const q = (this.input.value() ?? "").trim().toLowerCase();
    const { labels, types, people, fields: fld } = this.filters;
    if (!q && !labels.length && !types.length && !people.length && !fld.length) return list;
    return list.filter((i) => {
      if (labels.length && !labels.some((l) => (i.labels ?? []).some((x) => x.name === l))) return false;
      if (types.length && !types.includes(i.issueType?.name ?? "None")) return false;
      if (fld.length && !fld.some((f) => (this.fields?.[i.number] ?? []).some((v) => `${v.name}::${v.value}` === f))) return false;
      if (people.length) {
        const who = [i.author?.login, ...(i.assignees ?? []).map((a) => a.login)].filter(Boolean);
        if (!people.some((pe) => who.includes(pe))) return false;
      }
      if (q) {
        const hay = [
          `#${i.number}`, i.title, i.author?.login, i.issueType?.name, i.milestone?.title,
          ...(i.labels ?? []).map((l) => l.name), ...(i.assignees ?? []).map((a) => a.login),
          ...(this.fields?.[i.number] ?? []).map((v) => `${v.name} ${v.value}`),
        ].join(" ").toLowerCase();
        if (!hay.includes(q)) return false;
      }
      return true;
    });
  }

  move(d, cx) {
    const n = this.shown().length;
    if (!n) return;
    this.at = Math.max(0, Math.min(n - 1, (this.at < 0 ? (d > 0 ? -1 : 0) : this.at) + d));
    cx.notify();
  }

  key(e, cx) {
    if (this.typing) {
      if (e.key === "Escape") { this.input.set_value(""); cx.notify(); }
      return;
    }
    const k = String(e.key ?? "").toLowerCase();
    const plain = !(e.modifiers?.control || e.modifiers?.platform || e.modifiers?.alt);
    if (!plain) return;
    if (k === "/") { this.input.focus(); cx.stop_propagation(); return; }
    if (k === "down" || (k === "j" && !e.modifiers?.shift)) this.move(1, cx);
    else if (k === "up" || (k === "k" && !e.modifiers?.shift)) this.move(-1, cx);
    else if (k === "enter" || k === "o") {
      const iss = this.shown()[this.at];
      if (!iss) return;
      this.openIssue(iss);
    } else if (k === "r") this.load(cx);
    else if (k === "s") this.toggleState(cx);
    else if (k === "escape") { if (this.input.value()) this.clearFilters(cx); else return; }
    else return;
    cx.stop_propagation();
  }

  // A pill: tinted background, hairline border, colored text. Clickable ones toggle a
  // filter and read pressed while the filter is on.
  pill(text, color, t, onClick) {
    const hex = hexOf(color, null);
    const on = Boolean(onClick);
    return h_flex().h(17).px(7).gap(4).flex_none().items_center().rounded_full()
      .bg(hex ? alpha(hex, 0.16) : t.raised)
      .border(1).border_color(hex ? alpha(hex, 0.55) : t.border)
      .text_size(10).text_color(hex ?? t.muted)
      .when(on, (el) => el.cursor_pointer().hover((s) => s.bg(t.raised)).on_click(onClick))
      .child(div().max_w(100).truncate().child(text));
  }

  avatar(login, t) {
    const color = hue(login);
    const fallback = AvatarFallback.new().size_full().flex().items_center().justify_center()
      .bg(alpha(color, 0.25)).text_color(color).text_size(7.5).font_semibold()
      .child(two(login));
    const el = Avatar.new().w(16).h(16).flex_none().rounded_full().overflow_hidden();
    if (login && /^[a-zA-Z0-9-]+$/.test(login) && this.have[login]) {
      return el.image(AvatarImage.new(`${AVATAR_DIR}/${login}.png`).size_full().rounded_full().overflow_hidden()).fallback(fallback);
    }
    return el.fallback(fallback);
  }

  stateIcon(iss, t) {
    if (this.state === "closed") {
      if (iss.stateReason === "NOT_PLANNED") return div().text_size(11).flex_none().text_color(t.dim).child("✕");
      return div().text_size(11).flex_none().text_color(t.ansi5 ?? t.accent).child("✓");
    }
    return div().size(9).flex_none().rounded_full().border(1.5).border_color(t.ansi2 ?? t.working);
  }

  // A kanban card: raised, bordered, generous. Author line, wrapping pill line, and a
  // footer with the number and the triage button — which starts a pi on this issue in
  // a pane split to the right.
  row(iss, i, t) {
    const sel = i === this.at;
    const labels = iss.labels ?? [];
    const assignees = iss.assignees ?? [];
    const fields = this.fields?.[iss.number] ?? [];
    const stop = (_e, cx) => cx.stop_propagation();
    return v_flex().id(`ghi-${iss.number}`).p(11).gap(8).rounded(12).flex_none().cursor_pointer()
      .bg(t.raised).border(1).border_color(sel ? (t.accent ?? t.border) : t.border)
      .hover((s) => s.bg(t.hover))
      .on_click((_ev, cx) => { this.at = i; this.openIssue(iss); cx.notify(); })
      .child(h_flex().gap(9).items_start()
        .child(this.stateIcon(iss, t))
        .child(v_flex().flex_1().min_w(0).gap(3)
          .child(div().text_size(12.5).font_medium().text_color(t.foreground).child(iss.title))
          .child(h_flex().gap(6).items_center()
            .child(this.avatar(iss.author?.login, t))
            .child(div().text_size(10).text_color(t.dim).truncate().child(iss.author?.login ?? ""))
            .child(div().text_size(10).text_color(t.dim).child("·"))
            .child(div().text_size(10).text_color(t.dim).child(`updated ${ago(iss.updatedAt)} ago`))))
        .when(assignees.length, (el) => el.child(h_flex().gap(-4).flex_none().items_center()
          .children(assignees.slice(0, 3).map((a) => this.avatar(a.login, t)))
          .when(assignees.length > 3, (x) => x.child(div().text_size(9).text_color(t.dim).pl(6).child(`+${assignees.length - 3}`))))))
      .when(labels.length || iss.issueType || iss.milestone || fields.length, (el) => el.child(h_flex().gap(4).items_center().flex_wrap()
        .when(Boolean(iss.issueType), (x) => x.child(this.pill(iss.issueType.name, iss.issueType.color, t,
          (_e, cx) => { stop(); this.toggleFilter("types", iss.issueType.name, cx); })))
        .children(labels.slice(0, 3).map((l) => this.pill(l?.name ?? "", l?.color, t,
          (_e, cx) => { stop(); this.toggleFilter("labels", l?.name, cx); })))
        .when(labels.length > 3, (x) => x.child(this.pill(`+${labels.length - 3}`, null, t)))
        .when(Boolean(iss.milestone), (x) => x.child(this.pill(iss.milestone.title, null, t)))
        .children(fields.slice(0, 5).map((v) => this.pill(`${v.name}: ${v.value}`, v.color, t,
          (_e, cx) => { stop(); this.toggleFilter("fields", `${v.name}::${v.value}`, cx); })))))
      .child(h_flex().gap(6).items_center()
        .child(this.pill(`#${iss.number}`, null, t))
        .child(div().flex_1())
        .child(h_flex().id(`ghi-triage-${iss.number}`).h(22).px(9).gap(4).flex_none().items_center().rounded(7)
          .bg(alpha(t.accent ?? t.border, 0.14)).border(1).border_color(alpha(t.accent ?? t.border, 0.45))
          .text_color(t.accent ?? t.foreground).text_size(10.5).font_medium()
          .cursor_pointer().hover((s) => s.bg(alpha(t.accent ?? t.border, 0.28)))
          .on_mouse_down("left", stop)
          .on_click((_e, cx) => this.triage(iss, cx))
          .child(div().child("✦ triage"))));
  }

  activeFilterPills(t, cx) {
    const out = [];
    const chip = (kind, value, color) => h_flex().h(18).px(7).gap(4).flex_none().items_center().rounded_full()
      .bg(t.raised).border(1).border_color(t.border).text_size(10).text_color(t.foreground)
      .cursor_pointer().hover((s) => s.bg(t.hover))
      .on_click((_e, cx) => this.toggleFilter(kind, value, cx))
      .child(div().size(6).rounded_full().flex_none().bg(hexOf(color, t.muted)))
      .child(div().max_w(110).truncate().child(String(value).split("::").join(": ")))
      .child(div().text_color(t.dim).child("×"));
    for (const l of this.filters.labels) if (l === "*") out.push(chip("labels", "*", t.dim)); else {
      const known = (this.issues ?? []).flatMap((i) => i.labels ?? []).find((x) => x.name === l);
      out.push(chip("labels", l, known?.color));
    }
    for (const ty of this.filters.types) {
      const known = (this.issues ?? []).find((i) => i.issueType?.name === ty)?.issueType;
      out.push(chip("types", ty, known?.color));
    }
    for (const pe of this.filters.people) out.push(chip("people", pe, hue(pe)));
    return out;
  }

  // A facet's values over the fetched issues: [value, color, count], most used first.
  facetData(kind) {
    const counts = new Map();
    const push = (v, c) => { if (!v) return; const e = counts.get(v); if (e) e[2] += 1; else counts.set(v, [v, c, 1]); };
    for (const i of this.issues ?? []) {
      if (kind === "people") push(i.author?.login, hue(i.author.login));
      else if (kind === "types") push(i.issueType?.name ?? "None", hexOf(i.issueType?.color, null));
      else if (kind === "fields") { for (const vals of Object.values(this.fields ?? {})) for (const v of vals) push(`${v.name}::${v.value}`, v.color); }
      else for (const l of i.labels ?? []) push(l?.name, hexOf(l?.color, null));
    }
    return [...counts.values()].sort((a, b) => b[2] - a[2]).slice(0, 24);
  }

  openFacet(kind, cx) {
    const entries = this.facetData(kind);
    if (!entries.length) return;
    openOverlay({
      mod: "gh-issues", kind: "popup", width: 250,
      height: Math.min(340, 52 + 28 * entries.length),
      props: { facet: kind, entries, selected: this.filters[kind] ?? [], filters: JSON.parse(JSON.stringify(this.filters)) },
    });
  }

  // The popup copy of this mod: a facet menu. Picking toggles the value, mirrors the
  // filter set through shared state (the panel copy re-renders on it), and closes.
  menu(t) {
    const kind = this.p.facet;
    const title = { people: "Authors", types: "Issue types", labels: "Labels" }[kind] ?? "Filter";
    const sel = new Set(this.p.selected ?? []);
    const pick = (value) => {
      const base = { ...(shared(FKEY) ?? this.p.filters ?? {}) };
      const cur = [...(base[kind] ?? [])];
      const at = cur.indexOf(value);
      if (at >= 0) cur.splice(at, 1);
      else cur.push(value);
      base[kind] = cur;
      setShared(FKEY, base);
      closeOverlay(props().overlay);
    };
    const item = ([v, c, n]) => h_flex().h(26).px(8).gap(7).items_center().rounded(6).cursor_pointer()
      .hover((s) => s.bg(t.hover)).text_size(11.5).text_color(t.foreground)
      .on_click(() => pick(v))
      .child(div().size(7).flex_none().rounded_full().bg(c ?? t.dim))
      .child(div().flex_1().min_w(0).truncate().child(String(v).split("::").join(": ")))
      .child(div().text_size(10).text_color(t.dim).child(String(n)))
      .child(div().w(12).text_size(11).text_color(t.accent).child(sel.has(v) ? "✓" : ""));
    return v_flex().p(6).gap(2).text_color(t.foreground)
      .child(h_flex().px(6).pb(2).gap(6).items_center()
        .child(div().text_size(10).font_medium().text_color(t.dim).child(title.toUpperCase()))
        .child(div().flex_1())
        .child(div().id("ghi-menu-clear").h(16).px(6).flex_none().items_center().rounded(6).text_size(10)
          .text_color(t.dim).cursor_pointer().hover((s) => s.bg(t.hover))
          .on_click(() => {
            const base = { ...(shared(FKEY) ?? this.p.filters ?? {}) };
            base[kind] = [];
            setShared(FKEY, base);
            closeOverlay(props().overlay);
          }).child("clear")))
      .children((this.p.entries ?? []).map(item));
  }

  render() {
    const t = theme();
    if (this.p.facet) return this.menu(t);
    const sh = shared(FKEY);
    if (sh && JSON.stringify(sh) !== JSON.stringify(this.filters)) {
      this.filters = { labels: [], types: [], people: [], fields: [], ...sh };
      this.saveFilters();
      this.at = -1;
    }
    const stop = (_e, cx) => cx.stop_propagation();
    const shown = this.shown();
    const total = this.issues?.length ?? 0;
    const filtered = shown.length !== total;

    const statePill = h_flex().id("ghi-state").h(20).px(9).gap(5).flex_none().items_center().rounded_full()
      .bg(this.state === "open" ? alpha(t.ansi2 ?? t.working, 0.16) : alpha(t.ansi5 ?? t.accent, 0.16))
      .border(1).border_color(this.state === "open" ? alpha(t.ansi2 ?? t.working, 0.55) : alpha(t.ansi5 ?? t.accent, 0.55))
      .text_size(10.5).text_color(this.state === "open" ? (t.ansi2 ?? t.working) : (t.ansi5 ?? t.accent))
      .hover((s) => s.bg(t.raised))
      .on_mouse_down("left", stop).on_click((_e, cx) => this.toggleState(cx))
      .child(div().child(this.state));

    const head = v_flex().flex_none().px(8).pt(6).pb(7).gap(6)
      .child(h_flex().gap(6).items_center()
        .child(v_flex().flex_1().min_w(0).gap(1)
          .child(div().text_size(12).font_medium().truncate().text_color(this.repo ? t.foreground : t.dim)
            .child(this.repo ?? "GitHub Issues"))
          .child(div().text_size(10).text_color(t.dim)
            .child(this.issues ? (filtered ? `${shown.length} of ${total} ${this.state}` : `${total} ${this.state}`)
              : this.loading ? "loading…" : " ")))
        .child(statePill)
        .child(h_flex().id("ghi-refresh").size(24).flex_none().flex().items_center().justify_center().rounded_full()
          .text_color(t.dim).hover((s) => s.bg(t.hover).text_color(t.foreground))
          .on_mouse_down("left", stop).on_click((_e, cx) => this.load(cx))
          .child(this.loading
            ? svg("mods/workspace-chrome/spinner.svg").size(13).flex_none().text_color(t.working).spin(900)
            : div().text_size(14).child("⟳"))))
      .child(h_flex().h(26).px(8).gap(6).items_center().rounded_full()
        .bg(t.canvas ?? t.background).border(1).border_color(this.typing ? t.accent : t.input ?? t.border)
        .cursor_text().on_mouse_down("left", stop)
        .child(svg("mods/filetree/search.svg").size(12).flex_none().text_color(t.dim))
        .child(Input.new(this.input).flex_1().h_full().text_size(11.5).text_color(t.foreground))
        .when(Boolean(this.input.value()), (el) => el.child(div().id("ghi-clear").text_color(t.dim).cursor_pointer()
          .on_mouse_down("left", stop).on_click((_e, cx) => { this.input.set_value(""); cx.notify(); }).child("×"))))
      .when(Boolean(this.issues), (el) => el.child(h_flex().gap(4).items_center().flex_wrap()
        .children(FACETS.filter(([kind]) => kind !== "fields" || Object.keys(this.fields ?? {}).length).map(([kind, label]) => {
          const n = (this.filters[kind] ?? []).length;
          return h_flex().id(`ghi-facet-${kind}`).h(18).px(7).gap(3).flex_none().items_center().rounded_full()
            .cursor_pointer().text_size(10)
            .bg(n ? alpha(t.accent, 0.16) : t.raised)
            .border(1).border_color(n ? alpha(t.accent, 0.55) : t.border)
            .text_color(n ? t.accent : t.muted)
            .hover((s) => s.bg(t.hover))
            .on_mouse_down("left", stop)
            .on_click((_e, cx) => this.openFacet(kind, cx))
            .child(div().child(label))
            .when(Boolean(n), (x) => x.child(div().text_size(9).child(String(n))))
            .child(div().text_size(8).text_color(t.dim).child("▾"));
        }))
        .children(this.activeFilterPills(t))
        .when(this.activeFilterPills(t).length, (x) => x.child(div().id("ghi-clear-filters").h(18).px(6).flex_none().items_center().rounded_full().cursor_pointer()
          .text_size(10).text_color(t.dim).hover((s) => s.bg(t.hover))
          .on_click((_e, cx) => this.clearFilters(cx)).child("clear")))));

    const body = this.error && !this.issues
      ? div().px(10).py(8).text_size(11.5).text_color(t.blocked).child(this.error)
      : !this.issues
        ? div().px(10).py(8).text_size(11.5).text_color(t.dim).child(this.loading ? "Asking gh…" : "Nothing fetched yet — press ⟳ or r.")
        : !shown.length
          ? div().px(10).py(8).text_size(11.5).text_color(t.dim).child(total ? "No issue matches the search or filters." : `No ${this.state} issues.`)
          : div().flex_1().min_h(0).px(6).pb(6).gap(2).overflow_y_scrollbar()
            .children(shown.map((iss, i) => this.row(iss, i, t)));

    return v_flex().size_full().min_w(0).text_color(t.foreground).bg(t.surface)
      .track_focus(this.handle).on_key_down((e, cx) => this.key(e, cx))
      .child(head)
      .when(this.error && this.issues, (el) => el.child(div().px(10).py(3).text_size(10.5).text_color(t.blocked).child(this.error)))
      .child(body);
  }
}
