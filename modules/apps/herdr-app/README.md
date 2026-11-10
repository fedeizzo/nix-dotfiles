# Herdr-app mods

The Home Manager activation copies pinned mods into real folders so Herdr can
hot-reload them. `main-jj` uses the jj Changes panel and a patched spaces sidebar.
Select it with `herdr-app do layout.switch '{"name":"main-jj"}'`.
Its Files button opens `filetree`, with Material Icon Theme assets from
`file-icons`. The standalone `file-icons` panel is also installed. Filetree's
optional Claude activity hooks are not installed; Tree and previews work without
them, but Recent/activity highlighting requires configuring those hooks.
Open With uses **Ctrl+Alt+O**, preserving Ctrl+Shift+O for splitting panes.

## Updates

From the repository root:

```sh
nix flake update herdr-app-jj herdr-kanso herdr-mods
```

Then apply the configuration with your usual NixOS/Home Manager rebuild.
Activation copies changed files; there is no separate mod update command.
For just one repository, pass only its input name. Review `jj diff flake.lock`
before applying. The Kanso input supplies only `open-with`, not the whole layout.
The `herdr-mods` input updates Filetree and File Icons together.

Local mods live in `mods/` (`spaces-jj`, `tabs-jj`, `gh-issues`); `main-jj.json`
sits next to the installer. `spaces-jj` and `tabs-jj` are local clones of
Herdr-app 0.1.13, `spaces-jj` carrying the sidebar changes from herdr-app-jj
revision `48be7c658c2f0a47e1652046bf93542a26ade135`. They do not update automatically:
review the upstream sidebar patch when updating jj, and refresh the clones when
changing Herdr's layout/sidebar behavior. The other jj mods update from the input.

`tabs-jj` is a clone of the shipped `tabs` mod whose only change is an extra
panel button for `gh-issues` (GitHub issues of the focused pane's repo, via
`gh`). `gh-issues` also self-registers into `shared("workspace.panels")`, so
removing it hides its button again. The layout points its `tabs` block at
`tabs-jj`.

## gh-issues

Kanban cards for the repo of the focused pane: state, author (round GitHub
avatars baked with ffmpeg), labels, issue type, milestone, and org issue-type
fields (read through a batched GraphQL search, since `gh issue list` cannot
return them — silently skipped where the org has none). The list paints as soon
as `gh issue list` answers; fields and avatars enrich it in the background.
A search bar, facet popups (authors / type / labels / fields) and clickable
pills filter client-side; filters persist in `shared("gh-issues.filters")`.

Triage: every card has `✦ triage` (single pi), and cards are checkable —
the selection bar's `✦ triage N` feeds all checked issues to one pi session,
in a pane split to the right, with a prompt that investigates labels/type/
fields and duplicates but acts only with the user's approval. Edit the prompt
in the `TRIAGE` / `TRIAGE_BATCH` constants at the top of `main.js`.
Keys with the panel focused: `/` search, `j/k` or arrows move, `x` checks the
row, `o`/Enter opens in the browser, `r` refresh, `s` open/closed, `Esc` clears
search or filters. Layout keys: `Ctrl+Shift+G` shows the panel, `Ctrl+Alt+I`
opens it as a floating window.

The installer owns these mod source files and `layouts/main-jj.json`; edit their
repository copies, since activation reapplies them. Keys are merged; app state
and generated type files are preserved. Removing a mod from the installer does
not delete a previously copied folder; remove that folder separately.

Validate updates with `herdr-app check` for `jj-changes`, `jj-menu`,
`jj-workspace`, `spaces-jj`, `tabs-jj`, `gh-issues`, `open-with`, `filetree`,
and `file-icons`, then inspect `herdr-app status`.

gh-issues needs `gh` (authenticated) in the app's environment; avatar baking
additionally uses `curl` and `ffmpeg` — the panel degrades to initials
bubbles when those are unavailable.

## Profile extras

A profile can add mods, layouts, keys and floating blocks on top of the shared
set through `fi.herdrApp.{extraMods,extraLayouts,extraKeys,extraFloating}`.
The module writes them to a JSON manifest and passes it to the installer as an
optional last argument; with nothing set, the installer runs exactly as before.
Floating blocks are added (by id) to the root of `main-jj` and of every extra
layout, for mods that must run whatever layout is shown. Extra keys never
replace a key the user already bound (the installer says so on stderr).

The work profile uses this for the ops mods; see
`modules/profiles/work/herdr-app/README.md`. To try a manifest without a new
generation, install only the extras straight into the app's folder:

```sh
python3 modules/apps/herdr-app/install-mods.py --extras-only \
  ~/.config/herdr-app modules/profiles/work/herdr-app/extras.json
herdr-app check ~/.config/herdr-app/mods/ops && herdr-app status
```

Run `herdr-app check` on the installed copy, not the repository folder: the
check writes `gpui-kit.d.ts` and `jsconfig.json` next to the mod.
