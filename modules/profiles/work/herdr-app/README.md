# Ops mods (work profile)

Herdr-app additions for an orchestrator workflow: one pi session (the
orchestrator) triages tasks kept as markdown in a vault and delegates work to
agents it starts with `hc`. Installed only where the work profile and the
herdr-app module are both imported (`herdr-app.h.nix` next to this folder);
other hosts are unaffected.

Everything is declared in `extras.json` (mods, the `ops` layout, a floating
block with the settings, layout patches, keys). Nix reads it through
`fi.herdrApp.extra*`; the installer can also read it directly.

## What it does

The `ops` mod has five roles, chosen by `props.role`:

- **watch** (an invisible floating block added to `main-jj` and `ops`):
  toasts when a terminal labeled `role=delegate` or `role=standing` is
  blocked, errors, finishes a turn after working, or exits. A state must hold
  2 s, the same state is never toasted twice, and nothing is toasted for what
  was already true at start or while you look at that terminal. Clicking
  focuses the terminal (or peeks at a sub-agent without a pane). Once a day it
  also toasts `N due today · M overdue`; clicking opens the calendar. It
  answers the key commands below.
- **board** (default; the right column of the `ops` layout, or a window with
  `mod.open:ops`): Due (a month calendar with dots: red overdue, accent today,
  muted upcoming), Delegations (the registry joined with live hc status) and
  the Now / Next / Waiting / Inbox lanes. A task opens its note; a delegation
  focuses its terminal. Labeled agents missing from the registry show as
  "not in registry".
- **note**: a markdown viewer pane for task notes; wikilinks open in place.
  The board reuses one per tab.
- **calendar**: the Due calendar as a window.
- **button**: an "Ops" button right of the tab bar's Files / Changes. A layout
  patch wraps `main-jj`'s `tabs` block in a row with it and adds an `ops` child
  to the `panel-body` stack, so it opens and closes the board in the side panel
  exactly like those (one panel at a time, remembered per space by tabs-jj).
  In the `ops` layout it shows and hides the board column instead.

Keys (`extras.json`; none replaces a key you already bound):

| Key | Action |
|---|---|
| `cmd-shift-o` | `ops.orchestrator`: focus `role=orchestrator`, else start one in the vault |
| `cmd-shift-e` | `ops.emails`: focus or start the `emails` standing agent |
| `cmd-shift-b` | board window (`layout:ops`: show/hide the board column) |
| `cmd-shift-k` | calendar window |

Commands work wherever the watcher runs (`main-jj`, `ops`); in another layout
an open board window answers them.

## The vault contract

The board parses (read-only) `ops/Tasks.md` (`## <lane>` headings, task
bullets `- **T-NNN** · title · field · …`, optional `due: YYYY-MM-DD`),
`ops/Delegations.md` (tables under `## Active` / `## Standing agents`, agent
name in backticks in the first column) and `ops/tasks/*.md` (frontmatter
`task`, `status`, `owner`, `due`, …; the board line wins over the note).
Terminals are matched by the hc labels `task=`, `role=`, `agent=`, then by
terminal name, then by the `hc tN` address in the registry. Missing files,
labels or fields show less, never an error loop.

## Settings

On the floating `ops-watch` block's props in `extras.json`; the watcher
publishes them to the other copies through `shared("ops.config")`:

- `vault`: the vault folder (default `~/notes/notes`).
- `sound`: `false`, `true`, or a list of kinds (`blocked`, `error`, `done`,
  `exited`). Herdr already plays its own sound for agents in the window.
- `orchestrator`: `{ argv, cwd? }` used when no orchestrator runs.
- `standing`: `{ <command>: { title, name, cwd, argv, labels? } }`. Each entry
  is a board button and an `ops.<command>` key command; started with
  `role=standing`, `agent=<name>` and the current space's `herdr.space`.

## Iterating

```sh
python3 modules/apps/herdr-app/install-mods.py --extras-only \
  ~/.config/herdr-app modules/profiles/work/herdr-app/extras.json
herdr-app check ~/.config/herdr-app/mods/ops && herdr-app status
herdr-app do layout.switch --name ops   # or: herdr-app do ops.calendar
```

Saving copies hot-reloads every copy. Check the installed folder: the check
writes type files next to the mod it checks.
