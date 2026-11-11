"""Install pinned mod sources while preserving Herdr's generated files and state."""

import json
import shlex
import shutil
import sys
from pathlib import Path

app_home, jj_mods, open_with, file_mods, mods_dir, usage_limits, accounts_path, layout_source = map(
    Path, sys.argv[1:]
)
mods = {
    name: jj_mods / name for name in ("jj-changes", "jj-menu", "jj-workspace")
}
mods.update({"open-with": open_with, "spaces-jj": mods_dir / "spaces-jj"})
# Local mods: GitHub issues panel, and the tabs bar fork that gives it a button.
mods.update({"gh-issues": mods_dir / "gh-issues", "tabs-jj": mods_dir / "tabs-jj"})
mods.update({"usage-limits": usage_limits})
# Local files laid over upstream usage-limits (the Antigravity provider).
overlays = {"usage-limits": mods_dir / "usage-limits"}
mods.update({name: file_mods / name for name in ("file-icons", "filetree")})

for name, source in mods.items():
    target = app_home / "mods" / name
    target.mkdir(parents=True, exist_ok=True)
    sources = [source] + ([overlays[name]] if name in overlays else [])
    for source, path in ((s, p) for s in sources for p in s.rglob("*")):
        if name == "usage-limits" and path.relative_to(source).parts[0] == "test":
            continue
        destination = target / path.relative_to(source)
        if path.is_dir():
            destination.mkdir(parents=True, exist_ok=True)
        elif not destination.exists() or destination.read_bytes() != path.read_bytes():
            shutil.copyfile(path, destination)

# usage-limits: upstream ships one Claude and one Codex provider. Generate one
# provider per account, each with a wrapper helper that pins the CLI's home
# (CLAUDE_CONFIG_DIR / CODEX_HOME) before running the upstream helper.
ENV = {"claude": "CLAUDE_CONFIG_DIR", "codex": "CODEX_HOME"}  # agy has none
HELPER = {
    "claude": "claude-usage.sh",
    "codex": "codex-limits.sh",
    "antigravity": "antigravity-quota.sh",
}


def write_if_changed(path, text):
    if not path.exists() or path.read_text() != text:
        path.write_text(text)


usage_dir = app_home / "mods" / "usage-limits"
accounts = json.loads(accounts_path.read_text())
entries = []
for account in accounts:
    lines = ["#!/bin/sh"]
    if account.get("home"):
        if account["provider"] not in ENV:
            sys.exit(f"herdr-app: {account['provider']} accounts cannot set home")
        lines.append(f"export {ENV[account['provider']]}={shlex.quote(account['home'])}")
    lines.append(f'exec sh "$(dirname "$0")/{HELPER[account["provider"]]}"')
    helper = f"helpers/usage-{account['id']}.sh"
    write_if_changed(usage_dir / helper, "\n".join(lines) + "\n")
    entries.append(
        f"  {{ ...base.{account['provider']}, id: {json.dumps(account['id'])}, "
        f"name: {json.dumps(account['name'])}, helper: {json.dumps(helper)} }},"
    )
write_if_changed(
    usage_dir / "providers" / "index.js",
    'import claude from "./claude.js";\nimport codex from "./codex.js";\n'
    'import antigravity from "./antigravity.js";\n\n'
    "const base = { claude, codex, antigravity };\n"
    "export const PROVIDERS = [\n" + "\n".join(entries) + "\n];\n",
)

layout = app_home / "layouts" / "main-jj.json"
layout.parent.mkdir(parents=True, exist_ok=True)
source = layout_source
if not layout.exists() or layout.read_bytes() != source.read_bytes():
    shutil.copyfile(source, layout)

# Merge our bindings; retain all the user's other keys. Never create the file:
# Herdr ships its defaults only into a keys.json it wrote itself, and treats any
# pre-existing one as personal config, so seeding it before the app's first run
# would cost every default binding -- copy and paste included.
keys_path = app_home / "keys.json"
if not keys_path.exists():
    print(
        "herdr-app: no keys.json yet; start the app once, then re-activate to "
        "add the open-with and gh-issues bindings",
        file=sys.stderr,
    )
    sys.exit(0)

keys = json.loads(keys_path.read_text())
bindings = keys.setdefault("global", {})
bindings.setdefault("ctrl-alt-o", "mod.open:open-with")
bindings.setdefault("ctrl-shift-g", 'layout.select:{"stack":"panel-body","child":"gh-issues"}')
bindings.setdefault("ctrl-alt-i", "mod.open:gh-issues")
text = json.dumps(keys, indent=2, ensure_ascii=False) + "\n"
if not keys_path.exists() or keys_path.read_text() != text:
    keys_path.write_text(text)
