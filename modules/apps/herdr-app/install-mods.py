"""Install pinned mod sources while preserving Herdr's generated files and state.

  install-mods.py APP_HOME JJ_MODS OPEN_WITH FILE_MODS MODS_DIR USAGE_LIMITS ACCOUNTS LAYOUT [EXTRAS]
  install-mods.py --extras-only APP_HOME EXTRAS

EXTRAS is an optional JSON manifest of what a profile adds on top (paths are relative
to the manifest's own folder, or absolute):

  { "mods":     { "<name>": "<mod folder>" },
    "layouts":  { "<name>": "<layout json>" },
    "floating": [ <floating block>, ... ],   # added, by id, to the root of main-jj
                                             # and of every extra layout
    "keys":     { "<section>": { "<key>": <action> } } }

--extras-only installs just the manifest (and patches the installed main-jj layout),
for iterating on a profile's mods without a new generation.
"""

import json
import shlex
import shutil
import sys
from pathlib import Path


def copy_tree(sources, target, skip=()):
    """Copies each source folder over target, later ones winning; skips top-level `skip`."""
    target.mkdir(parents=True, exist_ok=True)
    for source, path in ((s, p) for s in sources for p in s.rglob("*")):
        if path.relative_to(source).parts[0] in skip:
            continue
        destination = target / path.relative_to(source)
        if path.is_dir():
            destination.mkdir(parents=True, exist_ok=True)
        elif not destination.exists() or destination.read_bytes() != path.read_bytes():
            shutil.copyfile(path, destination)


def write_if_changed(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists() or path.read_bytes() != data:
        path.write_bytes(data)


def with_floating(source_bytes, blocks):
    """The layout with `blocks` on its root's floating list (same id: replaced)."""
    if not blocks:
        return source_bytes
    layout = json.loads(source_bytes)
    ids = {block.get("id") for block in blocks}
    kept = [block for block in layout.get("floating", []) if block.get("id") not in ids]
    layout["floating"] = kept + blocks
    return (json.dumps(layout, indent=2, ensure_ascii=False) + "\n").encode()


def load_extras(path):
    if path is None:
        return {}
    manifest = json.loads(path.read_text())
    base = path.parent
    resolve = lambda value: (base / value).resolve()
    return {
        "mods": {name: resolve(value) for name, value in manifest.get("mods", {}).items()},
        "layouts": {name: resolve(value) for name, value in manifest.get("layouts", {}).items()},
        "floating": manifest.get("floating", []),
        "keys": manifest.get("keys", {}),
    }


def install_extras(app_home, extras):
    for name, source in extras.get("mods", {}).items():
        copy_tree([source], app_home / "mods" / name)
    for name, source in extras.get("layouts", {}).items():
        data = with_floating(source.read_bytes(), extras.get("floating", []))
        write_if_changed(app_home / "layouts" / f"{name}.json", data)


# usage-limits: upstream ships one Claude and one Codex provider. Generate one
# provider per account, each with a wrapper helper that pins the CLI's home
# (CLAUDE_CONFIG_DIR / CODEX_HOME) before running the upstream helper.
ENV = {"claude": "CLAUDE_CONFIG_DIR", "codex": "CODEX_HOME"}  # agy has none
HELPER = {
    "claude": "claude-usage.sh",
    "codex": "codex-limits.sh",
    "antigravity": "antigravity-quota.sh",
}


def install_usage_accounts(usage_dir, accounts):
    entries = []
    for account in accounts:
        lines = ["#!/bin/sh"]
        if account.get("home"):
            if account["provider"] not in ENV:
                sys.exit(f"herdr-app: {account['provider']} accounts cannot set home")
            lines.append(f"export {ENV[account['provider']]}={shlex.quote(account['home'])}")
        lines.append(f'exec sh "$(dirname "$0")/{HELPER[account["provider"]]}"')
        helper = f"helpers/usage-{account['id']}.sh"
        write_if_changed(usage_dir / helper, ("\n".join(lines) + "\n").encode())
        entries.append(
            f"  {{ ...base.{account['provider']}, id: {json.dumps(account['id'])}, "
            f"name: {json.dumps(account['name'])}, helper: {json.dumps(helper)} }},"
        )
    write_if_changed(
        usage_dir / "providers" / "index.js",
        (
            'import claude from "./claude.js";\nimport codex from "./codex.js";\n'
            'import antigravity from "./antigravity.js";\n\n'
            "const base = { claude, codex, antigravity };\n"
            "export const PROVIDERS = [\n" + "\n".join(entries) + "\n];\n"
        ).encode(),
    )


def merge_keys(app_home, sections, base=None):
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
        return
    keys = json.loads(keys_path.read_text())
    for section, bindings in (base or {}).items():
        current = keys.setdefault(section, {})
        for key, action in bindings.items():
            current.setdefault(key, action)
    # A profile's extra keys: same rule, but say when one is already taken.
    for section, bindings in sections.items():
        current = keys.setdefault(section, {})
        for key, action in bindings.items():
            if key in current and current[key] != action:
                print(f"herdr-app: {section} {key} is already bound; kept the existing binding", file=sys.stderr)
            current.setdefault(key, action)
    text = json.dumps(keys, indent=2, ensure_ascii=False) + "\n"
    if keys_path.read_text() != text:
        keys_path.write_text(text)


def main(argv):
    if argv[:1] == ["--extras-only"]:
        app_home, manifest = map(Path, argv[1:])
        extras = load_extras(manifest)
        install_extras(app_home, extras)
        main_layout = app_home / "layouts" / "main-jj.json"
        if main_layout.exists():
            write_if_changed(main_layout, with_floating(main_layout.read_bytes(), extras["floating"]))
        merge_keys(app_home, extras["keys"])
        return

    app_home, jj_mods, open_with, file_mods, mods_dir, usage_limits, accounts_path, layout_source = map(
        Path, argv[:8]
    )
    extras = load_extras(Path(argv[8]) if len(argv) > 8 else None)
    mods = {
        name: [jj_mods / name] for name in ("jj-changes", "jj-menu", "jj-workspace")
    }
    mods.update({"open-with": [open_with], "spaces-jj": [mods_dir / "spaces-jj"]})
    # Local mods: GitHub issues panel, and the tabs bar fork that gives it a button.
    mods.update({"gh-issues": [mods_dir / "gh-issues"], "tabs-jj": [mods_dir / "tabs-jj"]})
    # Upstream usage-limits, with local files laid over it (the Antigravity provider).
    mods.update({"usage-limits": [usage_limits, mods_dir / "usage-limits"]})
    mods.update({name: [file_mods / name] for name in ("file-icons", "filetree")})
    for name, sources in mods.items():
        skip = ("test",) if name == "usage-limits" else ()
        copy_tree(sources, app_home / "mods" / name, skip)

    install_usage_accounts(app_home / "mods" / "usage-limits", json.loads(accounts_path.read_text()))

    layout = app_home / "layouts" / "main-jj.json"
    write_if_changed(layout, with_floating(layout_source.read_bytes(), extras.get("floating", [])))
    install_extras(app_home, extras)

    bindings = {
        "ctrl-alt-o": "mod.open:open-with",
        "ctrl-shift-g": 'layout.select:{"stack":"panel-body","child":"gh-issues"}',
        "ctrl-alt-i": "mod.open:gh-issues",
    }
    merge_keys(app_home, extras.get("keys", {}), base={"global": bindings})


if __name__ == "__main__":
    main(sys.argv[1:])
