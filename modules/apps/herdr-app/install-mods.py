"""Install pinned mod sources while preserving Herdr's generated files and state.

  install-mods.py APP_HOME JJ_MODS OPEN_WITH FILE_MODS MODS_DIR LAYOUT [EXTRAS]
  install-mods.py --extras-only APP_HOME EXTRAS

EXTRAS is an optional JSON manifest of what a profile adds on top (paths are relative
to the manifest's own folder, or absolute):

  { "mods":     { "<name>": "<mod folder>" },
    "layouts":  { "<name>": "<layout json>" },
    "floating": [ <floating block>, ... ],   # added, by id, to the root of main-jj
                                             # and of every extra layout
    "patches":  [ <patch>, ... ],            # node edits, see patch_layout
    "keys":     { "<section>": { "<key>": <action> } } }

--extras-only installs just the manifest (and patches the installed main-jj layout),
for iterating on a profile's mods without a new generation.
"""

import json
import shutil
import sys
from pathlib import Path


def copy_tree(source, target):
    target.mkdir(parents=True, exist_ok=True)
    for path in source.rglob("*"):
        destination = target / path.relative_to(source)
        if path.is_dir():
            destination.mkdir(parents=True, exist_ok=True)
        elif not destination.exists() or destination.read_bytes() != path.read_bytes():
            shutil.copyfile(path, destination)


def write_if_changed(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists() or path.read_bytes() != data:
        path.write_bytes(data)


def find_node(node, node_id, parent=None):
    """(node, parent) for the node with that id, or (None, None)."""
    if node.get("id") == node_id:
        return node, parent
    for child in node.get("children", []):
        found = find_node(child, node_id, node)
        if found[0] is not None:
            return found
    return None, None


def patch_layout(layout, patch):
    """Applies one patch; each is idempotent, so re-running on its own output is a no-op.

    { "id": X, "append": [nodes] }  adds children to node X (same id: replaced)
    { "id": X, "wrap": node }       puts node in X's place, with X where node's
                                    children list "$self" (updated with "self": {...},
                                    e.g. a new size); skipped once node's id exists
    Optional "layouts": [names] limits it to those layouts. A missing X is skipped.
    """
    target, parent = find_node(layout, patch["id"])
    if target is None:
        return
    if "append" in patch:
        ids = {child.get("id") for child in patch["append"]}
        kept = [child for child in target.get("children", []) if child.get("id") not in ids]
        target["children"] = kept + patch["append"]
    if "wrap" in patch and find_node(layout, patch["wrap"].get("id"))[0] is None and parent is not None:
        target.update(patch.get("self", {}))
        wrapper = json.loads(json.dumps(patch["wrap"]))
        wrapper["children"] = [target if child == "$self" else child for child in wrapper.get("children", [])]
        siblings = parent["children"]
        siblings[siblings.index(target)] = wrapper


def with_extras(source_bytes, extras, name):
    """The layout with the manifest's floating blocks and patches for it."""
    blocks = extras.get("floating", [])
    patches = [p for p in extras.get("patches", []) if name in p.get("layouts", [name])]
    if not blocks and not patches:
        return source_bytes
    layout = json.loads(source_bytes)
    if blocks:
        ids = {block.get("id") for block in blocks}
        kept = [block for block in layout.get("floating", []) if block.get("id") not in ids]
        layout["floating"] = kept + blocks
    for patch in patches:
        patch_layout(layout, patch)
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
        "patches": manifest.get("patches", []),
        "keys": manifest.get("keys", {}),
    }


def install_extras(app_home, extras):
    for name, source in extras.get("mods", {}).items():
        copy_tree(source, app_home / "mods" / name)
    for name, source in extras.get("layouts", {}).items():
        data = with_extras(source.read_bytes(), extras, name)
        write_if_changed(app_home / "layouts" / f"{name}.json", data)


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
            write_if_changed(main_layout, with_extras(main_layout.read_bytes(), extras, "main-jj"))
        merge_keys(app_home, extras["keys"])
        return

    app_home, jj_mods, open_with, file_mods, mods_dir, layout_source = map(Path, argv[:6])
    extras = load_extras(Path(argv[6]) if len(argv) > 6 else None)
    mods = {
        name: jj_mods / name for name in ("jj-changes", "jj-menu", "jj-workspace")
    }
    mods.update({"open-with": open_with, "spaces-jj": mods_dir / "spaces-jj"})
    # Local mods: GitHub issues panel, and the tabs bar fork that gives it a button.
    mods.update({"gh-issues": mods_dir / "gh-issues", "tabs-jj": mods_dir / "tabs-jj"})
    mods.update({name: file_mods / name for name in ("file-icons", "filetree")})
    for name, source in mods.items():
        copy_tree(source, app_home / "mods" / name)

    layout = app_home / "layouts" / "main-jj.json"
    write_if_changed(layout, with_extras(layout_source.read_bytes(), extras, "main-jj"))
    install_extras(app_home, extras)

    bindings = {
        "ctrl-alt-o": "mod.open:open-with",
        "ctrl-shift-g": 'layout.select:{"stack":"panel-body","child":"gh-issues"}',
        "ctrl-alt-i": "mod.open:gh-issues",
    }
    merge_keys(app_home, extras.get("keys", {}), base={"global": bindings})


if __name__ == "__main__":
    main(sys.argv[1:])
