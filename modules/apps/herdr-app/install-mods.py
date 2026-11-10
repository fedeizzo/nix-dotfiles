"""Install pinned mod sources while preserving Herdr's generated files and state."""

import json
import shutil
import sys
from pathlib import Path

app_home, jj_mods, open_with, file_mods, mods_dir, layout_source = map(Path, sys.argv[1:])
mods = {
    name: jj_mods / name for name in ("jj-changes", "jj-menu", "jj-workspace")
}
mods.update({"open-with": open_with, "spaces-jj": mods_dir / "spaces-jj"})
# Local mods: GitHub issues panel, and the tabs bar fork that gives it a button.
mods.update({"gh-issues": mods_dir / "gh-issues", "tabs-jj": mods_dir / "tabs-jj"})
mods.update({name: file_mods / name for name in ("file-icons", "filetree")})

for name, source in mods.items():
    target = app_home / "mods" / name
    target.mkdir(parents=True, exist_ok=True)
    for path in source.rglob("*"):
        destination = target / path.relative_to(source)
        if path.is_dir():
            destination.mkdir(parents=True, exist_ok=True)
        elif not destination.exists() or destination.read_bytes() != path.read_bytes():
            shutil.copyfile(path, destination)

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
