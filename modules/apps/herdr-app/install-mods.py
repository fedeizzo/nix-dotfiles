"""Install pinned mod sources while preserving Herdr's generated files and state."""

import json
import shutil
import sys
from pathlib import Path

app_home, jj_mods, open_with, file_mods, assets = map(Path, sys.argv[1:])
mods = {
    name: jj_mods / name for name in ("jj-changes", "jj-menu", "jj-workspace")
}
mods.update({"open-with": open_with, "spaces-jj": assets / "spaces-jj"})
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
source = assets / "main-jj.json"
if not layout.exists() or layout.read_bytes() != source.read_bytes():
    shutil.copyfile(source, layout)

# Merge one binding; retain all the user's other keys.
keys_path = app_home / "keys.json"
keys = json.loads(keys_path.read_text()) if keys_path.exists() else {}
keys.setdefault("global", {})["ctrl-alt-o"] = "mod.open:open-with"
text = json.dumps(keys, indent=2, ensure_ascii=False) + "\n"
if not keys_path.exists() or keys_path.read_text() != text:
    keys_path.write_text(text)
