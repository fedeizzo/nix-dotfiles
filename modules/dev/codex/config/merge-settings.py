"""Apply declarative defaults while preserving Codex's runtime settings."""

import json
import os
from pathlib import Path
import sys
import tempfile

import tomlkit


def merge(target, defaults):
    for key, value in defaults.items():
        if isinstance(value, dict):
            if key not in target or not isinstance(target[key], dict):
                target[key] = tomlkit.table()
            merge(target[key], value)
        else:
            target[key] = value


def main():
    path = Path(sys.argv[1])
    defaults = json.loads(Path(sys.argv[2]).read_text())
    document = tomlkit.parse(path.read_text()) if path.exists() else tomlkit.document()
    merge(document, defaults)
    path.parent.mkdir(parents=True, exist_ok=True)
    # Replace the directory entry rather than following a former Nix store symlink.
    fd, temporary = tempfile.mkstemp(prefix=".codex-config-", dir=path.parent)
    try:
        with os.fdopen(fd, "w") as output:
            output.write(tomlkit.dumps(document))
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


if __name__ == "__main__":
    main()
