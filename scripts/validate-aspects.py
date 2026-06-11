#!/usr/bin/env python3
import os
import re
import sys

def main():
    if len(sys.argv) < 2:
        return

    files = sys.argv[1:]
    errors = []

    for file in files:
        basename = os.path.basename(file)
        if not basename.endswith('.nix') or basename == 'default.nix':
            continue

        parts = basename.split('.')
        if len(parts) < 3:
            continue

        classes_str = parts[-2]
        implied_classes = set()
        
        # Parse the classes
        if 'n' in classes_str: implied_classes.add('nixos')
        if 'h' in classes_str: implied_classes.add('homeManager')
        if 'd' in classes_str: implied_classes.add('darwin')
        if 'k' in classes_str: implied_classes.add('disko')
        if 'g' in classes_str: implied_classes.add('generic')

        with open(file, 'r') as f:
            content = f.read()

        # Find all declared classes
        declared = set(re.findall(r'flake\.modules\.([a-zA-Z]+)', content))

        if implied_classes != declared:
            errors.append(
                f"ERROR: File {file} named with '.{classes_str}.nix' implies classes {implied_classes}.\n"
                f"       But the file contains: {declared or 'None'}.\n"
                f"       Please rename the file or fix the flake.modules.<class> declarations."
            )

    if errors:
        print("\n".join(errors))
        sys.exit(1)

if __name__ == '__main__':
    main()
