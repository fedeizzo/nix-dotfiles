#!/usr/bin/env python3
import os
import re
import sys
from pathlib import Path

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 scripts/list-enabled-modules.py <hostname>")
        sys.exit(1)

    hostname = sys.argv[1]
    modules_dir = Path("modules")
    
    # Mapping: (class, name) -> list of files
    module_files = {}
    
    # Mapping: (class, name) -> set of (class, name) that it imports
    module_imports = {}

    # Regex to find declarations: flake.modules.<class>.<name>
    decl_regex = re.compile(r'flake\.modules\.([a-zA-Z]+)\.([a-zA-Z0-9_-]+)')
    
    # Regex to find imports: (inputs\.)?self\.modules\.<class>\.<name>
    import_regex = re.compile(r'(?:inputs\.)?self\.modules\.([a-zA-Z]+)\.([a-zA-Z0-9_-]+)')

    # Regex to find configurations: flake.nixosConfigurations.<hostname>
    config_regex = re.compile(r'flake\.(?:nixos|darwin)Configurations\.([a-zA-Z0-9_-]+)')

    # Step 1: Scan all files to find declarations, imports, and configurations
    hostname_roots = {} # hostname -> set of (cls, name) root modules

    for root, _, files in os.walk(modules_dir):
        for f in files:
            if not f.endswith('.nix'): continue
            path = Path(root) / f
            try:
                content = path.read_text(encoding='utf-8')
            except UnicodeDecodeError:
                continue

            decls = decl_regex.findall(content)
            imports = set(import_regex.findall(content))
            configs = config_regex.findall(content)

            for cls, name in decls:
                module_files.setdefault((cls, name), []).append(path)
                module_imports.setdefault((cls, name), set()).update(imports)

            for host in configs:
                # Assuming the configuration file imports the root modules directly
                # e.g. modules = [ self.modules.nixos.x1-nano ]
                hostname_roots.setdefault(host, set()).update(imports)

    roots = hostname_roots.get(hostname)
    
    if not roots:
        # Fallback: if the hostname matches a module name directly (e.g., 'homelab')
        roots = [(cls, name) for cls, name in module_files.keys() if name == hostname]
        
    if not roots:
        print(f"Could not find configuration or root modules for hostname '{hostname}'.")
        sys.exit(1)

    visited = set()
    
    def traverse(node):
        if node in visited:
            return
        visited.add(node)
        for child in module_imports.get(node, set()):
            traverse(child)

    for r in roots:
        traverse(r)
        
    print(f"🌳 Enabled dendritic modules for host '{hostname}':\n")
    
    by_class = {}
    for cls, name in visited:
        by_class.setdefault(cls, []).append(name)
        
    for cls in sorted(by_class.keys()):
        print(f"[{cls}]")
        for name in sorted(by_class[cls]):
            files = module_files.get((cls, name), [])
            file_paths = ", ".join(str(f) for f in files)
            prefix = "🟢" if (cls, name) in roots else "  "
            print(f"  {prefix} {name}")
            if file_paths:
                print(f"       {file_paths}")
            else:
                print(f"       (External or unresolved)")
        print("")

if __name__ == '__main__':
    main()
