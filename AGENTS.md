# AGENTS.md — Module Architecture Guide

This document describes the flake module system for LLM/agent navigation.

## Top-Level Pattern

Every module file under `modules/` declares:

1. **`flake-file.inputs.<name>`** — external flake references
2. **`flake.modules.<platform>.<name>`** — the actual module definition

```nix
# Example: modules/apps/voxtype.h.nix
{
  # 1. External flake input
  flake-file.inputs.voxtype.url = "github:peteonrails/voxtype/v1.0.0-rc1";

  # 2. Platform-specific modules
  flake.modules.homeManager.voxtype = { pkgs, inputs, ... }: {
    imports = [ inputs.voxtype.homeManagerModules.default ];
    programs.voxtype = { enable = true; /* ... */ };
  };
}
```

## File Locations

| Path | Purpose |
|------|---------|
| `modules/apps/` | Desktop apps (voxtype, zed, ghostty, etc.) |
| `modules/apps/desktop-environment/` | Niri, DMS, keybindings |
| `modules/services/` | Server services (traefik, postgres, immich, etc.) |
| `modules/home/` | Home-manager specific modules |
| `modules/infra/` | Infrastructure (backrest, blocky, etc.) |
| `modules/observability/` | Monitoring (gatus, glance, grafana, prometheus) |
| `modules/security/` | Authentik, etc. |
| `modules/system/` | NixOS system config (boot, disk, networking, etc.) |
| `modules/hosts/` | Per-host configurations (oven, homelab, etc.) |

## Module Types

Three entry points per module:

| Key | Platform | Purpose |
|-----|----------|---------|
| `flake.modules.nixos.<name>` | NixOS | System-level config |
| `flake.modules.homeManager.<name>` | Home Manager | User-level config |
| `flake.modules.darwin.<name>` | macOS | Darwin config |

Each receives `{ pkgs, lib, inputs, config, username, ... }`.

## Composing Modules

### Hosts (NixOS)

Hosts under `modules/hosts/` import modules and build configurations:

```nix
# modules/hosts/x1-nano.d/configuration.n.nix
{
  flake.nixosConfigurations.oven = inputs.nixpkgs.lib.nixosSystem {
    modules = [
      self.modules.nixos.x1-nano
    ];
  };

  flake.modules.nixos.x1-nano = { modulesPath, ... }: {
    imports = [
      inputs.self.modules.nixos.boot
      inputs.self.modules.nixos.networking
      inputs.self.modules.nixos.desktop-environment
      # ...
    ];
  };
}
```

### Home Manager Users

Individual users import modules via `inputs.self.modules.homeManager`:

```nix
# modules/hosts/x1-nano.d/users/oven.n.nix
users.${username} = {
  imports = with inputs.self.modules.homeManager; [
    desktop-environment
    voxtype
    zed
    git
    # ...
  ];
};
```

## Adding a New Module

1. Create `modules/apps/<name>.h.nix` (or `system/`, `services/`, etc.)
2. Follow the module pattern:
   - Declare `flake-file.inputs.<name>.url = "github:..."`
   - Define `flake.modules.homeManager.<name>` (or nixos/darwin)
   - Import external flake's module: `imports = [ inputs.<name>.homeManagerModules.default ]`
   - Set config: `programs.<name> = { enable = true; /* ... */ };`

## Naming Conventions

| Suffix | Meaning |
|--------|---------|
| `.h.nix` | Home Manager module (most common) |
| `.n.nix` | NixOS module |
| `.nh.nix` | Both NixOS + Home Manager |
| `.d.nix` | Darwin module |

## Key Files to Know

| File | Purpose |
|------|---------|
| `flake.nix` | Top-level flake definition |
| `modules/hosts/x1-nano.d/configuration.n.nix` | Main host config |
| `modules/hosts/x1-nano.d/users/oven.n.nix` | User config for 'oven' |
| `modules/apps/desktop-environment/desktop-environment.nh.nix` | Niri/DMS setup |
| `docs/MODULES.md` | Detailed module reference |

## Common Patterns

### Importing an External Flake Module

```nix
flake.modules.homeManager.<name> = { pkgs, inputs, ... }: {
  imports = [ inputs.<external-flake>.homeManagerModules.default ];
  programs.<name> = { /* config */ };
};
```

### Adding a Package

```nix
flake.modules.homeManager.<name> = { pkgs, ... }: {
  home.packages = [ pkgs.<package> ];
};
```

### System Service

```nix
flake.modules.nixos.<name> = { config, pkgs, ... }: {
  services.<name> = {
    enable = true;
    port = 8080;
    # ...
  };
};
```
