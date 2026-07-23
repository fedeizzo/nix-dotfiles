# Flake Modules

This repo uses a modular flake architecture. Each `.nix` file under `modules/` declares:

1. **Inputs** — external flakes via `flake-file.inputs.*`
2. **Module definitions** — `flake.modules.nixos.*`, `flake.modules.homeManager.*`, `flake.modules.darwin.*`

Hosts compose these modules. Users import homeManager modules.

---

## Structure

```
modules/
├── apps/              # Desktop apps (voxtype, zed, ghostty, etc.)
│   └── desktop-environment/  # Niri, DMS, keybindings
├── services/          # Server services (traefik, postgres, immich, etc.)
├── home/              # Home-manager specific modules
├── infra/             # Infrastructure (backrest, blocky, etc.)
├── observability/     # Monitoring (gatus, glance, grafana, prometheus)
├── security/          # Authentik, etc.
├── system/            # NixOS system config (boot, disk, networking, etc.)
├── hosts/             # Per-host configurations (oven, homelab, etc.)
└── home-manager/      # (not yet used — modules live in apps/home/system)
```

## Module Pattern

Every module file follows this shape:

```nix
{
  # 1. Declare external flake inputs
  flake-file.inputs.voxtype.url = "github:peteonrails/voxtype/v1.0.0-rc1";

  # 2. Define platform-specific modules
  flake.modules.homeManager.voxtype = { pkgs, inputs, ... }: {
    imports = [ inputs.voxtype.homeManagerModules.default ];
    programs.voxtype = { /* config */ };
  };

  flake.modules.nixos.voxtype = { self, config, pkgs, username, ... }: {
    home-manager.users.${username}.imports = [
      self.modules.homeManager.voxtype
    ];
  };
}
```

### `flake-file.inputs.*`

Declarations live in the file. They're merged into the top-level flake inputs.

```nix
flake-file.inputs.niri.url = "github:sodiboo/niri-flake";
flake-file.inputs.niri.inputs.nixpkgs.follows = "nixpkgs";
```

### `flake.modules.*`

Three entry points:

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
2. Follow the module pattern above
3. Import it into the relevant host or user

```nix
# modules/apps/myapp.h.nix
{
  flake-file.inputs.myapp.url = "github:user/myapp";

  flake.modules.homeManager.myapp = { pkgs, inputs, ... }: {
    imports = [ inputs.myapp.homeManagerModules.default ];
    programs.myapp = {
      enable = true;
      # ...
    };
  };
}
```

## Naming Conventions

| Suffix | Meaning |
|--------|---------|
| `.h.nix` | Home Manager module (most common) |
| `.n.nix` | NixOS module |
| `.nh.nix` | Both NixOS + Home Manager |
| `.d.nix` | Darwin module |

## Cross-References

- [Nix Flakes](https://wiki.nixos.org/wiki/Flakes)
- [Home Manager](https://github.com/nix-community/home-manager)
- [deploy-rs](https://github.com/serokell/deploy-rs) (for host deployments)
