# Nix Flakes Structure

## Introduction to Nix Flakes

[Nix Flakes](https://nixos.wiki/wiki/Flakes) is a new experimental feature in Nix that provides a more structured and reproducible way to manage Nix dependencies and configurations. This document explains how flake structure is used in this setup.

## What is a Flake?

A Nix flake is a self-contained, reproducible Nix build that includes:
- **Inputs**: Explicit dependencies on other flakes
- **Outputs**: Generated artifacts and configurations
- **Lock file**: Ensures reproducible builds

```nix
# flake.nix structure
{
  description = "My NixOS configuration";

  inputs = {
    # External dependencies
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
  };

  outputs = { self, nixpkgs, home-manager, ... }: {
    # Generated outputs
    nixosConfigurations = {
      my-system = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./configuration.nix ];
      };
    };
  };
}
```

## Flake Inputs Explained

### Core Dependencies

```nix
inputs = {
  # Nixpkgs
  nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
  nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
  
  # Home Manager
  home-manager.url = "github:nix-community/home-manager/release-24.11";
  home-manager.inputs.nixpkgs.follows = "nixpkgs";
  
  # Plasma Manager
  plasma-manager = {
    url = "github:nix-community/plasma-manager";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.home-manager.follows = "home-manager";
  };
};
```

### Why This Structure?

1. **Explicit versioning**: All dependencies are pinned to specific versions
2. **Follows**: Avoids duplicating nixpkgs across multiple flakes
3. **Isolation**: Each flake input is independent
4. **Reproducibility**: Same inputs always produce the same outputs

### Nixpkgs Versions

The setup uses both stable and unstable nixpkgs:

```nix
# Stable version (for most packages)
nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

# Unstable version (for latest packages)
nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
```

This allows using stable, tested packages while still accessing newer versions when needed.

## Flake Outputs

### System Configurations

```nix
outputs = { self, nixpkgs, home-manager, ... }:
{
  # NixOS configurations
  nixosConfigurations = {
    homelab = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./hosts/framework-desktop ];
    };
    
    x1-nano = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./hosts/x1-nano ];
    };
    
    macbook-pro = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [ ./hosts/macbook-pro ];
    };
  };
};
```

### Flake-parts Integration

The setup uses [flake-parts](https://flake.parts/) for modular configuration:

```nix
{
  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
        
        perSystem = { pkgs, ... }: {
          # System-specific configurations
          imports = [
            ./nix/git-hooks.nix
            ./nix/devshells.nix
            ./nix/topology.nix
          ];
        };
        
        flake = {
          imports = [
            ./nix/packages.nix
            ./nix/nixosConfigurations.nix
            ./nix/deployment.nix
          ];
        };
      };
}
```

## Directory Structure

### `home/` - Home Manager Configurations

```
home/
├── common/           # Shared across all machines
│   ├── cli/          # CLI tools configuration
│   ├── emacs/        # Emacs configuration
│   ├── fish/         # Fish shell configuration
│   ├── git/          # Git configuration
│   ├── kitty/        # Kitty terminal config
│   ├── languages/    # Language configurations
│   ├── rofi/         # Rofi launcher config
│   ├── starship/     # Starship prompt config
│   ├── user/         # User configuration
│   └── zathura/      # Zathura PDF viewer config
├── x1-nano/          # Personal laptop user config
├── macbook-pro/      # Work machine user config
├── raspberry/        # Raspberry Pi user config
└── configuration.nix # Main home-manager file
```

### `hosts/` - NixOS Host Configurations

```
hosts/
├── common/           # Shared system modules
├── framework-desktop/ # Homelab system config
├── x1-nano/          # Personal laptop system config
├── macbook-pro/      # Work machine system config
├── raspberry/        # Raspberry Pi system config
├── sd-installer/     # SD card installer config
└── xps-9510-homelab/ # Legacy homelab config
```

### `nix/` - Flake Modules

```
nix/
├── nixosConfigurations.nix  # All NixOS system declarations
├── deployment.nix           # SSH deployment configuration
├── devshells.nix            # Development environment
├── git-hooks.nix            # Git hooks configuration
├── packages.nix             # Package definitions
├── topology.nix             # Network topology tool
└── pkgs/                    # Custom packages
```

### `overlays/` - Package Modifications

```
overlays/
├── default.nix              # Main overlay file
└── custom-packages.nix      # Custom package overrides
```

### `secrets/` - Encrypted Secrets

```
secrets/
├── age.pub                   # Public encryption key
├── secrets.json              # Encrypted secrets file
└── secrets.age               # Age encryption key
```

### `scripts/` - System Management Scripts

```
scripts/
├── menu.sh                   # Main menu script
├── erase-disk-and-install-{machine}
├── deploy-homelab            # Deploy script
├── clean                     # Clean script
└── update                    # Update script
```

## Module Composition

### Home Manager Modules

```nix
# home/configuration.nix
{ inputs, ... }:
{
  imports = [
    # Common modules
    ./common/cli
    ./common/emacs
    ./common/fish
    ./common/git
    ./common/kitty
    
    # Machine-specific
    ./x1-nano
  ];
  
  # Global settings
  home = {
    username = "oven";
    homeDirectory = "/home/oven";
  };
}
```

### NixOS Module Composition

```nix
# nix/nixosConfigurations.nix
{ inputs, ... }:
{
  nixosConfigurations.x1-nano = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; hostname = "oven"; };
    
    modules = [
      # Common system modules
      ../hosts/common
      ../home/x1-nano
      
      # System-specific modules
      ../hosts/x1-nano
      
      # Local overrides
      ({ config, lib, ... }: {
        # Custom settings for this specific machine
        hardware.enableRedistributableFirmware = true;
      })
    ];
  };
}
```

## Flake Inputs Organization

### Platform-Specific Inputs

```nix
# Linux inputs
nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
home-manager.url = "github:nix-community/home-manager/release-24.11";

# macOS inputs
nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.05-darwin";
nix-darwin.url = "github:LnL7/nix-darwin/nix-darwin-24.05";

# Homelab inputs
nixpkgs-homelab.url = "github:nixos/nixpkgs/nixos-24.11";
climbing-lab.url = "github:fedeizzo/climbing-lab";

# Raspberry inputs
nixos-pikvm.url = "github:hatch01/nixos-pikvm";

# Flake management
flake-parts.url = "github:hercules-ci/flake-parts";
devshell.url = "github:numtide/devshell";
```

### Input Following

```nix
home-manager.inputs.nixpkgs.follows = "nixpkgs";
plasma-manager.inputs.nixpkgs.follows = "nixpkgs";
plasma-manager.inputs.home-manager.follows = "home-manager";
```

This ensures all flakes use the same nixpkgs version, avoiding duplication.

## Flake Outputs Types

### NixOS Configurations

```nix
nixosConfigurations = {
  homelab = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; hostname = "homelab"; };
    modules = [ ../hosts/framework-desktop ];
  };
};
```

### Darwing Configurations

```nix
darwinConfigurations = {
  COMP-D2G067292T = nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    specialArgs = { inherit inputs; hostname = "COMP-D2G067292T"; };
    modules = [ ../hosts/macbook-pro ];
  };
};
```

### Checks and Tests

```nix
checks = builtins.mapAttrs
  (_system: deployLib: deployLib.deployChecks self.deploy)
  inputs.deploy-rs.lib;
```

### Formatters

```nix
perSystem = { pkgs, ... }: {
  formatter = pkgs.nixpkgs-fmt;
};
```

## Benefits of Flake Structure

### 1. Reproducibility

```nix
# Locked inputs ensure reproducibility
inputs.nixpkgs = {
  url = "github:nixos/nixpkgs/12345abc";
  flake = true;
};
```

### 2. Isolation

Each flake input is isolated from others:
- No dependency conflicts
- No global state
- Clean evaluation

### 3. Sharing

Flakes can be easily shared:
- Repository as a flake
- Public flakes for reuse
- Easy dependency management

### 4. Development Experience

```nix
# Development shell
nix develop
# Or
nix develop .#devshell
```

Provides all development tools needed for the project.

## Conclusion

The flake structure provides a modular, reproducible, and maintainable way to manage the entire NixOS configuration. Each component has a clear purpose and location, making the system easy to understand and modify.

## Next Steps

- [Deployment strategy](../decisions/deployment.md) - How changes are deployed
- [Home Manager organization](home-manager.md) - User-level configuration
- [Custom packages](../decisions/packages.md) - Package management decisions
