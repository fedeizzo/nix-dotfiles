# Declarative System Design

## The Declarative Paradigm

This setup is built on a fundamentally different approach to system management: **declarative** rather than **imperative**.

### What is Declarative Management?

Declarative management means specifying **what** the system should look like, not **how** to get there.

#### Imperative Example

```bash
# Imperative: The steps to achieve the desired state
mkdir -p ~/.config/myapp
echo "setting = true" > ~/.config/myapp/config
apt update
apt install vim
systemctl enable my-service
```

Problems with this approach:
- Order matters: You must run commands in the right sequence
- Idempotency: Running twice might cause errors
- No guaranteed state: What if a command fails partway?
- No rollback: How do you undo this?

#### Declarative Example

```nix
# Declarative: The desired state
{ config, pkgs, ... }:
{
  home.file.".config/myapp/config" = {
    source = ./config;
  };
  
  environment.systemPackages = [ pkgs.vim ];
  
  services.my-service.enable = true;
}
```

Benefits of this approach:
- **State is the source of truth**: The configuration file describes the system
- **Idempotent**: Running the same configuration always results in the same state
- **Atomic updates**: Changes are applied atomically or not at all
- **Rollback**: Previous states are preserved and can be reverted
- **Version control**: Changes are diffable and reviewable

## Nix: The Engine of Declarative Management

[Nix](https://nixos.org/) is the package manager and build system that makes declarative management practical.

### Key Nix Concepts

#### 1. The Nix Store

```
/nix/store/
  └── 2x1d2a3b4c5d6e7f8g9h0i1j2k3l4m5n6o7p8q9r0s1t2u3v4w5x6y7z8a9b0c1d-vim-9.1/
      ├── bin
      │   └── vim
      └── share
          └── vim/
```

Every package is stored in an immutable directory with a hash-based name that encodes all its dependencies. This means:
- Two different versions of the same package can coexist
- Dependencies are explicit and isolated
- No "dependency hell" or library conflicts

#### 2. Functional Evaluation

Nix expressions are pure functions: given the same inputs, they always produce the same output.

```nix
# A Nix expression that takes package set as input
{ pkgs ? import <nixpkgs> {} }:

{
  myApp = pkgs.buildPerlPackage {
    pname = "myapp";
    version = "1.0";
    src = ./src;
    buildInputs = [ pkgs.perl ];
  };
}
```

This functional nature ensures:
- Reproducibility across builds
- No side effects
- Pure evaluation (no hidden dependencies)

#### 3. Garbage Collection

```bash
# View old generations
nix-env --list-generations

# Clean up unused packages
nix-collect-garbage
```

The garbage collector automatically removes:
- Packages not referenced by any installation
- Old generations beyond the retention period
- Unused derivations in the Nix store

### How NixOS Uses Nix

NixOS takes the functional package manager and applies it to the entire operating system.

```nix
# /etc/nixos/configuration.nix
{ config, pkgs, lib, ... }:

{
  # System configuration
  time.timeZone = "America/New_York";
  
  # Services
  services.openssh.enable = true;
  services.httpd.enable = true;
  
  # Users
  users.users.alice = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.fish;
  };
  
  # Networking
  networking.hostName = "my-server";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
  
  # Fonts
  fonts.fontconfig.enable = true;
}
```

This configuration is evaluated and applied:
1. **Build**: The configuration is evaluated to create a system derivation
2. **Deploy**: The new system is built and installed as a new generation
3. **Boot**: The system can boot into the new generation
4. **Rollback**: If anything breaks, boot into a previous generation

## The BTRFS Snapshots: Erase on Boot

One of the most distinctive features of this setup is the **erase-and-rebuild-on-every-boot** approach for personal machines.

### How It Works

```
┌─────────────────────────────────────────────────────────────┐
│                    Boot Sequence                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. Machine powers on                                      │
│                                                             │
│  2. Bootloader shows snapshot list                         │
│     ┌─────────────────────────────────────────────┐        │
│     │ NixOS (2025-01-15) - Current                │        │
│     │ NixOS (2025-01-14) - Previous                │        │
│     │ NixOS (2025-01-13) - Previous                │        │
│     └─────────────────────────────────────────────┘        │
│                                                             │
│  3. User selects current snapshot                            │
│                                                             │
│  4. System mounts the snapshot                              │
│                                                             │
│  5. Impermanence restores /home and /var                    │
│     - /home: User's personal files                          │
│     - /var: Logs, caches, runtime data                      │
│                                                             │
│  6. System is ready, stateless but persistent data          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Impermanence Configuration

```nix
# /etc/nixos/configuration.nix
{ config, pkgs, ... }:

{
  # Impermanence for stateless operation
  services.impermanence = {
    enable = true;
    
    # Bind mounts for persistent directories
    filesystems = {
      "/" = {};
      "/home" = {};
      "/var" = {};
    };
    
    # Empty directories that should be recreated
    emptyFilesystems = [
      "/var/log"
      "/var/tmp"
      "/tmp"
    ];
  };
  
  # BTRFS specific settings
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/...";
    fsType = "btrfs";
    options = [ "compress=zstd" "noatime" ];
  };
  
  # Enable snapshot on boot
  boot.initrd.btrfs.devices = {
    "/dev/disk/by-uuid/..." = {};
  };
}
```

### Why This Approach?

**Benefits**:
1. **Guaranteed consistency**: System is always in a known, declared state
2. **No configuration drift**: Nothing can accumulate outside the configuration
3. **Simplified debugging**: If something breaks, the next boot resets everything
4. **Security**: Any compromise is wiped on the next boot
5. **Simplicity**: No need for manual maintenance or backup strategies

**Trade-offs**:
1. **Performance**: Every boot requires recreating the filesystem
2. **Data management**: You must be intentional about what data persists
3. **Learning curve**: Requires understanding of NixOS and BTRFS
4. **Not for everyone**: This is an experimental approach

### Data Persistence

What persists across boots:
- `/home/*`: User's personal files, dotfiles, configuration
- `/var/cache`: Cached data (can be regenerated)
- `/var/lib`: Persistent runtime data (databases, etc.)

What does not persist:
- `/var/log`: Logs are ephemeral (can be regenerated or lost)
- `/var/tmp`: Temporary files
- `/tmp`: Temporary files

## Configuration Structure

The configuration is organized using **flake-parts**, which provides a modular architecture.

### Directory Layout

```
nix-dotfiles/
├── home/                    # Home Manager configurations
│   ├── common/             # Shared across all machines
│   │   ├── cli/            # CLI tools configuration
│   │   ├── emacs/          # Emacs configuration
│   │   ├── fish/           # Fish shell configuration
│   │   └── ...
│   ├── x1-nano/            # Personal laptop user config
│   ├── macbook-pro/        # Work machine user config
│   └── raspberry/          # Raspberry Pi user config
│
├── hosts/                   # NixOS host configurations
│   ├── common/             # Shared system modules
│   ├── framework-desktop/  # Homelab system config
│   ├── x1-nano/            # Personal laptop system config
│   ├── macbook-pro/        # Work machine system config
│   └── raspberry/          # Raspberry Pi system config
│
├── nix/                     # Flake modules and utilities
│   ├── nixosConfigurations.nix  # All NixOS system declarations
│   ├── deployment.nix      # SSH deployment configuration
│   ├── devshells.nix       # Development environment
│   └── ...
│
├── overlays/                # Custom package modifications
├── scripts/                 # System management scripts
└── secrets/                 # Encrypted secrets
```

### Module System

```nix
# Example: How modules are composed
# nix/nixosConfigurations.nix

{ inputs, ... }:
{
  nixosConfigurations.x1-nano = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; hostname = "oven"; };
    
    modules = [
      # Common modules (shared across systems)
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

### Module Inheritance

```nix
# Example: Module inheritance chain
# hosts/common/base.nix
{ lib, ... }:
{
  options.myCustom.enable = lib.mkEnableOption "my custom feature";
}

# hosts/x1-nano/system.nix
{ lib, pkgs, ... }:
{
  imports = [
    ../common/base.nix
    ../common/security.nix
  ];
  
  # Additional settings specific to x1-nano
  services.hyprland.enable = true;
}
```

## Home Manager: User-Level Configuration

[Home Manager](https://github.com/nix-community/home-manager) manages user-level configuration, separate from system-level configuration.

### What Home Manager Manages

- User accounts and groups
- Shell configuration (bash, fish, zsh)
- Application configurations (vim, emacs, git)
- Environment variables
- Desktop environment settings
- Dotfiles management

### Example Home Manager Configuration

```nix
# home/x1-nano/configuration.nix
{ config, pkgs, lib, ... }:

{
  # User configuration
  home.username = "oven";
  home.homeDirectory = "/home/oven";
  
  # Dotfiles
  home.stateVersion = "24.11";
  home.file.".config/myapp".source = ./config;
  
  # Environment
  home.sessionVariables = {
    EDITOR = "emacs";
    LANG = "en_US.UTF-8";
  };
  
  # Shell
  programs.fish = {
    enable = true;
    shellAliases = {
      ll = "ls -la";
      la = "ls -A";
    };
  };
  
  # Applications
  programs.vim = {
    enable = true;
    settings = {
      number = true;
      relativenumber = true;
    };
  };
}
```

### Why Separate User and System?

The separation between Home Manager and NixOS system configuration serves several purposes:

1. **Portability**: User configurations can be moved between machines
2. **Isolation**: System changes don't affect user configurations
3. **Reusability**: Same dotfiles work across different systems
4. **Testing**: User configuration can be tested independently
5. **Flexibility**: Different users on the same system can have different configurations

## Conclusion

The declarative system design provides:

- **Predictability**: The system always behaves as declared
- **Reproducibility**: Same configuration, same results
- **Safety**: Rollbacks are always available
- **Simplicity**: Configuration is the documentation
- **Automation**: Everything can be rebuilt and tested automatically

This approach requires a shift in thinking from "how do I make this happen" to "what should this look like", but the long-term benefits for maintenance and reliability are substantial.

## Next Steps

- [Deployment strategy](../decisions/deployment.md)
- [Secrets management](../decisions/secrets.md)
- [Getting started](../tutorial/installation.md)
