# ThinkPad X1 Nano (Personal Laptop)

## Overview

The **ThinkPad X1 Nano** is the personal daily driver, representing the "personal-first" approach with full desktop environment and development tools. This machine embodies the erase-and-rebuild philosophy with complete system restoration on every boot.

**Hostname**: `oven`  
**User**: `oven`  
**Architecture**: x86_64  
**OS**: NixOS with BTRFS snapshots  
**Status**: Active personal laptop

## Hardware Characteristics

### ThinkPad X1 Nano Specifications

- **Processor**: Intel Core i7 (11th gen)
- **Memory**: High RAM capacity for development work
- **Storage**: Fast NVMe SSD
- **Display**: High-resolution OLED display
- **Portability**: Ultra-lightweight form factor
- **Build quality**: Premium ThinkPad build

### Why This Machine?

The X1 Nano was chosen for:
- **Portability**: Easy to carry anywhere
- **Power**: Sufficient for development work
- **Linux compatibility**: Excellent ThinkPad Linux support
- **Build quality**: Reliable hardware for daily use
- **Performance**: Good balance of performance and battery life

## Configuration Approach

### System Type

The X1 Nano is a **full desktop system** with:
- Desktop environment (currently Hyprland or Plasma)
- Development tools and IDEs
- Personal applications
- Complete user experience

### BTRFS Snapshots & Erase on Boot

The most distinctive feature of this machine is the **erase-and-rebuild-on-every-boot** approach:

```nix
# Example BTRFS configuration
{ config, pkgs, ... }:

{
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/...";
    fsType = "btrfs";
    options = [ "compress=zstd" "noatime" "subvol=root" ];
  };

  services.impermanence = {
    enable = true;
    filesystems = {
      "/" = {};
      "/home" = {};
      "/var" = {};
    };
    emptyFilesystems = [
      "/var/log"
      "/var/tmp"
      "/tmp"
    ];
  };

  # Snapshot configuration
  boot.initrd.btrfs.devices = {
    "/dev/disk/by-uuid/..." = {};
  };
}
```

### Data Persistence Strategy

**What persists across boots:**
- `/home/*`: All user files, dotfiles, configuration
- `/var/cache`: Package and application caches
- `/var/lib`: Persistent runtime data

**What does not persist:**
- `/var/log`: Logs (can be regenerated or lost)
- `/var/tmp`: Temporary files
- `/tmp`: System temporary files

This provides:
- **Complete system reset**: Every boot starts fresh
- **Persistent user data**: Personal files are preserved
- **Security**: Any compromise is wiped on next boot
- **Simplicity**: No maintenance needed

### Development Environment

The X1 Nano serves as the primary development machine, featuring:

#### Emacs Configuration

```nix
# home/common/emacs/configuration.nix
{ config, pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
    
    extraPackages = epkgs: with epkgs; [
      lsp-mode
      lsp-ui
      org-mode
      projectile
      company
    ];
  };
  
  # Emacs LSP booster for improved performance
  programs.emacs-lsp-booster.enable = true;
}
```

#### Shell Configuration

```nix
# home/common/fish/configuration.nix
{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    
    shellAliases = {
      ll = "ls -la";
      la = "ls -A";
      gs = "git status";
      gp = "git push";
      ga = "git add";
    };
    
    functions = {
      # Custom functions
    };
  };
}
```

#### Development Tools

- **Git**: Version control with custom configuration
- **Jujutsu**: Modern version control system
- **Zsh/Fish**: Modern shell with fish
- **Neovim/Emacs**: Text editors for development
- **IDEs**: Development environment for various languages

### Desktop Environment

The X1 Nano uses a **modern tiling window manager** approach:

```nix
# Example desktop configuration
{ config, pkgs, ... }:

{
  # Hyprland window manager
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };

  # Or Plasma desktop environment
  services.xserver = {
    enable = true;
    displayManager.gdm.enable = true;
    desktopManager.plasma5.enable = true;
  };
}
```

**Key considerations:**
- **Tiling window manager**: Efficient workspace management
- **Customizable**: Full control over appearance and behavior
- **Performance**: Lightweight but feature-rich
- **Wayland support**: Modern display protocol

## User Configuration

### Home Manager Structure

The X1 Nano's user configuration is in `/home/x1-nano/` and includes:

```
home/x1-nano/
├── configuration.nix      # Main configuration
├── emacs/                 # Emacs-specific config
├── fish/                  # Fish shell config
├── git/                   # Git configuration
├── kitty/                 # Kitty terminal config
└── ...                    # Other user applications
```

### Personal Preferences

The configuration reflects personal preferences:
- **Editor choice**: Emacs (with lsp-booster for performance)
- **Shell**: Fish for modern features and ease of use
- **Terminal**: Kitty for GPU acceleration and features
- **Browser**: Firefox or Zen browser
- **PDF viewer**: Zathura for minimal, keyboard-driven viewing

## Machine-Specific Customizations

### Hardware-Specific Settings

```nix
# Machine-specific overrides
{ config, lib, pkgs, ... }:

{
  # Hardware-specific configurations
  hardware.enableRedistributableFirmware = true;
  
  # Power management for laptop
  powerManagement = {
    enable = true;
    # Custom power settings
  };
  
  # Touchpad configuration
  services.libinput = {
    enable = true;
    naturalScrolling = true;
  };
}
```

### User Identity

```nix
{
  users.users.oven = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "docker" ];
    shell = pkgs.fish;
  };
}
```

## Comparison to Other Systems

### vs. Homelab (Framework Desktop)

| Aspect | X1 Nano | Homelab |
|--------|---------|----|
| Interface | Desktop environment | SSH only |
| Focus | User experience | Services |
| Updates | Manual control | Automated |
| Persistence | User-driven | Selective |
| Customization | Personal | Server-focused |

### vs. Work Machine (MacBook Pro)

| Aspect | X1 Nano | MacBook Pro |
|--------|---------|-------|
| Control | Full (NixOS) | Limited (macOS) |
| OS | NixOS | macOS + Nix |
| Boot behavior | Erase on boot | Standard |
| Development | Native Linux | macOS |

## Maintenance

### Update Process

```bash
# Update system
menu update

# Refresh devshell
menu refresh

# Clean old generations
menu clean
```

### Backup Considerations

Since the system resets on every boot:
- **Personal files**: Backed up separately (not system managed)
- **Configuration**: Already in git repository
- **Development projects**: Backed up as needed
- **Important data**: External backup solutions

## Why This Approach?

The X1 Nano setup follows several principles:

### 1. Personal Control

Full control over the entire system:
- Complete system customization
- No restrictions on software
- Full development capabilities

### 2. Erase and Rebuild

Every boot provides:
- **Fresh state**: No accumulated state
- **Security**: Wiped compromise
- **Simplicity**: No manual maintenance
- **Consistency**: Always in declared state

### 3. Development Focus

Optimized for development work:
- Modern toolchain
- Full IDE capabilities
- Multiple editors and shells
- Development environment integration

### 4. Portability

Designed for mobile work:
- Lightweight laptop
- Good battery life
- Fast boot and resume
- Reliable hardware

## Future Considerations

Potential improvements:
- **Containerization**: Docker/Podman for development
- **Monitoring**: System health monitoring
- **Battery optimization**: Further power management
- **Display calibration**: Better OLED calibration
- **External displays**: Better multi-monitor support

## Next Steps

The X1 Nano represents the personal desktop experience, complementing:
- [Homelab server setup](homelab.md)
- [Work machine configuration](macbook-pro.md)
- [Declarative design principles](../design/declarative.md)
- [Erasing on boot philosophy](../design/erase-boot.md)

The personal laptop demonstrates how the erase-and-rebuild philosophy works in practice while maintaining a full development environment.
