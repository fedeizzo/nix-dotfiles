# MacBook Pro M1 Max (Work Machine)

## Overview

The **MacBook Pro M1 Max** serves as the work laptop, representing a **hybrid approach** where Nix-Darwin provides system-level configuration within the constraints of macOS. This machine demonstrates how the same configuration philosophy can apply even when full control over the operating system is not available.

**Hostname**: `COMP-D2G067292T`  
**User**: `federico.izzo`  
**Architecture**: aarch64-darwin (Apple Silicon M1)  
**OS**: macOS with Nix-Darwin  
**Status**: Active work machine

## Hardware Characteristics

### MacBook Pro M1 Max Specifications

- **Processor**: Apple M1 Max (10-core CPU, 32-core GPU)
- **Memory**: 32GB unified memory
- **Storage**: High-speed SSD
- **Display**: Liquid Retina XDR display
- **Portability**: Premium laptop form factor
- **Architecture**: ARM-based (Apple Silicon)

### Why This Machine?

The MacBook Pro was chosen for:
- **Employer provision**: Provided as work equipment
- **Performance**: Exceptional performance for development tasks
- **Battery life**: All-day battery life
- **Build quality**: Premium Apple build quality
- **ARM architecture**: Modern Apple Silicon platform
- **macOS**: Required work environment

## Configuration Approach

### Hybrid Configuration Strategy

Unlike the NixOS machines, the MacBook Pro runs **macOS with Nix-Darwin** for configuration:

```nix
# macOS system configuration
{ config, pkgs, lib, ... }:

{
  # System settings via Nix-Darwin
  system.defaults = {
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      InitialKeyRepeat = 10;
    };
  };

  # Nix packages alongside macOS
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";
  };
}
```

### Key Differences from NixOS

| Aspect | NixOS | macOS + Nix-Darwin |
|--------|-------|------|
| System control | Full | Limited |
| OS customization | Complete | Partial |
| Boot behavior | Custom (erase on boot) | Standard macOS |
| Package management | Nix only | Homebrew + Nix |
| Desktop environment | Customizable | Fixed (macOS) |
| System services | Declarative | Partial control |

### Nix-Darwin Integration

Nix-Darwin provides a subset of NixOS capabilities for macOS:

```nix
# Example Nix-Darwin configuration
{ config, pkgs, lib, ... }:

{
  # User configuration
  users.users.federico.izzo = {
    isNormalUser = true;
    extraGroups = [ "admin" ];
    shell = pkgs.fish;
  };

  # Environment variables
  environment.variables = {
    EDITOR = "emacs";
    LANG = "en_US.UTF-8";
  };

  # System packages
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = [
    pkgs.vim
    pkgs.git
    pkgs.emacs
  ];
}
```

## Desktop Environment

### macOS Desktop

The MacBook Pro uses the **native macOS desktop environment**:
- **Finder**: File management
- **Spotlight**: Application and file search
- **Dock**: Application launcher
- **System Preferences**: System settings

**Constraints:**
- Desktop appearance is fixed
- Limited customization options
- No tiling window manager (though possible via third-party tools)
- Standard macOS applications

### Nix Integration

Despite the macOS desktop, Nix provides:
- **Development tools**: GNU toolchain, compilers, interpreters
- **User applications**: Emacs, vim, git, etc.
- **Package management**: Consistent with other systems
- **Cross-platform consistency**: Same Nix expressions work everywhere

## Development Environment

### Common Development Setup

The MacBook Pro shares many development tools with the X1 Nano:

#### Emacs Configuration

```nix
# Emacs configuration shared across machines
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
      flycheck
    ];
  };
}
```

#### Shell Configuration

```nix
# Fish shell configuration
{ pkgs, ... }:

{
  programs.fish = {
    enable = true;
    
    shellAliases = {
      ll = "ls -la";
      la = "ls -A";
      gs = "git status";
      gp = "git push";
    };
  };
}
```

#### Cross-Platform Consistency

The same dotfiles work across all systems:
- **Emacs**: Same configuration, same packages
- **Fish shell**: Same aliases, same functions
- **Git**: Same configuration, same hooks
- **Editor**: Same settings, same plugins

## Machine-Specific Customizations

### macOS-Specific Settings

```nix
# macOS-specific configurations
{ config, lib, pkgs, ... }:

{
  # macOS-specific defaults
  system.defaults = {
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;  # Key repeat enabled
      InitialKeyRepeat = 10;     # Short initial delay
    };
    
    NSApplication = {
      ApplePressAndHoldEnabled = false;
    };
  };

  # Homebrew integration
  homebrew = {
    enable = true;
    casks = [
      "google-chrome"
      "firefox"
      "docker"
    ];
  };
}
```

### User Identity

```nix
{
  users.users.federico.izzo = {
    isNormalUser = true;
    extraGroups = [ "admin" ];
    shell = pkgs.fish;
  };
}
```

## Comparison to Other Systems

### vs. Personal Laptop (X1 Nano)

| Aspect | MacBook Pro | X1 Nano |
|--------|-------------|-----|
| OS | macOS + Nix-Darwin | NixOS |
| Desktop | Native macOS | Custom (Hyprland/Plasma) |
| Control | Limited | Full |
| Boot behavior | Standard | Erase on boot |
| Customization | Partial | Complete |
| Development | Native macOS | Native Linux |

### vs. Homelab (Framework Desktop)

| Aspect | MacBook Pro | Homelab |
|--------|-------------|---|
| Use case | Work laptop | Server |
| Access | GUI + SSH | SSH only |
| Focus | User experience | Services |
| OS | macOS | NixOS |
| Persistence | Standard | Erase on boot |

## Why This Approach?

The MacBook Pro setup follows several principles:

### 1. Accept Reality

Recognizing that work machines are provided by employers:
- Can't install NixOS on work hardware
- Must work within macOS constraints
- But can still apply Nix philosophy

### 2. Consistent Philosophy

Even with limited control:
- Same development tools
- Same shell configuration
- Same editor settings
- Same development workflow

### 3. Hybrid Advantage

Combining best of both worlds:
- **macOS**: Professional environment, excellent hardware
- **Nix**: Consistent development tools, package management
- **Flexibility**: Can use macOS features when needed

### 4. Cross-Platform Testing

MacBook Pro serves as:
- Testbed for macOS-specific configurations
- Validation of cross-platform compatibility
- ARM64 testing (Apple Silicon)
- Real-world usage of Nix-Darwin

## Limitations and Trade-offs

### What Can't Be Done

- **Full system control**: Can't modify system-level macOS settings
- **Boot behavior**: Standard macOS boot process
- **Desktop environment**: Fixed macOS desktop
- **System services**: Limited control over macOS services

### What Can Be Done

- **User configuration**: Full control over user environment
- **Development tools**: Complete development setup
- **Dotfiles**: Same configuration across all systems
- **Package management**: Consistent with other systems

## Future Considerations

Potential improvements:
- **Additional macOS tools**: More Homebrew integration
- **Better customization**: More macOS-specific tweaks
- **Development environment**: Enhanced macOS development tools
- **Cross-platform testing**: More frequent macOS testing

## Next Steps

The MacBook Pro demonstrates the **hybrid approach** of Nix-Darwin, complementing:
- [Homelab server setup](homelab.md)
- [Personal laptop configuration](x1-nano.md)
- [Nix-Darwin integration](../architecture/why-nix.md)
- [Cross-platform consistency](../design/declarative.md)

The work machine shows that even with limited control over the operating system, the same configuration philosophy can be applied to maintain consistency across all systems in the setup.
