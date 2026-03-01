# Hardware Inventory

This document provides a detailed overview of all machines in the setup, including their specifications, roles, and configuration approaches.

## Machine Overview

| Machine | Role | Hostname | Architecture | OS | State |
|---------|------|----------|--------------|-----|-------|
| Framework Desktop | Homelab Server | homelab | x86_64 | NixOS | Active |
| ThinkPad X1 Nano | Personal Laptop | oven | x86_64 | NixOS | Active |
| MacBook Pro M1 Max | Work Machine | COMP-D2G067292T | aarch64-darwin | macOS (Nix-Darwin) | Active |
| Raspberry Pi 4 | Backup Homelab | freezer | aarch64-linux | NixOS | Inactive |

## Detailed Machine Descriptions

### Framework Desktop (Homelab Server)

**Hostname**: `homelab`  
**Role**: Primary homelab server  
**Architecture**: x86_64  
**OS**: NixOS (configured via `/hosts/framework-desktop`)  
**Status**: Active

**Configuration**:
- Uses a specialized Framework Desktop configuration
- Runs various homelab services including:
  - Authentik (identity and access management)
  - Climbing Lab (personal tracking application)
- Accessible via SSH for remote deployment
- Full system declared in Nix, including all services and networking

**Deployment**: Remote build with `deploy-rs`, magic rollback enabled.

### ThinkPad X1 Nano (Personal Laptop)

**Hostname**: `oven`  
**Role**: Personal daily driver  
**Architecture**: x86_64  
**OS**: NixOS  
**Status**: Active

**Configuration**:
- Managed via `/hosts/x1-nano` for system configuration
- User configuration in `/home/x1-nano`
- Uses BTRFS snapshots with erase-on-boot philosophy
- User: `oven` (as defined in specialArgs)

**Features**:
- Hyprland desktop environment (via plasma-manager or custom config)
- Complete system restoration on every boot
- Selective persistence of `/home` and `/var` directories
- Emacs development environment with lsp-booster

### MacBook Pro M1 Max (Work Machine)

**Hostname**: `COMP-D2G067292T`  
**Role**: Work laptop (employer-provided)  
**Architecture**: aarch64-darwin  
**OS**: macOS with Nix-Darwin  
**Status**: Active

**Configuration**:
- Managed via `/hosts/macbook-pro`
- Uses nix-darwin for system-level configuration
- macOS desktop environment (not configurable via Nix)
- User: `federico.izzo`

**Constraints**:
- Can only run macOS (employer-provided)
- Limited control over system-level settings
- Still benefits from Nix for user-level configuration and development environment

### Raspberry Pi 4 (Backup Homelab)

**Hostname**: `freezer`  
**Role**: Backup homelab (inactive)  
**Architecture**: aarch64-linux  
**OS**: NixOS  
**Status**: Inactive

**Configuration**:
- Managed via `/hosts/raspberry`
- NixOS configuration for ARM architecture
- Uses impermanence for stateless operation
- Serves as backup homelab or development platform

**Purpose**:
- Redundancy for homelab services
- Testing ground for ARM-specific configurations
- Low-power server option when needed

## Configuration Philosophy by Machine

### Personal Machines (X1 Nano)

- **Erase on boot**: Complete system reset with BTRFS snapshots
- **Personal customization**: Full control over desktop environment
- **Development focus**: Emacs, fish shell, development tools

### Work Machine (MacBook Pro)

- **Partial control**: Can only configure what macOS allows
- **Hybrid approach**: Nix-Darwin for available configurations
- **Continuity**: Same configuration philosophy despite OS limitations

### Homelab (Framework Desktop)

- **Service-first**: No desktop environment, server-focused
- **Remote management**: SSH-only access
- **High availability**: Magic rollback, remote builds

### Backup (Raspberry Pi)

- **Simplified**: Stripped-down configuration
- **ARM-specific**: Tested for aarch64-linux
- **Flexibility**: Can be repurposed or used for learning

## Why This Hardware Selection?

### Diversity of Use Cases

The selection covers multiple scenarios:
- **Power user**: X1 Nano for personal use and development
- **Server**: Framework Desktop for homelab services
- **Work**: MacBook Pro for professional requirements
- **Learning**: Raspberry Pi for ARM and low-power scenarios

### Architecture Coverage

Supporting multiple architectures allows:
- Testing cross-platform compatibility
- Understanding ARM-specific considerations
- Serving different use cases (server vs. desktop vs. laptop)

### Cost-Benefit Analysis

Each machine serves a specific purpose:
- **Homelab**: Invest in reliable x86_64 for critical services
- **Personal**: High-quality laptop that can be fully controlled
- **Work**: Employer-provided, minimize personal investment
- **Backup**: Low-cost option for redundancy and experimentation

## Network Topology

See the [Network Topology](../homelab/network.md) section for details on how these machines communicate and the overall network structure.

## Next Steps

With the hardware inventory established, we can now explore:
- [The design philosophy behind the setup](../design/declarative.md)
- [Network and service architecture](../homelab/network.md)
- [How to install and set up a similar system](../tutorial/installation.md)
