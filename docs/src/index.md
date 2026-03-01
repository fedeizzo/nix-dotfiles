# Nix Dotfiles Documentation

Welcome to the documentation for my NixOS dotfiles configuration and homelab setup. This serves as both a personal reference and a tutorial for others.

## Quick Overview

### Your Machines

| Machine         | Role     | Hardware           | Architecture             |
| --------------- | -------- | ------------------ | ------------------------ |
| **homelab**     | Server   | Framework Desktop  | x86_64 NixOS             |
| **x1-nano**     | Personal | ThinkPad X1 Nano   | x86_64 NixOS             |
| **macbook-pro** | Work     | MacBook Pro M1 Max | aarch64-darwin macOS     |
| **raspberry**   | Backup   | Raspberry Pi 4     | aarch64-linux (inactive) |

### Key Features

- **Declarative infrastructure**: Entire systems declared as code
- **Multi-OS support**: Linux, macOS, and ARM from one config
- **Immutable boot**: BTRFS snapshots with erase-on-boot philosophy
- **Secrets management**: SOPS with age encryption
- **Remote deployment**: SSH-based updates with rollback
- **Modular design**: Shared modules with machine-specific overrides

## Navigation

### Architecture

- [Philosophy](architecture/philosophy.md) - Core design principles
- [Why Nix?](architecture/why-nix.md) - Reasons for choosing Nix
- [Discarded Alternatives](architecture/why-not.md) - What didn't work

### Machines

- [Hardware Inventory](machines/inventory.md) - Complete overview
- [Framework Desktop (Homelab)](machines/homelab.md) - Server setup
- [ThinkPad X1 Nano](machines/x1-nano.md) - Personal laptop
- [MacBook Pro M1 Max](machines/macbook-pro.md) - Work machine

### Design

- [Declarative System Design](design/declarative.md) - How it works
- [BTRFS Snapshots & Impermanence](design/erase-boot.md) - Erase on boot
- [Flakes Structure](design/flakes-structure.md) - Organization

### Decisions

- [Deployment Strategy](decisions/deployment.md) - Remote deployment
- [Secrets Management](decisions/secrets.md) - SOPS integration

### Tutorial

- [Installation Guide](tutorial/installation.md) - Get started
- [Customization](tutorial/customization.md) - Make it yours

## About This Project

This configuration is continuously evolving. Every section documents current best practices along with trade-offs and alternatives considered.

Ready to explore the [Architecture Philosophy](architecture/philosophy.md)?
