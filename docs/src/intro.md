# Introduction

Welcome to the documentation for my [NixOS](https://nixos.org/) dotfiles configuration and homelab setup. This documentation serves two purposes:

1. **Personal reference**: To keep track of architectural decisions and rationale
2. **Tutorial**: To help others understand and potentially adapt this setup

## Overview

This documentation covers a multi-machine setup managed through [Nix Flakes](https://nixos.wiki/wiki/Flakes), featuring:

- **Hardware diversity**: x86_64 Linux, ARM Raspberry Pi, and Apple Silicon macOS
- **Declarative infrastructure**: Systems managed as code
- **Security-first**: SOPS-based secrets management
- **Remote deployment**: SSH-based configuration updates
- **Modular design**: Shared modules with machine-specific overrides

## My Machines

| Machine             | Role   | Hardware           | Architecture         |
| ------------------- | ------ | ------------------ | -------------------- |
| **homelab**         | Server | Framework Desktop  | x86_64 NixOS         |
| **oven**            | Laptop | ThinkPad X1 Nano   | x86_64 NixOS         |
| **freezer**         | Server | Raspberry Pi 4     | aarch64-linux NixOS  |
| **COMP-D2G067292T** | Work   | MacBook Pro M1 Max | aarch64-darwin macOS |

## Key Architecture Choices

### Flakes-Based Configuration

The entire setup uses [flake-parts](https://flake.parts/) for modular organization:

- Reproducible builds with pinned dependencies
- Clean separation of concerns
- Easy sharing and reuse of modules

### Multi-System Approach

- **NixOS**: Full system management for Linux
- **Nix-Darwin**: macOS system configuration
- **Home Manager**: User-level configuration across all systems
- **Cross-platform**: Same Nix expressions work everywhere

### Design Patterns

- **Shared modules**: Configuration common across machines
- **Machine overrides**: Customization per device
- **Immutable boot**: BTRFS snapshots for system restoration
- **Remote deployment**: [deploy-rs](https://github.com/serokell/deploy-rs) for SSH updates

## Not Just Dotfiles

This goes beyond typical dotfiles management:

- **Full system management**: The entire OS is declared
- **Infrastructure as code**: Homelab services declared in Nix
- **Self-documenting**: Every choice includes rationale
- **Evolving**: Documents current state and trade-offs

## How to Use This Documentation

- **Quick start**: [Installation Guide](tutorial/installation.md)
- **Deep dive**: [Architecture Philosophy](architecture/philosophy.md)
- **Specific topics**: Navigate by area of interest

Ready to explore the [Architecture Philosophy](architecture/philosophy.md)?
