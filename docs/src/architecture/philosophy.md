# Architecture Philosophy

## Declarative Infrastructure

At the core of this setup lies a fundamental principle: **infrastructure as code**. Every aspect of the system, from the kernel modules loaded to the applications installed, is declared in configuration files that describe the *desired state* rather than the steps to reach it.

### Why Declarative?

The declarative approach contrasts with imperative approaches where you would:

```bash
# Imperative: Do this, then that
apt update
apt install firefox
mkdir -p ~/.config
cp my-config.yml ~/.config/
```

Instead, the declarative approach simply states:

```nix
# Declarative: This is what the system should look like
environment.systemPackages = [ pkgs.firefox ];
home.file.".config" = { source = ./my-config; };
```

### The Nix Advantage

[Nix](https://nixos.org/) provides several unique capabilities that make declarative management practical:

1. **Reproducibility**: Given the same inputs, you get the same outputs, every time
2. **Atomic updates**: Changes are applied atomically, making rollbacks trivial
3. **Garbage collection**: Old versions are automatically cleaned up when no longer needed
4. **Isolation**: Packages don't interfere with each other; dependencies are explicit

## Multi-OS Support

A key architectural decision was supporting multiple operating systems from a single configuration:

- **Linux (NixOS)**: Full system management
- **macOS (Nix-Darwin)**: System and user-level configuration
- **Cross-architecture**: Supporting both x86_64 and aarch64 (Apple Silicon, Raspberry Pi)

### Why This Matters

This approach recognizes that different tools serve different purposes:

- Use a Mac for work because that's what your employer provides
- Have a personal laptop that you control completely
- Run a homelab that you can rebuild in minutes
- Use a Raspberry Pi for lightweight server workloads

Yet, the *configuration philosophy* remains consistent across all systems.

## The Immutable Boot Strategy

One of the most distinctive features of this setup is the **erase-and-rebuild-on-every-boot** approach for personal machines.

### How It Works

Every boot creates a fresh BTRFS snapshot of the system. The previous boot's state is discarded, but specific directories (`/home`, `/var`, `/var/log`) are preserved using [impermanence](https://github.com/nix-community/impermanence).

```
Boot 1:  [snapshot] → [btrfs rollback] → [fresh system] → [restore /home, /var]
Boot 2:  [snapshot] → [btrfs rollback] → [fresh system] → [restore /home, /var]
```

### Why Erase Everything?

This approach, while seemingly extreme, has several advantages:

1. **Complete consistency**: The system is always in a known, declared state
2. **No configuration drift**: Nothing can accumulate outside the configuration
3. **Simplified debugging**: If something breaks, you can rollback to a known good state
4. **Security**: Any compromise is wiped on the next boot

### Trade-offs Acknowledged

This is not without costs:

- **Performance**: Every boot requires recreating the filesystem
- **Data management**: You must be intentional about what data persists
- **Not for everyone**: This is an experimental approach, not the default NixOS path

## Modular Design

The configuration is organized using [flake-parts](https://flake.parts/), which enables:

- **Shared modules**: Code that makes sense across multiple systems
- **System-specific overrides**: Customization that doesn't pollute shared code
- **Independent testing**: Each system can be built and tested in isolation
- **Reusability**: Modules can be imported by other flakes

### Directory Organization

```
home/       # Home Manager configurations (user-level)
  common/   # Shared across all machines
  x1-nano/  # Personal laptop specific
  macbook-pro/ # Work machine specific

hosts/      # NixOS host configurations (system-level)
  common/   # Shared modules
  x1-nano/  # Personal laptop system config
  xps-9510-homelab/ # Homelab system config

nix/        # Flake modules and utilities
  nixosConfigurations.nix # Declaration of all NixOS systems
  deployment.nix          # SSH deployment configuration

overlays/   # Custom package modifications

secrets/    # Encrypted secrets (SOPS)
```

## Security Through Encryption

Security is approached through multiple layers:

1. **Secrets encryption**: Using [SOPS](https://github.com/mozilla/sops) with age keys
2. **Minimal attack surface**: Only necessary services exposed
3. **Immutable infrastructure**: Compromised systems can be rebuilt from scratch
4. **Encrypted storage**: LUKS encryption for sensitive data

### Why SOPS?

SOPS (Secrets OPerationS) was chosen over alternatives for several reasons:

- **Key-based encryption**: Supports multiple encryption backends (GPG, age, KMS)
- **YAML/JSON support**: Easy to integrate with Nix configurations
- **Version control friendly**: Encrypted secrets can safely be committed
- **Wide adoption**: Used by [sops-nix](https://github.com/Mic92/sops-nix), which integrates seamlessly with NixOS

## Evolution, Not Perfection

The configuration is acknowledged as **evolving**. No system is perfect from the start, and the documentation captures:

- **Current best practices**: What works now
- **Discarded approaches**: What didn't work and why
- **Future considerations**: Ideas not yet implemented

This honesty about the process is intentional. The goal is not to present an idealized system, but to document a real one with all its trade-offs and iterations.

## Reading This Documentation

Each subsequent section will build on these foundational principles:

- [Why Nix?](why-nix.md) - Exploring why Nix was chosen over alternatives
- [Why Not X?](why-not.md) - Understanding rejected alternatives
- [System Design](../design/declarative.md) - Deep dive into how the system is structured
- [Getting Started](../tutorial/installation.md) - How to replicate or adapt this setup

The philosophy sections set the stage for understanding the more practical implementation details that follow.
