# Discarded Alternatives and Design Decisions

## Philosophy of This Section

This document captures decisions that were considered but ultimately rejected. Documenting what we *didn't* do is just as important as documenting what we did, as it provides context and rationale for the current architecture.

## Why Not ZFS?

**Considered**: Using ZFS filesystem instead of BTRFS for snapshots and rollback capabilities.

**Rejected because**:
- Higher memory requirements for ZFS ARC cache
- More complex initial setup for the specific "erase on boot" use case
- The erase-on-boot approach doesn't need ZFS's advanced features (copy-on-write at block level, compression, etc.)
- BTRFS is simpler and sufficient for the snapshot requirements

**Current approach**: BTRFS with automatic snapshots via impermanence and `nixos-rebuild switch --rollback`.

## Why Not Tmpfs for Everything?

**Considered**: Mounting `/` (root) as tmpfs for complete statelessness on every boot.

**Rejected because**:
- Performance degradation from RAM-based storage for the entire filesystem
- Memory constraints on lower-spec machines
- Logs and runtime data would need careful management
- The hybrid approach (BTRFS snapshots + selective tmpfs) provides better balance

**Current approach**: BTRFS snapshots with selective tmpfs mounting for temporary directories (`/var/log`, `/var/tmp`).

## Why Not Other Dotfiles Managers?

**Considered alternatives**:
- **dotdrop**: Shell-based, requires careful ordering
- **stow**: Symlink-based, doesn't handle configuration merging
- **yadm**: Git-based, good for versioning but limited configuration logic
- **GNU Stow**: Simple but doesn't handle system-level configuration

**Why not**:
- These are mostly imperative (do this, then that) vs declarative (this is the state)
- No package management integration
- No automatic cleanup of unused configurations
- Less suited for multi-machine management

**Selected**: Home Manager as part of the Nix ecosystem, providing declarative configuration that integrates with the rest of the system.

## Why Not Other Secrets Management?

**Considered alternatives**:
- **HashiCorp Vault**: Too heavy for personal use, requires running service
- **Ansible Vault**: Works but less integrated with Nix ecosystem
- **Age-encrypted files**: Similar to SOPS but without YAML/JSON support
- **Environment variables**: Not suitable for persistent configuration

**Why SOPS**:
- Works with age, GPG, KMS, and other encryption backends
- Seamless integration with sops-nix
- Encrypted YAML/JSON files integrate naturally with Nix
- Can decrypt at build time or runtime
- Wide adoption in Nix community

## Why Not Other Desktop Environments?

**Considered alternatives**:
- **GNOME**: Heavy, not as customizable as desired
- **XFCE**: Lightweight but requires more manual configuration
- **KDE Plasma**: Chosen via plasma-manager for good balance
- **Hyprland**: Selected for personal laptop as tiling window manager
- **macOS Desktop**: Work machine limitation

**Selection criteria**:
- Personal laptop (X1 Nano): Hyprland for modern features and customization
- Homelab/Server: No desktop environment, pure command line
- Work machine: macOS as provided by employer

## Why Not Other Deployment Tools?

**Considered alternatives**:
- **Ansible**: Excellent but imperative, different paradigm
- **NixOps**: Deprecated in favor of deploy-rs
- **systemd-networkd**: Native but limited for remote deployment
- **Manual SSH deployment**: Error-prone, not automated

**Why deploy-rs**:
- Native Nix integration
- Built on top of Nix's strengths (reproducibility, atomic updates)
- Supports remote builds
- Magic rollback capability
- Simplified compared to managing SSH scripts manually

## Why Not Other Window Managers?

**Considered alternatives for personal laptop**:
- **i3**: Classic tiling WM, but considered mature/old
- **bspwm**: Another tiling WM option
- **sway**: Wayland-compatible i3, but Hyprland is more modern

**Why Hyprland**:
- Modern Wayland compositor
- Built-in animations and effects
- Active development
- Better GPU acceleration
- Easier configuration via Nix modules

## Why Not Other Package Managers?

**Considered alternatives**:
- **Nixpkgs unstable only**: Chose not to use constantly
- **Flatpak**: Good for sandboxing but duplicates dependencies
- **Snap**: Heavy, not as well integrated with Nix
- **AUR (Arch User Repository)**: Different ecosystem, not Nix

**Why Nixpkgs with overlays**:
- Centralized package management
- Reproducible builds
- No conflicts between versions
- Can customize packages via overlays
- NUR (Nix User Repository) provides community extensions

## Why Not Other Version Control?

**Considered alternatives**:
- **Git submodules**: More complex, less flexible
- **Git worktrees**: Good for development but not for configuration
- **Separate repositories**: Too much fragmentation
- **Monorepo approach**: Chose to keep everything together

**Why single repository**:
- All configuration in one place
- Easier to reason about dependencies
- Simplifies deployment and updates
- Better version control for cross-component changes

## Why Not Other Backup Strategies?

**Considered alternatives**:
- **Full system backups**: Redundant with declarative approach
- **Incremental snapshots only**: Doesn't provide fresh system
- **Cloud backups only**: No local recovery option
- **External drives**: Manual process, no automation

**Current approach**:
- Impermanence handles the "erase and rebuild" philosophy
- Secrets encrypted and version controlled
- System state declared, not backed up
- Focus on reproducibility rather than backup

## Why Not Other Flake Management?

**Considered alternatives**:
- **Manual flake configuration**: Too error-prone
- **Other flake frameworks**: Flake-parts provides best structure
- **No flakes**: Outdated, less reproducible

**Why flake-parts**:
- Clean modular structure
- Easy to share modules
- Built-in system separation
- Well-maintained and widely adopted

## Why Not Other Encryption Keys?

**Considered alternatives**:
- **GPG only**: Good but less modern
- **KMS**: Requires cloud provider setup
- **SOPS with multiple keys**: Could work but age is simpler

**Why age**:
- Simpler key format (single file vs GPG keys)
- Faster key generation
- Easier to manage for personal use
- Works well with SOPS
- Less complex than GPG key management

## Why Not Other Network Solutions?

**Considered alternatives**:
- **DNS-based service discovery**: More complex than needed
- **Custom service registry**: Overhead without benefit
- **Static IPs only**: DHCP with static leases is sufficient

**Current approach**: Simple network configuration with DHCP and static leases for important services.

## Conclusion

Every decision documented here was chosen after considering alternatives. The key criteria consistently applied:

1. **Simplicity**: Can this be understood and maintained?
2. **Reproducibility**: Will it work the same way every time?
3. **Integration**: Does it fit with the rest of the ecosystem?
4. **Maintainability**: Can this be managed long-term?
5. **Flexibility**: Can it adapt to changing needs?

The "why not" decisions are just as important as the "why" decisions because they show the thought process behind the architecture. Each rejection represents a trade-off that was consciously made.

## Next Steps

With an understanding of the architecture and rejected alternatives, we can now explore:
- [The actual system structure](../machines/inventory.md)
- [How each component works](../design/declarative.md)
- [Practical usage and setup](../tutorial/installation.md)
