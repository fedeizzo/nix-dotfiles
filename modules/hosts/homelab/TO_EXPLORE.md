Similar NixOS Homelab Setups

1. Truxnell's Setup (Most Similar)

- ✅ Impermanence with btrfs
- ✅ Restic backups to Backblaze B2
- ✅ Per-service backup declarations (same pattern as yours!)
- ✅ Retention policy (3 daily, 7 weekly, 5 monthly, 12 yearly)
- Uses ZFS instead of manual btrfs snapshots

Link: https://truxnell.github.io/nix-config/

2. JManch/nixos

- ✅ Comprehensive restic module
- ✅ Failure notifications for backups
- ✅ Automated backup tasks
- Infrastructure-as-code approach

Link: https://github.com/JManch/nixos

3. Swarsel/.dotfiles

- ✅ Full homelab (2 servers + MicroVMs)
- ✅ Self-hosted services (Jellyfin, Nextcloud, Matrix, Navidrome)
- ✅ Impermanence setup
- Manages entire IT infrastructure declaratively

4. NotAShelf's Blog (Popular Tutorial)

- ✅ Full disk encryption + impermanence
- ✅ Btrfs with automatic snapshots
- ✅ Detailed disaster recovery
- One of the most comprehensive guides

Link: https://notashelf.dev/posts/impermanence

5. Guekka's Blog (Server-Focused)

- ✅ "NixOS as a server" series
- ✅ Impermanence for servers
- Production-oriented approach

Link: https://guekka.github.io/nixos-server-1/
