# IronClaw TODO

- [ ] Clean up leftover Hermes state on homelab after deploying the switch to IronClaw:
  - `/persist/var/lib/hermes`
  - `/persist/var/lib/signal-cli`
  - Backups of both paths in backrest, once no longer needed
- [ ] Port `/var/lib/ironclaw/config.toml` to a declarative Nix config once it is stable
  (see the TODO in `ironclaw.n.nix`).
