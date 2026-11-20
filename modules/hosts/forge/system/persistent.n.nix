{
  # Persist every fi.services toPersist entry across ephemeral-root wipes,
  # mirroring homelab's persistent.n.nix.
  flake.modules.nixos.forge = { config, ... }: {
    environment.persistence."/persist".directories =
      builtins.concatLists (map (el: el.toPersist) config.fi.services);
  };
}
