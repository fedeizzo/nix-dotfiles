{
  flake.modules.nixos.forge = {
    # homelab's mixer key: deploy-rs and the nixos-anywhere install run from homelab.
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILvOfJ4CWTMzkM3aH71bB616u5vxJQ46upPUTgqgOp4U mixer@homelab"
    ];
  };
}
