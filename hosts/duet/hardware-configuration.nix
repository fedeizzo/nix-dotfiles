{ config, lib, pkgs, ... }:

{
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/d1e32dc1-4771-4421-b317-6a427c2d425e";
      fsType = "ext4";
    };
  };

  boot.initrd.luks.devices = {
    "LUKS-DUET-NIXOS-ROOTFS" = {
      device = "/dev/disk/by-uuid/b6136029-172a-4797-9510-9fec6137b956";
    };
  };

  nix.settings.max-jobs = lib.mkDefault 4;
}
