{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    bash
  ];
  environment.etc."machine-id".source = "/nix/persist/etc/machine-id";
  boot.loader.grub.enable = true;

  boot.loader.grub.efiSupport = true;
  boot.loader.grub.device = "nodev";
}
