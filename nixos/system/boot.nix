{ config, pkgs, ... }:

{
  #################################
  # BOOT
  #################################
  boot.loader.systemd-boot = {
    enable = true;
    editor = false;
  };
  # dell
  # boot.loader.grub = {
  #   enable = true;
  #   theme = pkgs.nixos-grub2-theme;
  #   device = "nodev";
  #   version = 2;
  #   efiSupport = true;
  #   useOSProber = true;
  #   enableCryptodisk = true;
  #   efiInstallAsRemovable = true;
  # };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };
  # dell
  # boot.extraModprobeConfig = ''
  #   options iwlwifi power_save=1 disable_11ax=1
  # '';
}
