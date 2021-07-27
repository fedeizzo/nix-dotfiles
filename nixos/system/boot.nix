{ config, pkgs, ... }:

{
  #################################
  # BOOT
  #################################
  boot.loader.systemd-boot = {
    enable = true;
    editor = false;
  };
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
