{ config, pkgs, ... }:

{
  #################################
  # BOOT
  #################################
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages;
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };
}
