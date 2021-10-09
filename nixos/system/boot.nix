{ config, pkgs, ... }:

let
  polyDarkTheme = pkgs.fetchgit {
    url = "https://github.com/shvchk/poly-dark.git";
    rev = "ea17ade04e49fd0ddfc04474b00cdbbdd81c6a3e";
    sha256 = "sha256-7hvrlyi31yNFvtJaaGCL0MZmod+TmRjIPsNqtyVddZg=";
  };
in
{
  #################################
  # BOOT
  #################################
  # boot.loader.systemd-boot = {
  #   enable = true;
  #   editor = false;
  # };
  # boot.loader.efi.canTouchEfiVariables = true;
  # dell
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    version = 2;
    efiSupport = true;
    useOSProber = true;
    enableCryptodisk = true;
    efiInstallAsRemovable = true;
    configurationLimit = 5;
    splashImage = "${polyDarkTheme}/background.png";
    extraConfig = ''
      set theme=($drive1)//themes/poly-dark/theme.txt
    '';
  };
  system.activationScripts.copyGrubTheme = ''
    mkdir -p /boot/themes
    cp -R ${polyDarkTheme}/ /boot/themes/poly-dark
  '';
  boot.loader.efi.canTouchEfiVariables = false;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
    "dev.i915.perf_stream_paranoid" = 0;
  };
  # dell
  boot.extraModprobeConfig = ''
    options iwlwifi power_save=1 disable_11ax=1
  '';
  boot.plymouth = {
    enable = true;
    themePackages = [ pkgs.adi1090x-plymouth ];
    theme = "owl";
  };
}
