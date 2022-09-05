{ config, pkgs, ... }:

let
  polyDarkTheme = pkgs.fetchgit {
    url = "https://github.com/shvchk/poly-dark.git";
    rev = "ea17ade04e49fd0ddfc04474b00cdbbdd81c6a3e";
    sha256 = "sha256-7hvrlyi31yNFvtJaaGCL0MZmod+TmRjIPsNqtyVddZg=";
  };
in
{
  boot = {
    loader = {
      efi.canTouchEfiVariables = false;
      grub = {
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
    };
    cleanTmpDir = true;
    kernelPackages = pkgs.linuxPackages_latest;
    kernel.sysctl = {
      "vm.swappiness" = 10;
      "dev.i915.perf_stream_paranoid" = 0;
    };
    extraModprobeConfig = ''
      options iwlwifi power_save=1 disable_11ax=1
    '';
    plymouth = {
      enable = true;
      themePackages = [ pkgs.adi1090x-plymouth ];
      theme = "owl";
    };
    initrd.availableKernelModules = [
      "thunderbolt"
      "vmd"
      "xhci_pci"
      "ahci"
      "nvme"
      "usbhid"
      "usb_storage"
      "sd_mod"
      "sr_mod"
      "rtsx_pci_sdmmc"
    ];
    initrd.kernelModules = [ "i915" ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
    kernelParams = [
      "i915.enable_fbc=1"
      "i915.enable_psr=2"
      "i915.edp_vswing=2"
      "quiet"
      "udev.log_level=3"
      "splash"
      "boot.shell_on_fail" #https://discourse.nixos.org/t/12304#post_2
      # dell
      "acpi_rev_override"
      "mem_sleep_default=deep"
      "nvidia-drm.modeset=1"
    ];
    consoleLogLevel = 0;
  };
  system.activationScripts.copyGrubTheme = ''
    mkdir -p /boot/themes
    cp -R ${polyDarkTheme}/ /boot/themes/poly-dark
  '';
}
