{ pkgs, ... }:

{
  boot = {
    supportedFilesystems = [ "ext4" "ntfs" ];
    loader = {
      efi.canTouchEfiVariables = false;
      grub = {
        enable = true;
        device = "nodev";
        efiSupport = true;
        useOSProber = false;
        enableCryptodisk = true;
        efiInstallAsRemovable = true;
        configurationLimit = 5;
      };
    };
    tmp.cleanOnBoot = true;
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
    ];
    consoleLogLevel = 0;
    # required for build raspberry configuration
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };
}

