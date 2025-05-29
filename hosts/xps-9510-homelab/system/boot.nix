{ pkgs, ... }:

{
  environment.systemPackages = [ pkgs.tpm2-tss ];
  boot = {
    supportedFilesystems = [ "ext4" ];

    loader.systemd-boot.enable = true;

    tmp.cleanOnBoot = true;
    kernelPackages = pkgs.linuxPackages_6_14;
    kernel.sysctl = {
      "vm.swappiness" = 10;
      "dev.i915.perf_stream_paranoid" = 0;
    };
    extraModprobeConfig = ''
      options iwlwifi power_save=1 disable_11ax=1
    '';
    initrd = {
      systemd.enable = true;
      availableKernelModules = [
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
      kernelModules = [ "i915" ];
    };
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
}
