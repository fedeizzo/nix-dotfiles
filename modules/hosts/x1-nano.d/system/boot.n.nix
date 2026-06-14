{
  flake.modules.nixos.x1-nano = {
    boot = {
      supportedFilesystems = [ "ntfs" ];
      
      kernel.sysctl = {
        "vm.swappiness" = 10;
        "dev.i915.perf_stream_paranoid" = 0;
      };
      
      extraModprobeConfig = ''
        options iwlwifi power_save=1 disable_11ax=1
      '';
      
      initrd = {
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
          "i2c_dev" # ddcutil: controlling external display
        ];
        kernelModules = [ "i915" ];
        systemd.enable = false;
      };
      
      kernelModules = [ "kvm-intel" "uinput" ];
      
      kernelParams = [
        "i915.enable_fbc=1"
        "i915.enable_psr=2"
        "i915.edp_vswing=2"
        "quiet"
        "udev.log_level=3"
        "splash"
        "boot.shell_on_fail" #https://discourse.nixos.org/t/12304#post_2
      ];
      
      # required for build raspberry configuration
      binfmt.emulatedSystems = [ "aarch64-linux" ];
    };
  };
}
