{ pkgs, ... }:

{
  # environment.systemPackages = [ pkgs.tpm2-tss ];
  boot = {
    supportedFilesystems = [ "ext4" "btrfs" ];

    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    tmp.cleanOnBoot = true;
    kernelPackages = pkgs.linuxPackages_latest;
    # kernel.sysctl = {
    #   "vm.swappiness" = 10;
    #   "dev.i915.perf_stream_paranoid" = 0;
    # };
    initrd = {
      systemd.enable = true;
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "thunderbolt"
        "usbhid"
        "uas"
        "sd_mod"
      ];
      kernelModules = [ "amdgpu" ];
    };
    kernelModules = [ "kvm-amd" ];
    extraModulePackages = [ ];
    kernelParams = [
      "nohibernate"
      "amdgpu.gttsize=98304" # 96 GiB
      "ttm.pages_limit=25165824" # 96 GiB (25165824 × 4 KiB = 96 GiB)
      "amd_iommu=off"
    ];
    consoleLogLevel = 0;
  };
}
