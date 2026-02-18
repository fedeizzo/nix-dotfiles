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
      # systemd.enable = true;
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
      # Caps pinned memory to 124 GiB; 32505856 × 4 KiB = 126976 MiB = 124 GiB
      # Reserving a minimum of 4 GiB for the OS (max 124 GiB for iGPU)
      "amdgpu.gttsize=126976" # Caps GPU unified memory to 124 GiB; 126976 MiB ÷ 1024 = 124 GiB
      "ttm.pages_limit=32505856" # Caps pinned memory to 124 GiB; 32505856 × 4 KiB = 126976 MiB = 124 GiB
      "amd_iommu=off" # Disables IOMMU for lower latency
    ];
    consoleLogLevel = 0;
  };
}
