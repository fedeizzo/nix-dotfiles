{
  flake.modules.nixos.framework-desktop = { pkgs, ... }: {
    boot = {
      loader.efi.canTouchEfiVariables = true;

      initrd = {
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
      kernelParams = [
        "nohibernate"
        # Caps pinned memory to 124 GiB; 32505856 × 4 KiB = 126976 MiB = 124 GiB
        # Reserving a minimum of 4 GiB for the OS (max 124 GiB for iGPU)
        "amdgpu.gttsize=126976" # Caps GPU unified memory to 124 GiB; 126976 MiB ÷ 1024 = 124 GiB
        "ttm.pages_limit=32505856" # Caps pinned memory to 124 GiB; 32505856 × 4 KiB = 126976 MiB = 124 GiB
        # "amd_iommu=off" # Disables IOMMU for lower latency
        "amd_iommu=on" # Required for XDNA / NPU SVA binding, required for NPU
        "iommu=pt"     # Passthrough mode for better performance, required for NPU
      ];
    };
  };
}
