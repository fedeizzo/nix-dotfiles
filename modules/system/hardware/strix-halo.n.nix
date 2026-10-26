# Framework Desktop (AMD Ryzen AI Max+ 395 "Strix Halo", 128 GB unified memory).
# Shared by every host built on this board: homelab, forge.
{
  flake-file.inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  flake-file.inputs.nix-amd-ai.url = "github:noamsto/nix-amd-ai";

  flake.modules.nixos.strix-halo = { inputs, pkgs, lib, config, pkgs-unstable, ... }: {
    imports = [
      inputs.nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series
    ];

    nixpkgs.config = {
      rocmSupport = true;
      hostPlatform = "x86_64-linux";
    };

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

    hardware = {
      bluetooth = {
        enable = false;
        powerOnBoot = false;
      };
      enableRedistributableFirmware = true;

      amdgpu.initrd.enable = true;
      cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
      graphics = {
        enable = true;
      };
    };

    powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

    systemd.tmpfiles.rules = [
      "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
    ];

    services.tuned = {
      enable = true;
      profiles = {
        strix-halo = {
          main = {
            include = "accelerator-performance";
          };
        };
      };
    };

    systemd.services.tuned-set-profile = {
      description = "Set TuneD profile";
      after = [ "tuned.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs-unstable.tuned}/bin/tuned-adm profile accelerator-performance";
      };
    };

    environment.systemPackages = with pkgs; [
      amdgpu_top
      tpm2-tss
      # rocmPackages.amdsmi
      rocmPackages.rocm-smi
      rocmPackages.rocminfo
      btop-rocm
    ];
  };
}
