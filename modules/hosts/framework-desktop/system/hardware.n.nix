{
  flake-file.inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  flake-file.inputs.nix-amd-ai.url     = "github:noamsto/nix-amd-ai";

  flake.modules.nixos.framework-desktop = { inputs, pkgs, lib, config, pkgs-unstable, ... }: {
    imports = [
      inputs.nixos-hardware.nixosModules.framework-desktop-amd-ai-max-300-series
    ];

    systemd.services.jellyfin.environment.LIBVA_DRIVER_NAME = "iHD"; # Or "i965" if using older driver
    environment.sessionVariables = { LIBVA_DRIVER_NAME = "iHD"; }; # Same here
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
  };
}
