{ inputs, pkgs, lib, config, ... }:

{
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

    # fancontrol = {
    #   enable = true;
    # };

    amdgpu.initrd.enable = true;
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    graphics = {
      enable = true;
      # extraPackages = with pkgs; [
      #   intel-media-driver # For Broadwell (2014) or newer processors. LIBVA_DRIVER_NAME=iHD
      #   libva-vdpau-driver # Previously vaapiVdpau
      #   intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
      #   vpl-gpu-rt # QSV on 11th gen or newer
      #   intel-ocl # OpenCL support
      # ];
    };
  };

  zramSwap.enable = true;

  services = {
    smartd = {
      enable = true;
      autodetect = true;
      notifications.wall.enable = true;
    };
    fstrim = {
      enable = true;
      interval = "weekly";
    };
    pcscd.enable = true;
    fwupd.enable = true; # update firmware
  };

  environment.systemPackages = with pkgs; [
    acpi
    lm_sensors
  ];

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  services = {
    thermald = {
      enable = true;
    };
    # auto-cpufreq.enable = true;
    upower = {
      enable = true;
      usePercentageForPolicy = false;
    };
  };
  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];
}
