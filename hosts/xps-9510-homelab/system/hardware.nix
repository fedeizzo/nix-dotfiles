{ inputs, pkgs, lib, config, ... }:

{
  imports = [
    inputs.nixos-hardware.nixosModules.dell-xps-15-9510
    inputs.nixos-hardware.nixosModules.dell-xps-15-9510-nvidia
  ];

  hardware = {
    bluetooth = {
      enable = false;
      powerOnBoot = false;
    };
    cpu.intel.updateMicrocode = true;
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "570.86.16"; # use new 570 drivers
      sha256_64bit = "sha256-RWPqS7ZUJH9JEAWlfHLGdqrNlavhaR1xMyzs8lJhy9U=";
      openSha256 = "sha256-DuVNA63+pJ8IB7Tw2gM4HbwlOh1bcDg2AN2mbEU9VPE=";
      settingsSha256 = "sha256-9rtqh64TyhDF5fFAYiWl3oDHzKJqyOW3abpcf2iNRT8=";
      usePersistenced = false;
    };

    # graphics = {
    #   enable = true;
    #   extraPackages = with pkgs; [
    #     intel-media-driver
    #     intel-vaapi-driver
    #     vaapiVdpau
    #     intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
    #     vpl-gpu-rt # QSV on 11th gen or newer
    #     intel-media-sdk # QSV up to 11th gen
    #   ];
    # };
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
    # tlp = {
    #   enable = true;
    #   settings = {
    #     TLP_ENABLE = 1;
    #     TLP_DEFAULT_MODE = "AC";
    #     WIFI_PWR_ON_AC = "on";
    #     WIFI_PWR_ON_BAT = "on";
    #     CPU_HWP_ON_AC = "performance";
    #     CPU_HWP_ON_BAT = "performance";
    #     DEVICES_TO_ENABLE_ON_STARTUP = "wifi";
    #   };
    # };
    thermald = {
      enable = true;
    };
    # auto-cpufreq.enable = true;
    upower = {
      enable = true;
      usePercentageForPolicy = false;
    };
  };
}
