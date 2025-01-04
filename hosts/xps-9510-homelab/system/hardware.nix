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
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;

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
