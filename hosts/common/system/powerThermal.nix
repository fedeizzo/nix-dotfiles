{ lib, pkgs, ... }:

{
  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services = {
    tlp = {
      enable = true;
      settings = {
        TLP_ENABLE = 1;
        TLP_DEFAULT_MODE = "AC";
        WIFI_PWR_ON_AC = "on";
        WIFI_PWR_ON_BAT = "on";
        CPU_HWP_ON_AC = "performance";
        CPU_HWP_ON_BAT = "performance";
        DEVICES_TO_ENABLE_ON_STARTUP = "bluetooth wifi";
      };
    };
    thermald = {
      enable = true;
    };
    auto-cpufreq.enable = true;
    upower = {
      enable = true;
      usePercentageForPolicy = false;
    };
  };

  programs.ccache.enable = true;

  environment.systemPackages = with pkgs; [
    acpi
    lm_sensors
  ];
}
