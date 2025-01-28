{ lib, pkgs, ... }:

{
  powerManagement = {
    enable = true;
    cpuFreqGovernor = lib.mkDefault "powersave";
    powertop.enable = true;
  };

  services = {
    thermald = {
      enable = true;
    };
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
