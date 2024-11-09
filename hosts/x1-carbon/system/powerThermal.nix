{ lib, pkgs, ... }:

{
  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

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
