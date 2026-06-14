{
  flake.modules.nixos.power = { pkgs, ... }: {
    powerManagement = {
      enable = true;
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

    environment.systemPackages = with pkgs; [
      acpi
      lm_sensors
    ];
  };
}
