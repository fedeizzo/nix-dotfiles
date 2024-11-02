{ inputs, ... }:

{
  hardware = {
    bluetooth = {
      enable = false;
      powerOnBoot = false;
    };
    cpu.intel.updateMicrocode = true;
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

  imports = [
    inputs.nixos-hardware.nixosModules.dell-xps-15-9510
  ];
}
