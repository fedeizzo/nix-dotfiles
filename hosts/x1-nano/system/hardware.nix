{ inputs, ... }:

{
  hardware = {
    trackpoint = {
      enable = true;
      speed = 200;
      sensitivity = 200;
      emulateWheel = true;
    };
    cpu.intel.updateMicrocode = true;
    i2c.enable = true; # external monitor control
  };

  services = {
    # illum.enable = true; # Enable the brightness buttons
    pcscd.enable = true;
    fwupd.enable = true; # update firmware
  };

  zramSwap.enable = true;

  # https://nicholaslyz.com/blog/2024/04/29/how-to-undervolt-a-laptop-with-nixos/
  # services.undervolt = {
  #   enable = true;
  #   coreOffset = -90;
  #   uncoreOffset = -90;
  #   gpuOffset = -70;
  #   turbo = 0; # enabled
  #   p1.limit = 20;
  #   p1.window = 20;
  #   p2.limit = 40;
  #   p2.window = 10;
  # };

  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-nano
  ];
}
