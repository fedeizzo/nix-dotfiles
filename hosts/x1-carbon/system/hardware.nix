{ inputs, ... }:

{
  hardware.cpu.intel.updateMicrocode = true;
  hardware.trackpoint = {
    enable = true;
    speed = 200;
    sensitivity = 200;
  };
  services.illum.enable = true; # Enable the brightness buttons
  services.pcscd.enable = true;
  services.fwupd.enable = true; # update firmware

  # https://nicholaslyz.com/blog/2024/04/29/how-to-undervolt-a-laptop-with-nixos/
  services.undervolt = {
    enable = true;
    coreOffset = -80;
    uncoreOffset = -80;
    gpuOffset = -60;
    turbo = 0; # enabled
    p1.limit = 20;
    p2.limit = 40;
  };

  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
  ];
}
