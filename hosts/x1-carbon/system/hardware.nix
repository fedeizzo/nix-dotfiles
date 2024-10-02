{ inputs, ... }:

{
  hardware.cpu.intel.updateMicrocode = true;
  services.pcscd.enable = true;
  services.fwupd.enable = true; # update firmware

  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
  ];
}
