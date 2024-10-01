{ inputs, ... }:

{
  fileSystems = {
    "/" = {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=2G" "mode=755" ];
    };
    "/nix" = {
      device = "/dev/disk/by-label/root";
      fsType = "ext4";
    };
    "/boot" =
      {
        device = "/dev/disk/by-label/boot";
        fsType = "vfat";
      };
  };

  boot.initrd.luks.devices."nixenc".device = "/dev/disk/by-label/nixenc";

  swapDevices = [ ];

  hardware.cpu.intel.updateMicrocode = true;
  services.pcscd.enable = true;
  services.fwupd.enable = true; # update firmware

  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
  ];
}
