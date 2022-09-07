{ lib, config, ... }:

lib.mkIf (config.fs == "ext4") {
  fileSystems."/" = {
    device = "/dev/disk/by-label/root";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  boot.initrd.luks.devices."nixenc".device = "/dev/disk/by-uuid/d5f0de1a-613a-4116-a102-c4be3780071a";

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
