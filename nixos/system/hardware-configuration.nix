{ config, lib, pkgs, modulesPath,... }:

{
  imports =[
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  boot.initrd.luks.devices."nixenc".device = "/dev/disk/by-uuid/642c4b0c-fa7c-4410-8d38-aa6e8e1ccae4";

  fileSystems."/nix" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@nix" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/snapshot" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@snapshot" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@home" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/opt" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@opt" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/root" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@root" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/usr/local" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@usr_local" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/var/log" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@var_log" "autodefrag" "space_cache=v2" "noatime" "compress=zstd:2"];
    };

  fileSystems."/swap" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@swap" "autodefrag" "space_cache=v2" "noatime" "nocow"];
    };

  fileSystems."/tmp" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@tmp" "autodefrag" "space_cache=v2" "noatime" "nocow"];
    };

  fileSystems."/var/cache" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@var_cache" "autodefrag" "space_cache=v2" "noatime" "nocow"];
    };

  fileSystems."/var/tmp" =
    { device = "/dev/disk/by-uuid/6647b58f-a513-4a3f-97dc-1f9668354102";
      fsType = "btrfs";
      options = [ "subvol=@var_tmp" "autodefrag" "space_cache=v2" "noatime" "nocow"];
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/9B30-E7BC";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
