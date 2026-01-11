{ inputs, ... }:

{
  imports = [
    inputs.disko.nixosModules.disko
  ];

  disko.enableConfig = true;

  disko.devices = {
    disk.main = {
      type = "disk";
      device = "/dev/sda"; # 500GB external SSD

      content = {
        type = "gpt";
        partitions = {
          boot = {
            size = "1G";
            type = "EF00"; # EFI System Partition
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
              mountOptions = [ "umask=0077" ];
            };
          };

          pimsd = {
            size = "100G";
            type = "8300";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/var/lib/pimsd"; # USB ISOs
            };
          };

          pipst = {
            size = "10G";
            type = "8300";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/var/lib/pipst"; # PiKVM persistence
            };
          };

          root = {
            size = "100%";
            type = "8300";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
            };
          };
        };
      };
    };
  };
}
