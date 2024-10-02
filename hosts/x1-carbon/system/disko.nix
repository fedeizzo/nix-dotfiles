{ ... }:

{
  disko.devices = {
    disk = {
      nvme0n1 = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              label = "boot";
              name = "ESP";
              size = "1024M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [
                  "defaults"
                ];
              };
            };

            swap = {
              size = "16G";
              label = "swap";
              content = {
                type = "swap";
                randomEncryption = true;
                resumeDevice = true;
              };
            };

            luks = {
              size = "100%";
              label = "nixenc";
              askPassword = true;
              content = {
                type = "luks";
                name = "nixenc";
                settings = {
                  allowDiscards = true;
                  bypassWorkqueues = true;
                };

                content = {
                  nix = {
                    size = "100%";
                    content = {
                      type = "filesystem";
                      format = "ext4";
                      mountpoint = "/nix";
                    };
                  };
                };
              };
            };
          };
        };
      };
    };

    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=2G"
        "defaults"
        "mode=755"
      ];
    };
  };
}
