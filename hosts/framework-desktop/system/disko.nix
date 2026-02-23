{ lib, inputs, ... }:

let
  mtime = "7"; # weekly cleanup
in
{
  imports = [
    inputs.disko.nixosModules.disko
  ];
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/nvme0n1";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              label = "boot";
              name = "ESP";
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };

            luks = {
              size = "100%";
              label = "cryptroot";
              content = {
                type = "luks";
                name = "cryptroot";
                settings = {
                  allowDiscards = true;
                  bypassWorkqueues = true;
                };

                content = {
                  type = "btrfs";
                  extraArgs = [ "-L" "nixos" "-f" ];

                  subvolumes = {
                    "/root" = {
                      mountpoint = "/";
                      mountOptions = [
                        "subvol=root"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = [
                        "subvol=nix"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/var/log" = {
                      mountpoint = "/var/log";
                      mountOptions = [
                        "subvol=var/log"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/var/lib/sops" = {
                      mountpoint = "/var/lib/sops";
                      mountOptions = [
                        "subvol=var/lib/sops"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/persist" = {
                      mountpoint = "/persist";
                      mountOptions = [
                        "subvol=persist"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/games" = {
                      mountpoint = "/games";
                      mountOptions = [
                        "subvol=games"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };

                    "/home/media" = {
                      mountpoint = "/home/media";
                      mountOptions = [
                        "subvol=media"
                        "noatime"
                        "compress=zstd:3"
                        "ssd"
                        "space_cache=v2"
                      ];
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };

  boot.initrd.postDeviceCommands = lib.mkAfter ''
    mkdir /btrfs_tmp
    mount /dev/mapper/cryptroot /btrfs_tmp
    if [[ -e /btrfs_tmp/root ]]; then
        mkdir -p /btrfs_tmp/old_roots
        timestamp=$(date --date="@$(stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
        mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
    fi

    delete_subvolume_recursively() {
        IFS=$'\n'
        for i in $(btrfs subvolume list -o "$1" | cut -f 9- -d ' '); do
            delete_subvolume_recursively "/btrfs_tmp/$i"
        done
        btrfs subvolume delete "$1"
    }

    for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +${mtime}); do
        delete_subvolume_recursively "$i"
    done

    btrfs subvolume create /btrfs_tmp/root

    # Ensure media subvolume is owned by media user
    if [[ -d /btrfs_tmp/home/media ]]; then
        chown -R 800:1800 /btrfs_tmp/home/media
    fi

    umount /btrfs_tmp
  '';

  fileSystems = {
    "/persist".neededForBoot = true;
    "/var/log".neededForBoot = true;
    "/var/lib/sops".neededForBoot = true;
  };
}
