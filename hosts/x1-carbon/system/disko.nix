{ lib, ... }:

{
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
                mountOptions = [
                  "defaults"
                ];
              };
            };
            luks = {
              size = "100%";
              label = "nixenc";
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
                      mountOptions = [ "subvol=root" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
                    };
                    "/nix" = {
                      mountpoint = "/nix";
                      mountOptions = [ "subvol=nix" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
                    };
                    "/var/log" = {
                      mountpoint = "/var/log";
                      mountOptions = [ "subvol=var/log" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
                    };
                    "/var/lib/sops" = {
                      mountpoint = "/var/lib/sops";
                      mountOptions = [ "subvol=var/lib/sops" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
                    };
                    "/persist" = {
                      mountpoint = "/persist";
                      mountOptions = [ "subvol=persist" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
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
    mount /dev/root_vg/root /btrfs_tmp
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

    for i in $(find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30); do
        delete_subvolume_recursively "$i"
    done

    btrfs subvolume create /btrfs_tmp/root
    umount /btrfs_tmp
  '';
  fileSystems."/persist".neededForBoot = true;
  fileSystems."/var/log".neededForBoot = true;
  fileSystems."/var/lib/sops".neededForBoot = true;
}
