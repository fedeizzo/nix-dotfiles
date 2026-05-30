{ lib, inputs, pkgs, ... }:

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
                        "subvol=home/media"
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
  boot.initrd.systemd.services.btrfs-wipe-root = {
      description = "Wipe and recreate BTRFS root subvolume";
      wantedBy = [ "initrd.target" ];

      # Must wait for LUKS to be unlocked
      requires = [ "dev-mapper-cryptroot.device" ];
      after = [ "dev-mapper-cryptroot.device" ];

      # Must finish before the actual root filesystem is mounted
      before = [ "sysroot.mount" ];

      unitConfig.DefaultDependencies = "no";
      serviceConfig.Type = "oneshot";

      script = ''
        mkdir -p /btrfs_tmp
        mount -t btrfs /dev/mapper/cryptroot /btrfs_tmp

        if [[ -e /btrfs_tmp/root ]]; then
            mkdir -p /btrfs_tmp/old_roots
            timestamp=$(${pkgs.coreutils}/bin/date --date="@$(${pkgs.coreutils}/bin/stat -c %Y /btrfs_tmp/root)" "+%Y-%m-%-d_%H:%M:%S")
            ${pkgs.coreutils}/bin/mv /btrfs_tmp/root "/btrfs_tmp/old_roots/$timestamp"
        fi

        delete_subvolume_recursively() {
            IFS=$'\n'
            for i in $(${pkgs.btrfs-progs}/bin/btrfs subvolume list -o "$1" | ${pkgs.coreutils}/bin/cut -f 9- -d ' '); do
                delete_subvolume_recursively "/btrfs_tmp/$i"
            done
            ${pkgs.btrfs-progs}/bin/btrfs subvolume delete "$1"
        }

        for i in $(${pkgs.findutils}/bin/find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +${mtime}); do
            delete_subvolume_recursively "$i"
        done

        ${pkgs.btrfs-progs}/bin/btrfs subvolume create /btrfs_tmp/root

        # Ensure media subvolume is owned by media user
        if [[ -d /btrfs_tmp/home/media ]]; then
            ${pkgs.coreutils}/bin/chown -R 800:1800 /btrfs_tmp/home/media
        fi

        umount /btrfs_tmp
      '';
    };

  fileSystems = {
    "/persist".neededForBoot = true;
    "/var/log".neededForBoot = true;
    "/var/lib/sops".neededForBoot = true;
  };
}
