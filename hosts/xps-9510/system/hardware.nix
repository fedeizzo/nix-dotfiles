{ pkgs, inputs, ... }:

let
  fs-diff = pkgs.writeShellScriptBin "fs-diff" ''
    #!/usr/bin/env bash
    # fs-diff.sh
    doas mkdir -p /mnt
    doas mount -o subvol=/ /dev/mapper/nixenc /mnt
    set -euo pipefail

    OLD_TRANSID=$(doas btrfs subvolume find-new /mnt/root-blank 9999999)
    OLD_TRANSID=$(echo $OLD_TRANSID | awk -F' ' '{print $4}')

    doas btrfs subvolume find-new "/mnt/root" "$OLD_TRANSID" |
    sed '$d' |
    cut -f17- -d' ' |
    sort |
    uniq |
    while read path; do
      path="/$path"
      if [ -L "$path" ]; then
        : # The path is a symbolic link, so is probably handled by NixOS already
      elif [ -d "$path" ]; then
        : # The path is a directory, ignore
      else
        echo "$path"
      fi
    done
    doas umount -R /mnt
  '';
in
{
  environment.systemPackages = [ fs-diff ];
  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir -p /mnt

    # We first mount the btrfs root to /mnt
    # so we can manipulate btrfs subvolumes.
    mount -o subvol=/ /dev/mapper/nixenc /mnt

    # While we're tempted to just delete /root and create
    # a new snapshot from /root-blank, /root is already
    # populated at this point with a number of subvolumes,
    # which makes `btrfs subvolume delete` fail.
    # So, we remove them first.
    #
    # /root contains subvolumes:
    # - /root/var/lib/portables
    # - /root/var/lib/machines
    #
    # I suspect these are related to systemd-nspawn, but
    # since I don't use it I'm not 100% sure.
    # Anyhow, deleting these subvolumes hasn't resulted
    # in any issues so far, except for fairly
    # benign-looking errors from systemd-tmpfiles.
    btrfs subvolume list -o /mnt/root |
    cut -f9 -d' ' |
    while read subvolume; do
      echo "deleting /$subvolume subvolume..."
      btrfs subvolume delete "/mnt/$subvolume"
    done &&
    echo "deleting /root subvolume..." &&
    btrfs subvolume delete /mnt/root

    echo "restoring blank /root subvolume..."
    btrfs subvolume snapshot /mnt/root-blank /mnt/root

    # Once we're done rolling back to a blank snapshot,
    # we can unmount /mnt and continue on the boot process.
    umount /mnt
  '';
  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/root";
      fsType = "btrfs";
      options = [ "subvol=root" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
    };
    "/nix" = {
      device = "/dev/disk/by-label/root";
      fsType = "btrfs";
      options = [ "subvol=nix" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
    };
    "/var/log" = {
      device = "/dev/disk/by-label/root";
      fsType = "btrfs";
      options = [ "subvol=log" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
      neededForBoot = true;
    };
    "/var/lib/sops" = {
      device = "/dev/disk/by-label/root";
      fsType = "btrfs";
      options = [ "subvol=sops" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
      neededForBoot = true;
    };
    "/persist" = {
      device = "/dev/disk/by-label/root";
      fsType = "btrfs";
      options = [ "subvol=persist" "noautodefrag" "space_cache=v2" "noatime" "compress=zstd:3" "ssd" "discard" ];
      neededForBoot = true;
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
    inputs.nixos-hardware.nixosModules.dell-xps-15-9510
  ];
}
