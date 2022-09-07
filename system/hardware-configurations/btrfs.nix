{ lib, pkgs, config, ... }:

let
  fs-diff = pkgs.writeShellScriptBin "fs-diff" ''
    #!/usr/bin/env bash
    # fs-diff.sh
    set -euo pipefail

    OLD_TRANSID=$(sudo btrfs subvolume find-new /mnt/root-blank 9999999)
    OLD_TRANSID=$${OLD_TRANSID#transid marker was }

    sudo btrfs subvolume find-new "/mnt/root" "$OLD_TRANSID" |
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
  '';
in
lib.mkIf (config.fs == "btrfs") {
  environment.systemPackages = [ fs-diff ];
  boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
    mkdir -p /mnt

    # We first mount the btrfs root to /mnt
    # so we can manipulate btrfs subvolumes.
    mount -o subvol=/ /dev/mapper/enc /mnt

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
  fileSystems."/" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=root"
      "noautodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard"
    ];
  };

  boot.initrd.luks.devices."nixenc" = {
    device = "/dev/disk/by-label/nixenc";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=home"
      "noautodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard"
    ];
  };
  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=nix"
      "noautodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard"
    ];
  };
  fileSystems."/var/log" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=log"
      "noautodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard"
    ];
    neededForBoot = true;
  };
  fileSystems."/persist" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=persist"
      "noautodefrag"
      "space_cache=v2"
      "noatime"
      "compress=zstd:3"
      "ssd"
      "discard"
    ];
  };
  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  swapDevices = [ ];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
