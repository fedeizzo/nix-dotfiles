#!/bin/sh
set -e
HOSTNAME="rasp-nixos"
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"
NIX_ISO_DEV="/dev/mmcblk0p1"

wipefs -a $BOOT_DEV

# boot partition UEFI
parted -a optimal $BOOT_DEV -- mklabel gpt
parted -a optimal $BOOT_DEV -- mkpart ESP fat32 1MiB 513MiB
parted -a optimal $BOOT_DEV -- set 1 esp on

# root partition
parted -a optimal $ROOT_DEV -- mkpart primary 513MiB 100%
boot="$BOOT_DEV"1
root="$ROOT_DEV"2

mkfs.fat -F 32 -n boot "$boot"
mkfs.ext4 -L root "$root"

# mount root on mnt
mount "$root" /mnt
mkdir -p /mnt/etc/nixos

# mount boot partition
mkdir /mnt/boot
mount "$boot" /mnt/boot

# install all system
cp nixos/flake.nix /mnt/etc/nixos
cp nixos/flake.lock /mnt/etc/nixos
cp -r nixos/raspberry /mnt/etc/nixos
nixos-install --root /mnt --flake /mnt/etc/nixos#$HOSTNAME

mkdir -p /firmware
mount $NIX_ISO_DEV /firmware
cp -r /firmware/* /mnt/boot

umount -R /mnt
