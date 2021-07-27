#!/bin/sh
set -e
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"

# boot partition UEFI
sgdisk -n 0:0:+260MiB -t 0:ef00 -c 0:boot "$BOOT_DEV"
# root partition
sgdisk -n 0:0:0 -t 0:8300 -c 0:nixenc "$ROOT_DEV"

if [[ $BOOT_DEV == $ROOT_DEV ]]; then
    boot="$BOOT_DEV"1
    root="$BOOT_DEV"2
else
    boot="$BOOT_DEV"1
    root="$ROOT_DEV"2
fi

# force re-reading the partition table
sync
partprobe "$BOOT_DEV"
partprobe "$ROOT_DEV"

# print results
sgdisk -p "$BOOT_DEV"
sgdisk -p "$ROOT_DEV"

# crypt root partition
cryptsetup --type luks2 luksFormat "$root"
cryptsetup open "$root" nixenc

# formatting filesystems
mkfs.vfat -n boot "$boot"
mkfs.ext4 -L root /dev/mapper/nixenc
root="/dev/mapper/nixenc"

# mount root on mnt
mount "$root" /mnt

# mount boot partition
mkdir /mnt/boot
mount "$boot" /mnt/boot

# configure nixos
nixos-generate-config --root /mnt

# my personal config
./install.sh fresh
nixos-install

umount -R /mnt
cryptsetup close nixenc