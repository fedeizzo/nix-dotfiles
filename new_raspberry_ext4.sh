#!/bin/sh
set -e
HOSTNAME="rasp-nixos"
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"

block_before_install=$1

# boot partition UEFI
sgdisk -n 0:0:+512MiB -t 0:ef00 -c 0:boot "$BOOT_DEV"
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

# formatting filesystems
mkfs.vfat -n boot "$boot"
mkfs.ext4 -L root "$root"

# mount root on mnt
mount "$root" /mnt
mkdir -p /mnt/etc/nixos

# mount boot partition
mkdir /mnt/boot
mount "$boot" /mnt/boot

if [ -z $block_before_install ]; then
    # set iso root password and change user
    passwd root

    # setup UEFI
    su -c 'nix-env -iA nixos.wget nixos.unzip'
    su -c 'cd /mnt/boot && wget https://github.com/pftf/RPi4/releases/download/v1.33/RPi4_UEFI_Firmware_v1.33.zip && unzip RPi4_UEFI_Firmware_v1.33.zip'
    su -c 'rm /mnt/boot/README.md'
    su -c 'rm RPi4_UEFI_Firmware_v1.33.zip'

    # setup nix flake
    su -c 'nix-env -iA nixos.nixUnstable'
    su -c 'nix-env -iA nixos.git'
    su -c 'mkdir -p ~/.config/nix'
    su -c 'echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf'

    # install all system
    su -c "cp nixos/flake.nix /mnt/etc/nixos"
    su -c "cp nixos/flake.lock /mnt/etc/nixos"
    su -c "cp -r nixos/raspberry /mnt/etc/nixos"
    su -c "nixos-install --flake /mnt/etc/nixos#$HOSTNAME"

    umount -R /mnt
elif [ $block_before_install == "-b" ]; then
    nixos-generate-config --root /mnt
else
    echo $block_before_install" not implemented"
fi
