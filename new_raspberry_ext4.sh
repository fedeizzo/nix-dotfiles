#!/bin/sh
set -e
HOSTNAME="rasp-nixos"
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"

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

block_before_install=$1
if [ -z $block_before_install ]; then
    # setup UEFI
    cwd=$(pwd)
    nix-env -iA nixos.wget nixos.unzip
    cd /mnt/boot && wget 'https://github.com/pftf/RPi4/releases/download/v1.33/RPi4_UEFI_Firmware_v1.33.zip' && unzip RPi4_UEFI_Firmware_v1.33.zip
    rm /mnt/boot/README.md
    rm RPi4_UEFI_Firmware_v1.33.zip

    cd $cwd
    # setup nix flake
    nix-env -iA nixos.nixUnstable
    nix-env -iA nixos.git
    mkdir -p ~/.config/nix
    echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf

    # install all system
    cp nixos/flake.nix /mnt/etc/nixos
    cp nixos/flake.lock /mnt/etc/nixos
    cp -r nixos/raspberry /mnt/etc/nixos
    nixos-install --flake /mnt/etc/nixos#$HOSTNAME

    umount -R /mnt
elif [ $block_before_install == "-b" ]; then
    nixos-generate-config --root /mnt
else
    echo $block_before_install" not implemented"
fi
