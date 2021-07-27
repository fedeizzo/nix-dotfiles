#!/bin/sh
set -e

# PARAMETERS 
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"
USER="fedeizzo"

create_persistent_dir() {
    dirs_list=$1
    for dir in $dirs_list; do
        dir_path="/mnt/nix/persistent/$(echo "${dir#@}" | sed 's/_/\//g')"
        mkdir -p "$dir_path"
    done
}

bind_mount_persistent_dir() {
    dirs_list=$1
    for dir in $dirs_list; do
        dir_path="/mnt/nix/persistent/$(echo "${dir#@}" | sed 's/_/\//g')"
        mount_point="/mnt/$(echo "${dir#@}" | sed 's/_/\//g')"
        mount -o bind "$dir_path" "$mount_point"
    done
}

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
nix="/dev/mapper/nixenc"

# mount tmpfs
mount -t tmpfs none /mnt

# create directories
mkdir -p /mnt/{boot,nix,etc/nixos,var/log,swap}

# mount /boot and /nix
mount "$boot" /mnt/boot
mount "$nix" /mnt/nix

mkdir -p /mnt/nix/persistent

# PERSISTENT DIRECTORY
#  ------------------------------------
# |Folder                             |
# |------------------------------------
# |/nix                               |
# |/nix/persistent/etc/nixos          |
# |/nix/persistent/home               |
# |/nix/persistent/swap               |
# |/nix/persistent/var/cache          |
# |/nix/persistent/var/lib            |
# |/nix/persistent/var/log            |
# |/nix/persistent/var/tmp            |
#  ------------------------------------
persistent_dirs="/etc/nixos /home /swap /var/cache /var/lib /var/log /var/tmp" 

# create persistent dirs
create_persistent_dir "$persistent_dirs"
bind_mount_persistent_dir "$persistent_dirs"

# create swapfile system
truncate -s 0 /mnt/nix/persistent/swap/.swapfile
fallocate -l 2G /mnt/nix/persistent/swap/.swapfile
chmod 600 /mnt/nix/persistent/swap/.swapfile
mkswap /mnt/nix/persistent/swap/.swapfile
swapon /mnt/nix/persistent/swap/.swapfile

# configure nixos
nixos-generate-config --root /mnt

# my personal config
# ./install.sh -f laptop_tmpfs
# nixos-install --no-root-passwd
nixos-install --root /mnt --flake <myflakepath>

# swapoff /mnt/nix/persistent/swap/.swapfile
# umount -R /mnt
# cryptsetup close nixenc
