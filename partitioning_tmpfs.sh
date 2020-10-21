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
mkfs.btrfs -L root /dev/mapper/nixenc
nix="/dev/mapper/nixenc"

# mount tmpfs
mount -t tmpfs none /mnt

# create directories
mkdir -p /mnt/{boot,nix,etc/nixos,var/log,swap}

# mount /boot and /nix
mount "$boot" /mnt/boot
mount "$nix" /mnt/nix

# create and mount persistente directories
# persistent_dirs="etc/nixos var/log var/cache var/tmp var/lib/machines var/lib/portables var/lib/misc var/lib/postgresql var/lib/systemd var/lib/docker var/lib/bluetooth swap home/fedeizzo/.cache home/fedeizzo/.local/share home/fedeizzo/.mozilla home/fedeizzo/.ssh  home/persistent" 
persistent_dirs="etc/nixos var/log var/cache var/tmp var/lib/machines var/lib/portables var/lib/misc var/lib/postgresql var/lib/systemd var/lib/docker var/lib/bluetooth swap" 
persistend_files="home/fedeizzo/.zsh_history"
for d in $persistent_dirs; do
    mkdir -p /mnt/nix/persistent/$dir
done

# for d in $persistent_dirs; do
    # mount -o bind /mnt/nix/persistent/$dir /mnt/$dir
# done

# create swapfile system
truncate -s 0 /mnt/nix/persistent/swap/.swapfile
fallocate -l 2G /mnt/nix/persistent/swap/.swapfile
chmod 600 /mnt/nix/persistent/swap/.swapfile
mkswap /mnt/nix/persistent/swap/.swapfile
swapon /mnt/nix/persistent/swap/.swapfile

# configure nixos
nixos-generate-config --root /mnt

# my personal config
./install.sh -f laptop_tmpfs
nixos-install

swapoff /mnt/swap/.swapfile
umount -R /mnt
cryptsetup close nixenc
