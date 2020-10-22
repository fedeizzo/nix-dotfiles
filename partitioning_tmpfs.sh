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

mkdir -p /mnt/nix/persistent

# create and mount persistente directories
mntopt="autodefrag,space_cache=v2,noatime,compress=zstd:2"
mntopt_nocow="autodefrag,space_cache=v2,noatime,nocow"

persistent_dirs="@etc_nixos @var_log @var_lib_machines @var_lib_portables @var_lib_misc @var_lib_postgresql @var_lib_systemd @var_lib_docker @var_lib_bluetooth @home_fedeizzo_.cache @home_fedeizzo_.local_share @home_fedeizzo_.mozilla @home_fedeizzo_.ssh @home_persistent" 
persistent_dirs_nocow="@var_cache @var_tmp @swap" 
# persistent_files="home/fedeizzo/.zsh_history"

# mount subvolumes
for sv in $persistent_dirs; do
    dir="/mnt/nix/persistent/$(echo "${sv#@}" | sed 's/_/\//g')"
    if [ "$sv" != "@" ]; then
        mkdir -p "$dir"
    fi
    mount -o "$mntopt,subvol=$sv" "$root" "$dir"
done

# mount subvolumes with nocow
for sv in $persistent_dirs_nocow; do
    dir="/mnt/nix/persistent/$(echo "${sv#@}" | sed 's/_/\//g')"
    if [ "$sv" != "@" ]; then
        mkdir -p "$dir"
    fi
    mount -o "$mntopt_nocow,subvol=$sv" "$root" "$dir"
    chattr +C -R "$dir"
done

# create swapfile system
truncate -s 0 /mnt/nix/persistent/swap/.swapfile
fallocate -l 2G /mnt/nix/persistent/swap/.swapfile
chmod 600 /mnt/nix/persistent/swap/.swapfile
mkswap /mnt/nix/persistent/swap/.swapfile
swapon /mnt/nix/persistent/swap/.swapfile

# configure nixos
nixos-generate-config --root /mnt


# fix options not automatically written
for sv in $persistent_dirs; do
sed "s/options = \[ \"subvol=${sv}\" \]/options = [ \"subvol=${sv}\" \"${mntopt//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done
for sv in $persistent_dirs_nocow; do
sed "s/options = \[ \"subvol=${sv}\" \]/options = [ \"subvol=${sv}\" \"${mntopt_nocow//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done

# my personal config
./install.sh -f laptop_tmpfs
nixos-install

swapoff /mnt/nix/persistent/swap/.swapfile
umount -R /mnt
cryptsetup close nixenc
