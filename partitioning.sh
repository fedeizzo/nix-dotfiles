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
root="/dev/mapper/nixenc"

# common mount option
mntopt="autodefrag,space_cache=v2,noatime,compress=zstd:2"
mntopt_nocow="autodefrag,space_cache=v2,noatime,nocow"

### check ssh/hdd
# if [ "$(cat "/sys/block/$(echo "$device" | sed 's/\/dev\///')/queue/rotational")" -eq "0" ]; then
#     ssd=true
# else
#     ssd=false
# fi
ssd=true
if $ssd; then
    mntopt="$mntopt,discard=async"
    mntopt_nocow="$mntopt,discard=async"
fi

# mount root on mnt
mount -o "$mntopt" "$root" /mnt
# subvolume array
subvolumes="@ @snapshot @home @opt @root @usr_local @var_log"
subvolumes_nocow="@swap @tmp @var_cache @var_tmp"

# create root, swap and snapshot subvolumes
for sv in $subvolumes; do
    btrfs subvolume create "/mnt/$sv"
done
for sv in $subvolumes_nocow; do
    btrfs subvolume create "/mnt/$sv"
done
sync
umount /mnt

# mount subvolumes
for sv in $subvolumes; do
    dir="/mnt/$(echo "${sv#@}" | sed 's/_/\//g')"
    if [ "$sv" != "@" ]; then
        mkdir -p "$dir"
    fi
    mount -o "$mntopt,subvol=$sv" "$root" "$dir"
done

# mount subvolumes with nocow
for sv in $subvolumes_nocow; do
    dir="/mnt/$(echo "${sv#@}" | sed 's/_/\//g')"
    if [ "$sv" != "@" ]; then
        mkdir -p "$dir"
    fi
    mount -o "$mntopt_nocow,subvol=$sv" "$root" "$dir"
    chattr +C -R "$dir"
done

# mount boot partition
mkdir /mnt/boot
mount "$boot" /mnt/boot

# create swapfile system
truncate -s 0 /mnt/swap/.swapfile
fallocate -l 2G /mnt/swap/.swapfile
chmod 600 /mnt/swap/.swapfile
mkswap /mnt/swap/.swapfile
swapon /mnt/swap/.swapfile

# configure nixos
nixos-generate-config --root /mnt

# fix options not automatically written
for sv in $subvolumes; do
    sed "s/options = \[ \"subvol=@${sv}\" \]/options = [ \"subvol=@${sv}\" \"${mntopt//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done
for sv in $subvolumes_nocow; do
    sed "s/options = \[ \"subvol=${sv}\" \]/options = [ \"subvol=${sv}\" \"${mntopt_nocow//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done

# my personal config
nixos-install

swapoff /mnt/swap/.swapfile
umount -R /mnt
cryptsetup close nixenc
