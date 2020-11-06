#!/bin/sh
set -e

# PARAMETERS 
BOOT_DEV="/dev/sda"
ROOT_DEV="/dev/sda"
USER="fedeizzo"

create_subvolume() {
    for sv in $1; do
        if [[ $sv == "@" ]]; then
            name="/mnt/nix/$sv"
        else
            name="/mnt/nix/persistent/$sv"
        fi
        btrfs subvolume create "$name"
    done
}

mount_subvolume() {
    for sv in $2; do
        if [[ $sv == "@" ]]; then
            dir="/mnt/nix/$(echo "${sv#@}" | sed 's/_/\//g')"
        else
            dir="/mnt/nix/persistent/$(echo "${sv#@}" | sed 's/_/\//g')"
            sv="persistent/$sv"
            mkdir -p "$dir"
        fi
        mount -o "$3,subvol=$sv" "$1" "$dir"
        if [[ $4 = true ]]; then
            chattr +C -R "$dir"
        fi
    done
}

create_persistent_dir() {
    for dir in $dirs_list; do
        dir="/mnt/nix/persistent/$(echo "${sv#@}" | sed 's/_/\//g')"
        mkdir -p "$dir"
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
mkfs.btrfs --csum xxhash -L root /dev/mapper/nixenc
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

# PERSISTENT DIRECTORY
#  --------------------------------------------------------------
# |Folder                             | Volume                   |
# |--------------------------------------------------------------|
# |/nix                               | @                        |
# |/nix/persistent/etc/nixos          |                          |
# |/nix/persistent/home/.cache        |                          |
# |/nix/persistent/home/.local_share  |                          |
# |/nix/persistent/home/.mozilla      |                          |
# |/nix/persistent/home/.ssh          | @home_${USER}_ssh        |
# |/nix/persistent/home/persistent    | @home_${USER}_persistent |
# |/nix/persistent/swap               | @swap                    |
# |/nix/persistent/var/cache          |                          |
# |/nix/persistent/var/lib            |                          |
# |/nix/persistent/var/lib/bluetooth  |                          |
# |/nix/persistent/var/lib/docker     |                          |
# |/nix/persistent/var/lib/machines   |                          |
# |/nix/persistent/var/lib/misc       |                          |
# |/nix/persistent/var/lib/portables  |                          |
# |/nix/persistent/var/lib/postgresql | @var_lib_postgresql      |
# |/nix/persistent/var/lib/systemd    |                          |
# |/nix/persistent/var/log            |                          |
# |/nix/persistent/var/tmp            |                          |
# |/nix/persistent/snapshots          | @snapshots               |
#  --------------------------------------------------------------
persistent_dirs="/etc/nixos /var/log /var/lib/machines /var/lib/portables /var/lib/misc /var/lib/postgresql /var/lib/systemd /var/lib/docker /var/lib/bluetooth /home/fedeizzo/.cache /home/fedeizzo/.local/share /home/fedeizzo/.mozilla /home/fedeizzo/.ssh /home/persistent /var/cache /var/tmp /swap" 
# persistent_files="home/fedeizzo/.zsh_history"

subvolumes="@ @home_${USER}_persistent @var_lib_postgresql @snapshots"
subvolumes_nocow="@swap"

# create subvolumes
create_subvolume "$subvolumes"
create_subvolume "$subvolumes_nocow"
sync
umount -R /mnt/nix

# mount subvolumes
mount_subvolume "$nix" "$subvolumes" "$mntopt" false
mount_subvolume "$nix" "$subvolumes_nocow" "$mntopt_nocow" true

# create persistent dirs
create_persistent_dir "$persistent_dirs"

# create swapfile system
truncate -s 0 /mnt/nix/persistent/swap/.swapfile
fallocate -l 2G /mnt/nix/persistent/swap/.swapfile
chmod 600 /mnt/nix/persistent/swap/.swapfile
mkswap /mnt/nix/persistent/swap/.swapfile
swapon /mnt/nix/persistent/swap/.swapfile

# configure nixos
nixos-generate-config --root /mnt

# fix options not automatically written
for sv in $subvolumes; do
    if [[ $sv != "@" ]]; then
        sv="persistent\/$sv"
    fi
    sed "s/options = \[ \"subvol=${sv}\" \]/options = [ \"subvol=${sv}\" \"${mntopt//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done

for sv in $subvolumes_nocow; do
    if [[ $sv != "@" ]]; then
        sv="persistent\/$sv"
    fi
    sed "s/options = \[ \"subvol=${sv}\" \]/options = [ \"subvol=${sv}\" \"${mntopt_nocow//,/\" \"}\"\]/" -i /mnt/etc/nixos/hardware-configuration.nix
done

sed 's/fsType = "tmpfs";/fsType = "tmpfs";\n    options = [ "defaults" "size=2G" "mode=755" ];/' -i /mnt/etc/nixos/hardware-configuration.nix

# my personal config
./install.sh -f laptop_tmpfs
# nixos-install --no-root-passwd

# swapoff /mnt/nix/persistent/swap/.swapfile
# umount -R /mnt
# cryptsetup close nixenc
