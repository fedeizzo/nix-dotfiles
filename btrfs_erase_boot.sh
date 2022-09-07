#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git nixUnstable
set -e

colorPrint() {
  echo -e "$(tput setaf 6)$1$(tput sgr0)"
}

errorPrint() {
  echo -e "$(tput setaf 1)$1$(tput sgr0)"
}

prompt_confirm() {
  while true; do
    read -r -p "${1:-Continue?} [y/n]: " reply
    case $reply in
    [yY][eE][sS] | [yY])
      return 0
      ;;
    [nN][oO] | [nN])
      return 1
      ;;
    *) printf " \033[31m %s \n\033[0m" "invalid input" ;;
    esac
  done
}


colorPrint "Welcome. this script will let you set up a new machine with NixOS.\n"

if [ $(id -u) -ne 0 ]; then
  errorPrint "Please run as root (you can use 'sudo su' to get a shell)"
  exit 1
fi

if [ ! -d /sys/firmware/efi/ ]; then
  errorPrint "It seems you did not boot via UEFI. Exiting"
  exit 1
fi

lsblk
while
  colorPrint "-- Choose the disk to format"
  read disk
  regex="^${disk}\+\s\+[0-9]\+:[0-9]\+\s\+[0-9]\+\s\+[0-9]\+\([,.][0-9]\+\)\?\w\s\+[0-9]\+\s\+disk\b"
  ! (lsblk | grep -q $regex)
do
  colorPrint "-- Invalid disk ($disk)"
  lsblk
done

colorPrint "You are about to choose disk $disk"
colorPrint
lsblk | grep $regex
colorPrint
colorPrint "The contents of the disk are about to be completely erased. Are you sure you want to proceed?"
if ! prompt_confirm; then
  errorPrint "Operation cancelled, terminating"
  exit
fi
colorPrint "Are you ABSOLUTELY sure this is the right one?"
if ! prompt_confirm; then
  errorPrint "Operation cancelled, terminating"
  exit
fi
sleep 2

devpath="/dev/${disk}"
colorPrint $devpath

colorPrint "Wiping the disk"
wipefs -af $devpath
blkdiscard -f $devpath

# boot partition UEFI
sgdisk -n 0:0:+512MiB -t 0:ef00 -c 0:boot "${devpath}"
# root partition
sgdisk -n 0:0:0 -t 0:8300 -c 0:nixenc "${devpath}"


boot="${devpath}"p1
root="${devpath}"p2
sync

colorPrint "Configuring luks2"
# crypt root partition
cryptsetup --type luks2 luksFormat "$root"
cryptsetup open "$root" nixenc
cryptsetup config "$root" --label nixenc

colorPrint "Formatting"
# formatting filesystems
mkfs.vfat -n boot "$boot"
mkfs.btrfs -L root /dev/mapper/nixenc
root="/dev/mapper/nixenc"

colorPrint "Creating btrfs subvolumes"
mntopt="noautodefrag,space_cache=v2,noatime,ssd,compress=zstd:3,discard"
mount -o "$mntopt" -t btrfs "$root" /mnt
subvolumes="@root @home @nix @persist @log"
for sv in $subvolumes; do
    btrfs subvolume create "/mnt/$sv"
done
sync
btrfs subvolume snapshot -r /mnt/root /mnt/root-blank
umount /mnt

colorPrint "Mounting btrfs subvolumes"
for sv in $subvolumes; do
    dir="/mnt/$(echo "${sv#@}" | sed 's/_/\//g')"
    if [ "$sv" != "@" ]; then
        mkdir -p "$dir"
    fi
    mount -o "$mntopt,subvol=$sv" "$root" "$dir"
done

colorPrint "Creating a blank snapshot of the root for the erase operation"


colorPrint "Mount boot partition"
mkdir /mnt/boot
mount "$boot" /mnt/boot

colorPrint "Installing configuration under system and home folders"
./install.sh -f
nixos-install --no-root-passwd

umount -R /mnt
cryptsetup close nixenc
