#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git nixUnstable
hostname=fedeizzo-nixos
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

if [[ $1 == "--complete" ]]; then
    colorPrint
    colorPrint "Complete new machine installation."
    cp "/etc/adjtime"  "/persist/etc/adjtime"
    cp "/etc/machine-id"  "/persist/etc/machine-id"
    cp "/var/lib/NetworkManager/secret_key"  "/persist/var/lib/NetworkManager/secret_key"
    cp "/var/lib/NetworkManager/seen-bssids"  "/persist/var/lib/NetworkManager/seen-bssids"
    cp "/var/lib/NetworkManager/timestamps"  "/persist/var/lib/NetworkManager/timestamps"
    exit 0
fi

colorPrint
colorPrint "Do you want to setup age folder for sops-nix?"
if prompt_confirm; then
    colorPrint "Choose the device containing the key file for age initialization"
    lsblk
    while
    colorPrint "-- Choose the disk to format"
    read gpgdisk
    regex="^[^a-zA-Z]*${gpgdisk}\+\s\+[0-9]\+:[0-9]\+\s\+[0-9]\+\s\+[0-9]\+\([,.][0-9]\+\)\?\w\s\+[0-9]\+\s\+part\b"
    ! (lsblk | grep -q $regex)
    do
    colorPrint "-- Invalid disk ($gpgdisk)"
    lsblk
    done

    colorPrint "You are about to choose disk $gpgdisk"
    colorPrint
    mkdir -p /mnttmp
    mount "/dev/$gpgdisk" /mnttmp

    colorPrint "Choose the file containing the age key."
    find /mnttmp -name '*.txt'
    while
    colorPrint "-- Choose the file"
    read agefile
    regex="^${agefile}$"
    ! (find /mnttmp -name '*.txt' | grep -q $regex)
    do
    colorPrint "-- Invalid file ($agefile)"
    lsblk
    done

    colorPrint "You are about to choose file $agefile"
    SOPSHOME="/var/lib/sops"
    mkdir -p $SOPSHOME
    cp $agefile $SOPSHOME
    chmod -R 600 $SOPSHOME
    umount -R /mnttmp
    rmdir /mnttmp
fi

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

colorPrint "Checking if disk $disk is an ssd"
is_hdd=$(cat /sys/block/$disk/queue/rotational)

devpath="/dev/${disk}"
colorPrint $devpath

colorPrint "Wiping the disk"
wipefs -af $devpath
blkdiscard -f $devpath

# boot partition UEFI
sgdisk -n 0:0:+512MiB -t 0:ef00 -c 0:boot "${devpath}"
# root partition
sgdisk -n 0:0:0 -t 0:8300 -c 0:nixenc "${devpath}"


if [[ $is_hdd == 0 ]]; then
    boot="${devpath}"p1
    root="${devpath}"p2
else
    boot="${devpath}"1
    root="${devpath}"2
fi
sync

colorPrint "Configuring luks2"
# crypt root partition
cryptsetup --type luks2 luksFormat "$root"
cryptsetup open "$root" nixenc
colorPrint "Luks2 correctly configured"
cryptsetup config "$root" --label nixenc

colorPrint "Formatting"
# formatting filesystems
mkfs.vfat -n boot "$boot"
mkfs.btrfs -L root /dev/mapper/nixenc
root="/dev/mapper/nixenc"

colorPrint "Creating btrfs subvolumes"
mntopt="noautodefrag,space_cache=v2,noatime,ssd,compress=zstd:3,discard"
mount -t btrfs "$root" /mnt
btrfs subvolume create "/mnt/root"
# btrfs subvolume create "/mnt/home"
btrfs subvolume create "/mnt/nix"
btrfs subvolume create "/mnt/persist"
btrfs subvolume create "/mnt/log"
btrfs subvolume create "/mnt/sops"
sync
colorPrint "Creating a blank snapshot of the root for the erase operation"
btrfs subvolume snapshot -r /mnt/root /mnt/root-blank
umount /mnt

colorPrint "Mounting btrfs subvolumes"
mount -o "$mntopt,subvol=root" "$root" /mnt
# mkdir /mnt/home
# mount -o "$mntopt,subvol=home" "$root" /mnt/home
mkdir /mnt/nix
mount -o "$mntopt,subvol=nix" "$root" /mnt/nix
mkdir /mnt/persist
mount -o "$mntopt,subvol=persist" "$root" /mnt/persist
mkdir -p /mnt/var/log
mount -o "$mntopt,subvol=log" "$root" /mnt/var/log
mkdir -p /mnt/var/lib/sops
mount -o "$mntopt,subvol=sops" "$root" /mnt/var/lib/sops

colorPrint "Mount boot partition"
mkdir /mnt/boot
mount "$boot" /mnt/boot

colorPrint "Installing configuration under system folder"
cp /var/lib/sops/keys.txt /mnt/var/lib/sops/
chmod 600 -R /mnt/var/lib/sops
mkdir -p /mnt/etc/nixos
./install.sh -f
nixos-install --no-root-passwd --flake /mnt/etc/nixos#$hostname
installationReturnValue=$?

if [ $installationReturnValue -ne 0 ]; then
  echo "----------Unsuccessful installation----------"
  echo "Exiting"
  exit 1
fi

colorPrint "System installed. Do you want to umount the drive and exit?"
if ! prompt_confirm; then
  colorPrint "Remember to run umount -R /mnt and cryptsetup close nixenc before rebooting"
  exit 0
fi
umount -R /mnt/*
umount -R /mnt
cryptsetup close nixenc
