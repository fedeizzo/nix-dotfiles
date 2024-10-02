#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git
set -e
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
colorPrint "Do you want to setup another device for the nix store during the installation?"
if prompt_confirm; then
    lsblk
    while
    colorPrint "-- Choose the disk"
    read gpgdisk
    regex="^[^a-zA-Z]*${gpgdisk}\+\s\+[0-9]\+:[0-9]\+\s\+[0-9]\+\s\+[0-9]\+\([,.][0-9]\+\)\?\w\s\+[0-9]\+\s\+part\b"
    ! (lsblk | grep -q $regex)
    do
    colorPrint "-- Invalid disk ($gpgdisk)"
    lsblk
    done

    colorPrint "You are about to choose disk $gpgdisk"
    colorPrint
    mkdir -p /mntcache
    mount "/dev/$gpgdisk" /mntcache
    mkdir -p /mntcache/flake/cache
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

if [ $(id -u) -ne 0 ]; then
  errorPrint "Please run as root (you can use 'sudo su' to get a shell)"
  exit 1
fi

if [ -d /mntcache/flake/cache ]; then
  TMPDIR=/mntcache/flake/cache nix --extra-experimental-features 'nix-command flakes' run 'github:nix-community/disko#disko-install' -- --flake .#oven --disk main /dev/nvme0n1
else
  nix --extra-experimental-features 'nix-command flakes' run 'github:nix-community/disko#disko-install' -- --flake .#oven --disk main /dev/nvme0n1
fi
