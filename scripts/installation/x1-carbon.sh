#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git bitwarden-cli jq
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

colorPrint
colorPrint "Do you want to setup another device as swap for the nix store during the installation?"
if prompt_confirm; then
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

    wipefs -af "/dev/$disk"
    sgdisk -n 0:0:+50GiB -t 0:8200 -c 0:swap /dev/$disk
    mkswap /dev/sdb1
    swapon /dev/sdb1
    mount -o remount,size=50G,noatime /nix/.rw-store
fi

if [ $(id -u) -ne 0 ]; then
  errorPrint "Please run as root (you can use 'sudo su' to get a shell)"
  exit 1
fi

bw login --check
if [ $? -eq 1 ]; then
    bw login
fi
if [ ! -f /var/lib/sops/keys.txt ]; then
    mkdir -p /var/lib/sops
    while true; do
        secret=$(bw get item 'sops-age-keys-x1-carbon' | jq -r ."notes")

        if [ $? -eq 0 ]; then
            echo $secret > /var/lib/sops/keys.txt
            chmod 600 /var/lib/sops/keys.txt
            break
        else
            errorPrint "Command failed, retrying ..."
            sleep 1
        fi
    done
fi

nix --extra-experimental-features 'nix-command flakes' run 'github:nix-community/disko#disko-install' -- --flake 'github:fedeizzo/nix-dotfiles#oven' --disk main /dev/nvme0n1 --extra-files /var/lib/sops/keys.txt /var/lib/sops/keys.txt
