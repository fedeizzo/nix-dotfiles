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
        secret=$(bw get item 'sops-age-keys-homelab' | jq -r ."notes")

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

nix --extra-experimental-features 'nix-command flakes' run 'github:nix-community/disko#disko-install' -- --flake 'github:fedeizzo/nix-dotfiles#homelab' --disk main /dev/nvme0n1 --disk games /dev/nvme1n1 --extra-files /var/lib/sops/keys.txt /var/lib/sops/keys.txt
