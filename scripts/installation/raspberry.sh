#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git bitwarden-cli jq

if [ $(id -u) -ne 0 ]; then
  echo "Please run as root (you can use 'sudo su' to get a shell)"
  exit 1
fi

# bw login --check
# if [ $? -eq 1 ]; then
#     bw login
# fi
# if [ ! -f /var/lib/sops/keys.txt ]; then
#     mkdir -p /var/lib/sops
#     while true; do
#         secret=$(bw get item 'sops-age-keys-homelab' | jq -r ."notes")

#         if [ $? -eq 0 ]; then
#             echo $secret > /var/lib/sops/keys.txt
#             chmod 600 /var/lib/sops/keys.txt
#             break
#         else
#             errorPrint "Command failed, retrying ..."
#             sleep 1
#         fi
#     done
# fi

nix --extra-experimental-features 'nix-command flakes' run 'github:nix-community/disko#disko-install' -- --flake 'github:fedeizzo/nix-dotfiles#freezer' --disk main /dev/sdb

# --extra-files /var/lib/sops/keys.txt /var/lib/sops/keys.txt
