#!/bin/sh
# helpMessage="usage: ./install.sh -f <configuration>\n
# options:\n
# \t-f: fresh install that copy in /mnt/etc instead /etc\n\n
# configuration:\n
# \tlaptop: laptop configuration\n
# \tlaptop_tmpfs: laptop configuration with tmpfs\n
# "

# copy_conf() {
#     dir=$(pwd)

#     cp "${dir}/nixos/common.nix" /etc/nixos/common.nix
#     cp "${dir}/nixos/$1/configuration.nix" /etc/nixos/configuration.nix
#     cp -r "${dir}/nixos/pkgs" /etc/nixos
# }

# fresh_install() {
#     dir=$(pwd)

#     cp "${dir}/nixos/common.nix" /mnt/etc/nixos/common.nix
#     cp "${dir}/nixos/$1/configuration.nix" /mnt/etc/nixos/configuration.nix
#     cp -r "${dir}/nixos/pkgs" /mnt/etc/nixos
# }

# if [[ $1 == "-f" ]]; then
#     shift
#     fresh_install $1
# else
#     if [[ "$1" =~ ^[laptop|laptop_tmps]+$ ]]; then
#         echo "Copying $1 configuration"
#         copy_conf $1
#     else
#         echo -e $helpMessage
#     fi
# fi
cp ./flake.nix /etc/nixos
cp ./flake.lock /etc/nixos
cp -r system /etc/nixos
