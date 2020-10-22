#!/bin/sh
helpMessage="usage: ./install.sh -f <configuration>\n
options:\n
\t-f: fresh install that copy in /mnt/etc instead /etc\n\n
configuration:\n
\tlaptop: laptop configuration\n
\tlaptop_tmpfs: laptop configuration with tmpfs\n
"

copy_conf() {
    dir=$(pwd)

    cp "${dir}/nixos/common.nix" /etc/nixos/common.nix
    cp "${dir}/nixos/$1/configuration.nix" /etc/nixos/configuration.nix
    cp -r "${dir}/nixos/pkgs" /etc/nixos
}

# TODO fix destination path to /mnt/etc
fresh_install() {
    dir=$(pwd)

    cp "${dir}/nixos/common.nix" /mnt/etc/nixos/common.nix
    cp "${dir}/nixos/$1/configuration.nix" /mnt/etc/nixos/configuration.nix
    cp -r "${dir}/nixos/pkgs" /mnt/etc/nixos
}

fresh=false
if [[ $1 == "-f" ]]; then
    fresh="true"
    shift
else
    fresh="false"
fi

case $1 in
    "laptop")
        echo "Copying laptop configuration"
        copy_conf "laptop"
        ;;
    "laptop_tmpfs")
        echo "Copying laptop configuration with tmpfs"
        copy_conf "laptop_tmpfs"
        ;;
    "fresh")
        fresh_install $1
        ;;
    *)
        echo -e $helpMessage
        ;;
esac
