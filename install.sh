#!/bin/sh
helpMessage="usage: ./install.sh -f <configuration>\n
options:\n
\t-f: fresh install that copy in /mnt/etc instead /etc\n\n
configuration:\n
\tlaptop: laptop configuration\n
"

copy_laptop_conf() {
    dir=$(pwd)

    cp "${dir}/nixos/laptop/configuration.nix" /etc/nixos/configuration.nix
    cp -r "${dir}/nixos/laptop/pkgs" /etc/nixos
}

fresh_install() {
    dir=$(pwd)
    cp "${dir}/nixos/laptop/configuration.nix" /mnt/etc/nixos/configuration.nix
    cp -r "${dir}/nixos/laptop/pkgs" /mnt/etc/nixos
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
        copy_laptop_conf
        ;;
    "fresh")
        fresh_install
        ;;
    *)
        echo -e $helpMessage
        ;;
esac
