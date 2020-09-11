#!/bin/sh
helpMessage="usage: ./install.sh <configuration>"

copy_laptop_conf() {
    dir=$(pwd)
    ln -fs "${dir}/laptop/configuration.nix" /etc/nixos/configuration.nix
    ln -fs "${dir}/laptop/environment.nix" /etc/nixos/environment.nix
    ln -fs "${dir}/laptop/networking.nix" /etc/nixos/networking.nix
    ln -fs "${dir}/laptop/programs.nix" /etc/nixos/programs.nix
    ln -fs "${dir}/laptop/security.nix" /etc/nixos/security.nix
    ln -fs "${dir}/laptop/services.nix" /etc/nixos/services.nix
    nix-channel --add https://github.com/rycee/home-manager/archive/release-20.03.tar.gz home-manager
    nix-channel --update
    nix-shell '<home-manager>' -A install
}

case $1 in
    "laptop")
        copy_laptop_conf
        ;;
    *)
        echo $helpMessage
        ;;
esac
