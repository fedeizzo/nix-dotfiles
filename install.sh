#!/bin/sh
helpMessage="usage: ./install.sh <configuration>"

copy_laptop_conf() {
    dir=$(pwd)
    # ln -fs "${dir}/laptop/configuration.nix" /etc/nixos/configuration.nix
    # ln -fs "${dir}/laptop/environment.nix" /etc/nixos/environment.nix
    # ln -fs "${dir}/laptop/networking.nix" /etc/nixos/networking.nix
    # ln -fs "${dir}/laptop/programs.nix" /etc/nixos/programs.nix
    # ln -fs "${dir}/laptop/security.nix" /etc/nixos/security.nix
    # ln -fs "${dir}/laptop/services.nix" /etc/nixos/services.nix

    cp "${dir}/nixos/laptop/configuration.nix" /etc/nixos/configuration.nix
    cp "${dir}/nixos/laptop/environment.nix" /etc/nixos/environment.nix
    cp "${dir}/nixos/laptop/hardware.nix" /etc/nixos/hardware.nix
    cp "${dir}/nixos/laptop/networking.nix" /etc/nixos/networking.nix
    cp "${dir}/nixos/laptop/programs.nix" /etc/nixos/programs.nix
    cp "${dir}/nixos/laptop/security.nix" /etc/nixos/security.nix
    cp "${dir}/nixos/laptop/services.nix" /etc/nixos/services.nix
    cp -r "${dir}/nixos/laptop/pkgs" /etc/nixos/pkgs
    # nix-channel --add https://github.com/rycee/home-manager/archive/release-20.03.tar.gz home-manager
    # nix-channel --update
    # nix-shell '<home-manager>' -A install
}

fresh_install() {
    dir=$(pwd)
    cp "${dir}/nixos/laptop/configuration.nix" /mnt/etc/nixos/configuration.nix
    cp "${dir}/nixos/laptop/environment.nix" /mnt/etc/nixos/environment.nix
    cp "${dir}/nixos/laptop/hardware.nix" /mnt/etc/nixos/hardware.nix
    cp "${dir}/nixos/laptop/networking.nix" /mnt/etc/nixos/networking.nix
    cp "${dir}/nixos/laptop/programs.nix" /mnt/etc/nixos/programs.nix
    cp "${dir}/nixos/laptop/security.nix" /mnt/etc/nixos/security.nix
    cp "${dir}/nixos/laptop/services.nix" /mnt/etc/nixos/services.nix
    cp -r "${dir}/nixos/laptop/pkgs" /mnt/etc/nixos/pkgs
}

case $1 in
    "laptop")
        copy_laptop_conf
        ;;
    "fresh")
        fresh_install
        ;;
    *)
        echo $helpMessage
        ;;
esac
