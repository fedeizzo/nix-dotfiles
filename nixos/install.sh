#!/bin/sh
helpMessage="usage: ./install.sh <command>\n\n
commands:\n
\tupdate: update and install configuration\n
\tinstall: install configuration\n
\tnew: install configuration in /mnt (fresh install)\n
\thelp: print this message\n

No argument launch install procedure.
"

update() {
    nix flake update
}

install() {
    local dst="/etc/nixos"
    if [[ $1 == "new" ]]; then
        dst=/mnt/etc/nixos
    fi
    sudo cp ./flake.nix $dst
    sudo cp ./flake.lock $dst
    sudo cp -r system $dst
    sudo nixos-rebuild switch
}

case $1 in
    "update")
        update
        install
        ;;
    "install")
        install
        ;;
    "help")
        echo -e $helpMessage
        ;;
    "new")
        install "new"
        ;;
    *)
        install
        ;;
esac
