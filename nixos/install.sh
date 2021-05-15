#!/bin/sh
helpMessage="usage: ./install.sh <command>\n\n
command:\n
\tupdate: update and install configuration\n
\tinstall: install configuration\n
\thelp: print this message\n

No argument launch install procedure.
"

update() {
    nix flake update
}

install() {
    sudo cp ./flake.nix /etc/nixos
    sudo cp ./flake.lock /etc/nixos
    sudo cp -r system /etc/nixos
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
    *)
        install
        ;;
esac
