#!/usr/bin/env bash
SUPER_CMD="doas"
mkdir -p $HOME/.config
[ -f $HOME/.config/emacs/straight/versions/default.el ] || ln -s $(pwd)/dotfiles/emacs/default.el $HOME/.config/emacs/straight/versions/default.el
$SUPER_CMD ln -s /home/fedeizzo/nix-dotfiles/* /etc/nixos/
$SUPER_CMD nixos-rebuild switch 
