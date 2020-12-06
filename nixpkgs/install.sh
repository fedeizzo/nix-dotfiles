#!/usr/bin/env bash

mkdir -p $HOME/.config
ln -s $(pwd) $HOME/.config/nixpkgs

# mkdir - p $HOME/.config
#   ln - s $(pwd) $HOME/.config/nixpkgs
#   ln - s $(pwd)/$1/home.nix $HOME/.config/nixpkgs/home.nix
#   ln - s $(pwd)/$1/config.nix $HOME/.config/nixpkgs/config.nix
#   ln - s $(pwd) /../dotfiles $HOME/.config/dotfiles
