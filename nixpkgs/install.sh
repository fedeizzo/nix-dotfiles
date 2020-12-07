#!/usr/bin/env bash

mkdir -p $HOME/.config
ln -s $(pwd) $HOME/.config/nixpkgs
[ -f $HOME/.config/qutebrowser/autoconfig.yml ] || ln -s $(pwd)/dotfiles/dot_config/private_qutebrowser/autoconfig.yml $HOME/.config/qutebrowser/autoconfig.yml
[ -f $HOME/.config/qutebrowser/quickmarks ] || ln -s $(pwd)/dotfiles/dot_config/private_qutebrowser/quickmarks $HOME/.config/qutebrowser/quickmarks
nix build .#linux && ./result/activate

# mkdir - p $HOME/.config
#   ln - s $(pwd) $HOME/.config/nixpkgs
#   ln - s $(pwd)/$1/home.nix $HOME/.config/nixpkgs/home.nix
#   ln - s $(pwd)/$1/config.nix $HOME/.config/nixpkgs/config.nix
#   ln - s $(pwd) /../dotfiles $HOME/.config/dotfiles
