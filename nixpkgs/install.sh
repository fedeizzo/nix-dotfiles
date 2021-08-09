#!/usr/bin/env bash

mkdir -p $HOME/.config
[ -f $HOME/.config/qutebrowser/autoconfig.yml ] || ln -s $(pwd)/dotfiles/qutebrowser/autoconfig.yml $HOME/.config/qutebrowser/autoconfig.yml
[ -f $HOME/.config/qutebrowser/quickmarks ] || ln -s $(pwd)/dotfiles/qutebrowser/quickmarks $HOME/.config/qutebrowser/quickmarks
[ -f $HOME/.config/nvim ] || ln -s $(pwd)/dotfiles/nvim/spell $HOME/.config/nvim
nix build .#linux && ./result/activate
