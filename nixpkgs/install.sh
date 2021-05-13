#!/usr/bin/env bash

mkdir -p $HOME/.config
[ -f $HOME/.config/qutebrowser/autoconfig.yml ] || ln -s $(pwd)/dotfiles/qutebrowser/autoconfig.yml $HOME/.config/qutebrowser/autoconfig.yml
[ -f $HOME/.config/qutebrowser/quickmarks ] || ln -s $(pwd)/dotfiles/qutebrowser/quickmarks $HOME/.config/qutebrowser/quickmarks
nix build .#linux && ./result/activate
