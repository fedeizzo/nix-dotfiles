#!/usr/bin/env bash
set -e

nix-shell shell.nix --run "home-manager switch"
ln -s $(pwd)/../dotfiles/dot_config/private_qutebrowser/autoconfig.yml $HOME/.config/qutebrowser/autoconfig.yml
ln -s $(pwd)/../dotfiles/dot_config/private_qutebrowser/quickmarks $HOME/.config/qutebrowser/quickmarks
