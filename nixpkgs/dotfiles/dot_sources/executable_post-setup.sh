#!/usr/bin/env bash

print_ok() {
    printf "\e[32m%b\e[0m" "$1""\n"
}

print_info() {
    printf "\e[36m%b\e[0m" "$1""\n"
}
# Exit if some error occurs
set -e 

pacman -S zsh

# User and home creation. Default shell change to zsh
print_info "Insert new user: "
read -r username
useradd -s /usr/bin/zsh -m $username
passwd $username
usermod -aG wheel fedeizzo
print_ok "User and home created. Default shell set to zsh"

# Wheel group modification
print_info "Changing wheel permission"
sed -i 's/^\#%whell ALL=(ALL) ALL/%whell ALL=(ALL) ALL/' /etc/sudoers
print_ok "done"

# Set /etc/localtime
ln -sf /usr/share/zoneinfo/Europe/Rome /etc/locatime
print_ok "localtime linked"

# Changing user
su - $username

# Installing yay
print_info "Installing yay"
cd /tmp
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
print_ok "done"

# Installing all packages
cd /tmp
git clone https://github.com/fedeizzo/dotfiles.git
yay -S $(cat dotfiles/dot_config/pacman-packages/pkglist)
# TODO think to add informant package
# .zshrc must be created in order to install nvm

# enable services
print_info "Enabling some services"
sudo systemctl enable betterlockscreen.service
sudo systemctl enable bluetooth.service
sudo systemctl enable bumblebeed.service
sudo systemctl enable connman.service
sudo systemctl enable cronie.service
sudo systemctl enable docker.service
sudo systemctl enable getty@.service
sudo systemctl enable lm_sensors.service
sudo systemctl enable thermald.service
sudo systemctl enable tlp.service
sudo systemctl enable fstrim.timer
print_ok "done"

# Set dotfiles
print_info "Setting dotfiles"
chezmoi init https://github.com/fedeizzo/dotfiles.git
mkdir ~/.config/chezmoi
cp ~/.local/share/chezmoi/dot_config/chezmoi/private_chezmoi.toml
chezmoi apply
source ~/.zshrc
source ~/.zshenv
print_ok "done"

print_info "Installing nim and nimble"
choosenim stable
source ~/.zshenv
print_ok "done"

print_info "Cloning and installing rofi_spotlight and lemonblocks"
# Installing nofi_spotlight
cd /tmp
git clone https://github.com/fedeizzo/rofi_spotlight.git
cd rofi_spotlight
nimble install

# Installing lemonblocks
cd /tmp
git clone https://github.com/fedeizzo/lemonblocks.git
cd lemonblocks
nimble install
print_ok "done"

# Clone my personal home page for qutebrowser
print_info "Cloning personal page for qutebrowser"
cd /tmp
git clone https://github.com/fedeizzo/fedeizzo.github.io.git
print_ok "done"

# Install Oh my zsh and relative extension
print_info "Installing Oh my zsh and some plugins"
cd ~
export RUNZSH=no
export KEEP_ZSHRC=yes
wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh
sh install.sh
source .zshrc
source .zshenv
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
 git clone https://github.com/zsh-users/zsh-history-substring-search ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-history-substring-search
 git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
print_ok "done"

# Install vim-plug
print_info "Installing vim-plug"
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
print_ok "done"

# Login in bitwarden cli
# print_info "Login into bitwarden"
# bw login

# TODO file da spostare
# DONE nixos
# main.conf /etc/connman
# pacman.conf /etc
# tlp.conf /etc
# DONE nixos
# default.pa /etc/pulse
# bluetooth folder /var/lib
# connman folder /var/lib
