#!/usr/bin/env bash
SUPER_CMD="doas"
if [[ $1 == "-f" ]]; then
    SUPER_CMD=""
else
    mkdir -p $HOME/.config
    [ -f $HOME/.config/emacs/straight/versions/default.el ] || ln -s $(pwd)/dotfiles/emacs/default.el $HOME/.config/emacs/straight/versions/default.el
fi
current_dir=$(pwd)
for f in $(ls); do
    if ! [[ -f /etc/nixos/${f} ]]; then
	if ! [[ -d /etc/nixos/${f} ]]; then
	    if [[ $1 == "-f" ]]; then
		$SUPER_CMD cp -r ${current_dir}/${f} /mnt/etc/nixos/
	    else
		$SUPER_CMD ln -s ${current_dir}/${f} /etc/nixos/
	    fi
	fi
    fi
done
[[ $1 != "-f" ]] && $SUPER_CMD nixos-rebuild switch 
