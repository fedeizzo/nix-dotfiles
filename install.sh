#!/usr/bin/env bash
SUPER_CMD="doas"
if [[ $1 == "-f" ]]; then
    SUPER_CMD=""
else
    if [[ $HOSTNAME == "fedeizzo-nixos" ]]; then
	mkdir -p $HOME/.config/emacs/straight/version
	[ -f $HOME/.config/emacs/straight/versions/default.el ] || ln -s $(pwd)/home/modules/emacs/default.el $HOME/.config/emacs/straight/versions/default.el
    fi
fi
current_dir=$(pwd)
$SUPER_CMD find /etc/nixos -xtype l -exec rm {} \;
for f in $(ls); do
    if ! [[ -f /etc/nixos/${f} ]]; then
	if ! [[ -d /etc/nixos/${f} ]]; then
	    ecoh /etc/nixos/${f}
	    if [[ $1 == "-f" ]]; then
		$SUPER_CMD cp -r ${current_dir}/${f} /mnt/etc/nixos/
	    else
		$SUPER_CMD ln -s ${current_dir}/${f} /etc/nixos/
	    fi
	fi
    fi
done
[[ $1 != "-f" ]] && $SUPER_CMD nixos-rebuild switch 
