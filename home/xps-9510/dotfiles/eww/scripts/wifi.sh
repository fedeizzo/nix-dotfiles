#!/usr/bin/env bash
case $1 in
    "toggle")
	if [[ $(nmcli radio wifi) == 'enabled' ]]; then
	    nmcli radio wifi off
	    $2 update wifi-enable=false
	else
	    nmcli radio wifi on
	    $2 update wifi-enable=true
	fi
	;;
    "status")
	if [[ $(nmcli radio wifi) == 'enabled' ]]; then
	    echo 'true'
	else
	    echo 'false'
	fi
	;;
esac
