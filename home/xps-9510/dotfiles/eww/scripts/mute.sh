#!/usr/bin/env bash

case $1 in
    "toggle")
	pamixer --get-mute &> /dev/null
	if [[ $? == 0 ]]; then
	    pamixer -u
	    $2 update mute-enable=false
	else
	    pamixer -m
	    $2 update mute-enable=true
	fi
	;;
    "status")
	pamixer --get-mute &> /dev/null
	if [[ $? == 0 ]]; then
	    echo 'true'
	else
	    echo 'false'
	fi
	;;
esac
