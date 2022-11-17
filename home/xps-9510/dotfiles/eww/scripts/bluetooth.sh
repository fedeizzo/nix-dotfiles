#!/usr/bin/env bash
case $1 in
    "toggle")
        bluetoothctl show | grep 'Powered: yes' &> /dev/null
	if [[ $? == 0 ]]; then
	    bluetoothctl power off
	    $2 update bluetooth-enable=false
	else
	    bluetoothctl power on
	    $2 update bluetooth-enable=true
	fi
	;;
    "status")
        bluetoothctl show | grep 'Powered: yes' &> /dev/null
	if [[ $? == 0 ]]; then
	    echo 'true'
	else
	    echo 'false'
	fi
	;;
esac
