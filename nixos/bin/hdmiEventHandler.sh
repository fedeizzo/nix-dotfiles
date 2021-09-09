#!/usr/bin/env bash

export XAUTHORITY=/home/fedeizzo/.Xauthority
export DISPLAY=:0

STATUS=$(cat /sys/class/drm/card0-DP-3/status)
XRANDR_PATH=$1

if [[ $STATUS == "connected" ]]; then
    sleep 5
    $XRANDR_PATH/bin/xrandr --addmode DP-3 1920x1080 &> /idk2.log
    $XRANDR_PATH/bin/xrandr --output eDP-1 --crtc 1 --preferred --output DP-3 --primary --right-of eDP-1  --crtc 1 --preferred &> /idk.log
else
    sleep 5
    $XRANDR_PATH/bin/xrandr --output DP-3 --off
    $XRANDR_PATH/bin/xrandr --output eDP-1 --primary --preferred
fi
