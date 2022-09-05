#!/usr/bin/env bash
killall .waybar-wrapped
riverctl spawn 'waybar'

killall .nm-applet-wrapped
riverctl spawn 'nm-applet --indicator'

killall swaybg
riverctl spawn 'swaybg -i $HOME/.config/river/wallpaper.png'

killall swaync
riverctl spawn 'swaync'

killall kanshi
riverctl spawn 'kanshi'

floatig_kitty_processes=$(ps -aux | grep "class=floatingkitty" | wc -l)
if [[ $floatig_kitty_processes == 1 ]]; then
    riverctl spawn 'kitty --class="floatingkitty"'
    sleep 0.5
    riverctl set-view-tags 128
fi

riverctl spawn 'wl-paste -t text --watch clipman store'
riverctl spawn 'swayidle -w timeout 300 "swaylock --indicator-radius 0 -i $HOME/.config/sway/lock-screen.jpg" timeout 600 "swaymsg "output * dpms off"" resume "swaymsg "output * dpms on"" before-sleep "swaylock --indicator-radius 0 -i $HOME/.config/sway/lock-screen.jpg"'

rivertile -view-padding 6 -outer-padding 6
