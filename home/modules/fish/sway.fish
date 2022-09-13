set TTY1 (tty)
set WLR_DRM_DEVICES "/dev/dri/card0:/dev/dri/card1"
[ "$TTY1" = "/dev/tty1" ] && exec sway
