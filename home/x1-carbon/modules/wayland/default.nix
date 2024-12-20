{ pkgs, ... }:

{
  imports = [
    ./gtk-qt.nix
    ./hyprland.nix
    ./kanshi.nix
    ./style.nix
    ./waybar.nix
    ../../../common/rofi
  ];

  home.packages = with pkgs; [
    wlr-randr # xrandr replacement
    kanshi # autorandr replacement
    wdisplays # arandr replacement
    ddcui # control external monitor
    ddcutil
    xdg-utils
    # clipboard
    wl-clipboard
    clipman
    # image viewerf
    imv
    # screenshot for flameshoot
    swappy
    grimblast
    libappindicator # tray
    wtype # xdotool replacement
    swaynotificationcenter # notification center
    socat # socket reader
    libinput-gestures
  ];
  xdg.configFile."libinput-gestures.conf" = {
    text = ''
      gesture swipe right 3 wtype -d 10 -M alt -k left -m alt
      gesture swipe left 3 wtype -d 10 -M alt -k right -m alt
      gesture swipe up 4 hyprctl dispatch togglespecialworkspace " "
    '';
  };
  home.file.".pam_environment" = {
    text = "XDG_CURRENT_DESKTOP_DEFAULT=Hyprland";
  };
}
