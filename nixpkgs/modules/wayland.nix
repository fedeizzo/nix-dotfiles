{ pkgs, inputs, system, ... }:

{
  home.packages = with pkgs; [
    # dmenu replacement
    bemenu
    # status bar
    waybar
    # xrandr replacement
    wlr-randr
    # autorandr replacement
    kanshi
    # arandr replacement
    wdisplays
    # feh replacement
    swaybg
    # clipboard
    wl-clipboard
    # image viewer
    imv
  ];
  xdg.configFile."waybar/config" = {
    source = ../dotfiles/waybar/config;
  };
  xdg.configFile."waybar/style.css" = {
    source = ../dotfiles/waybar/style.css;
  };
}
