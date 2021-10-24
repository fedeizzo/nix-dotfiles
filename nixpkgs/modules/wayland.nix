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
    clipman
    # image viewer
    imv
    # screenshot for flameshoot
    grim
    # tray
    libappindicator
    # xdotool replacement
    ydotool
    # devour replacement
    swayhide
  ];
  programs.kitty = {
    enable = true;
    font.name = "JetBrains Mono";
    font.size = 11;
    settings = {
      "enable_audio_bell" = "no";
    };
    keybindings = {
      "alt+k" = "scroll_line_up";
      "alt+j" =  "scroll_line_up";
    };
    extraConfig = ''
      include ./nord.conf
    '';
  };
  xdg.configFile."kitty/nord.conf" = {
    source = ../dotfiles/kitty/nord.conf;
  };
  xdg.configFile."waybar/config" = {
    source = ../dotfiles/waybar/config;
  };
  xdg.configFile."waybar/style.css" = {
    source = ../dotfiles/waybar/style.css;
  };
  xdg.configFile."kanshi/config" = {
    source = ../dotfiles/kanshi;
  };
  xdg.configFile."sway" = {
    source = ../dotfiles/sway;
  };
}
