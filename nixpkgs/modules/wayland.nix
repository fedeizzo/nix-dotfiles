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
    slurp
    swappy
    # tray
    libappindicator
    # xdotool replacement
    ydotool
    # devour replacement
    swayhide
    # autotiling
    autotiling
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
  xdg.configFile."environment.d/envvars.conf" = {
    text = ''
      XDG_CURRENT_DESKTOP=sway
      XDG_RUNTIME_DIR=/run/user/$YOUR_USER_ID
      WAYLAND_DISPLAY=wayland-1
      SDL_VIDEODRIVER=wayland
      _JAVA_AWT_WM_NONREPARENTING=1
      QT_QPA_PLATFORM=wayland
      XDG_CURRENT_DESKTOP=sway
      XDG_SESSION_DESKTOP=sway
      MOZ_ENABLE_WAYLAND = "1"
      XDG_CURRENT_DESKTOP=Unity
      XDG_SESSION_TYPE = "wayland"
      GTK_USE_PORTAL=0
    '';
  };
  home.file.".pam_environmet" = {
    text = "XDG_CURRENT_DESKTOP DEFAULT=sway";
  };
  home.file.".xprofile" = {
    text = ''
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export QT_QPA_PLATFORM=wayland
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_DESKTOP=sway
      export MOZ_ENABLE_WAYLAND = "1"
      export XDG_CURRENT_DESKTOP=Unity
      export XDG_SESSION_TYPE = "wayland"
      export GTK_USE_PORTAL=0
    '';
    executable = true;
  };
}
