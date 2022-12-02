{ pkgs, pkgs-unstable, ... }:

{
  imports = [
    ../../common/rofi
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    recommendedEnvironment = true;
    extraConfig = ''
      source=~/.config/hypr/config.conf
    '';
    systemdIntegration = false;
  };
  home.packages = with pkgs; [
    # river
    river
    glib
    lswt
    river-tag-overlay
    xdg-utils
    # dmenu replacement
    j4-dmenu-desktop
    wofi
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
    swappy
    grimblast
    # drawing
    # tray
    libappindicator
    # xdotool replacement
    wtype
    # devour replacement
    swayhide
    # autotiling
    autotiling
    # notification center
    swaync
    # gamma adapter
    wlsunset
    # keyboard manager
    swhkd
    # socket reader
    socat
    libinput-gestures
  ];
  programs.eww = {
    enable = true;
    package = pkgs-unstable.eww-wayland;
    configDir = ../dotfiles/eww;
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
  xdg.configFile."images/wallpaper.png" = {
    source = ../../common/images/wallpaper.png;
  };
  xdg.configFile."images/lock-screen.jpg" = {
    source = ../../common/images/lock-screen.jpg;
  };
  xdg.configFile."river" = {
    source = ../dotfiles/river;
  };
  xdg.configFile."swhkd/swhkdrc" = {
    source = ../dotfiles/swhkd/swhkdrc;
  };
  xdg.configFile."clight.conf" = {
    source = ../dotfiles/clight.conf;
  };

  xdg.configFile."libinput-gestures.conf" = {
    text = ''
      gesture swipe right 3 wtype -d 10 -M alt -k left -m alt
      gesture swipe left 3 wtype -d 10 -M alt -k right -m alt
      gesture swipe up 4 hyprctl dispatch togglespecialworkspace " "
    '';
  };
  xdg.configFile."environment.d/envvars.conf" = {
    text = ''
      XDG_RUNTIME_DIR=/run/user/$YOUR_USER_ID
      WAYLAND_DISPLAY=wayland-1
      SDL_VIDEODRIVER=wayland
      _JAVA_AWT_WM_NONREPARENTING=1
      QT_QPA_PLATFORM=wayland
      QT_QPA_PLATFORM_PLUGIN_PATH=${pkgs-unstable.qt6.qtwayland.outPath}/lib/qt-6/plugins/platforms
      XDG_SESSION_DESKTOP=hyrpland
      MOZ_ENABLE_WAYLAND="1"
      XDG_CURRENT_DESKTOP=Unity
      XDG_SESSION_TYPE="wayland"
      GTK_USE_PORTAL=0
    '';
  };
  home.file.".pam_environmet" = {
    text = "XDG_CURRENT_DESKTOP DEFAULT=hyrpland";
  };
  home.file.".xprofile" = {
    text = ''
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export QT_QPA_PLATFORM=wayland
      export QT_QPA_PLATFORM_PLUGIN_PATH=${pkgs-unstable.qt6.qtwayland.outPath}/lib/qt-6/plugins/platforms
      export XDG_SESSION_DESKTOP=hyrpland
      export MOZ_ENABLE_WAYLAND="1"
      export XDG_CURRENT_DESKTOP=Unity
      export XDG_SESSION_TYPE="wayland"
      export GTK_USE_PORTAL=0
    '';
    executable = true;
  };
}
