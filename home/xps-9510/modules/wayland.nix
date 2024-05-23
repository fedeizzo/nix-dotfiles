{ pkgs, pkgs-unstable, ... }:

{
  imports = [
    ../../common/rofi
  ];
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    package = pkgs-unstable.hyprland;

    extraConfig = ''
      submap=resize
      binde=, right     , resizeactive , 10 0
      binde=, left      , resizeactive , -10 0
      binde=, up        , resizeactive , 0 -10
      binde=, down      , resizeactive , 0 10
      bindm=, mouse:272 , resizewindow
      bind= , escape    , submap       , reset

      submap=reset
    '';
    systemd = {
      enable = true;
    };
    settings = {
      "$mod" = "SUPER";
      general = {
        border_size = 2;
        no_border_on_floating = true;
        gaps_in = 5;
        gaps_out = 5;
        "col.active_border" = "0x55B48EAD";
        "col.inactive_border" = "0xFF2E3440";
        layout = "master";
      };

      decoration = {
        rounding = 5;
        active_opacity = 1;
        inactive_opacity = 1;
        drop_shadow = true;
        blur = {
          enabled = true;
          size = 3;
          passes = 1;
        };
      };

      animations = {
        enabled = true;
      };

      animation = [
        "workspaces,1,5,default,slide"
        "windows,1,5,default,slide"
      ];

      input = {
        kb_layout = "us";
        kb_variant = "altgr-intl";
        follow_mouse = 1;
        touchpad = {
          natural_scroll = true;
          disable_while_typing = true;
          tap-to-click = true;
        };
      };

      gestures = {
        workspace_swipe = true;
        workspace_swipe_fingers = 4;
        workspace_swipe_create_new = false;
        workspace_swipe_forever = true;
      };

      misc = {
        disable_hyprland_logo = true;
        enable_swallow = true;
        swallow_regex = "^(kittylf)$";
      };

      master = {
        new_is_master = true;
        new_on_top = true;
      };

      monitor = [
        "eDP-1,1920x1200@60,0x0,1"
      ];
      workspace = [
        "eDP-1,1"
      ];

      windowrulev2 = [
        "float,class:^(pinentry-qt)$,title:"
        "float,class:^(firefox)$,title:^(Picture-in-Picture)$"
        "idleinhibit fullscreen,class:^(firefox)$,title:"
        "float,class:,title:^(Firefox — Sharing Indicator)$"
        "workspace 4 silent,class:^(telegramdesktop)$,title:^(Telegram*)$"
        "noanim,class:^(kittylf)$,title:"
        "opacity 0.9,class:^(kittylf)$,title:"
        "float,class:,title:Solanum"
        "float,class:blueberry.py,title:"
        "float,class:,title:^(Zoom*)$"
        "float,class:Bitwarden,title:"
        "float,class:anki,title:"
      ];

      bind = [
        "$mod      , Q      , killactive             ,                                                                       "
        "$mod      , T      , togglefloating         ,                                                                       "
        "$mod      , Y      , pin                    ,                                                                       "
        "$mod      , F      , fullscreen             ,                                                                       "
        "$modSHIFT , ESCAPE , exit                   ,                                                                       "
        "$mod      , TAB    , exec                   , ~/.sources/hyprland_workspace                                         "
        "$mod      , U      , layoutmsg              , swapwithmaster                                                        "
        "$mod      , K      , layoutmsg              , cycleprev                                                             "
        "$mod      , J      , layoutmsg              , cyclenext                                                             "
        "$modSHIFT , K      , layoutmsg              , swapprev                                                              "
        "$modSHIFT , J      , layoutmsg              , swapnext                                                              "
        "$mod      , R      , submap                 , resize                                                                "
        "$modSHIFT , I      , movetoworkspacesilent  , special                                                               "
        "$mod      , I      , exec                   , ~/.sources/create_kitty_scratchpad                                    "
        "$mod      , I      , togglespecialworkspace ,                                                                       "
        "$mod      , 1      , exec                   , ~/.sources/hyprland_workspace 1                                       "
        "$mod      , 2      , exec                   , ~/.sources/hyprland_workspace 2                                       "
        "$mod      , 3      , exec                   , ~/.sources/hyprland_workspace 3                                       "
        "$mod      , 4      , exec                   , ~/.sources/hyprland_workspace 4                                       "
        "$mod      , 5      , exec                   , ~/.sources/hyprland_workspace 5                                       "
        "$mod      , 6      , exec                   , ~/.sources/hyprland_workspace 6                                       "
        "$mod      , 7      , exec                   , ~/.sources/hyprland_workspace 7                                       "
        "$modSHIFT , 1      , movetoworkspacesilent  , 1                                                                     "
        "$modSHIFT , 2      , movetoworkspacesilent  , 2                                                                     "
        "$modSHIFT , 3      , movetoworkspacesilent  , 3                                                                     "
        "$modSHIFT , 4      , movetoworkspacesilent  , 4                                                                     "
        "$modSHIFT , 5      , movetoworkspacesilent  , 5                                                                     "
        "$modSHIFT , 6      , movetoworkspacesilent  , 6                                                                     "
        "$modSHIFT , 7      , movetoworkspacesilent  , 7                                                                     "
        "$mod      , S      , exec                   , rofi -show \"fd\" -modi \"fd:~/.sources/rofi_spotlight\"              "
        "$modSHIFT , N      , exec                   , swaync-client -t -sw                                                  "
        "$mod      , RETURN , exec                   , kitty                                                                 "
        "$mod      , E      , exec                   , emacsclient -c                                                        "
        "$mod      , D      , exec                   , rofi -show drun                                                       "
        "$mod      , B      , exec                   , rofi-rbw                                                              "
        "$mod      , W      , exec                   , kitty --class kittylf /home/fedeizzo/.sources/lfrun                   "
        "$mod      , C      , exec                   , clipman pick -t rofi                                                  "
        "$mod      , X      , exec                   , swaylock --indicator-radius 0 -i $HOME/.config/images/lock-screen.jpg "
        "$modSHIFT , Z      , exit                   ,                                                                       "
        "$mod      , P      , exec                   , rofi -show \"sc\" -modi \"sc:~/.sources/rofi_screenshot\"             "
        "$modSHIFT , C      , forcerendererreload    ,                                                                       "
      ];

      bindl = [ ];
      bindr = [ ];
      binde = [
        "$mod      , H                     , resizeactive           , -10 0"
        "$mod      , L                     , resizeactive           , 10 0"
        "          , XF86AudioRaiseVolume  , exec                   , pamixer --increase 5                          "
        "          , XF86AudioLowerVolume  , exec                   , pamixer --decrease 5                          "
        "          , XF86AudioMute         , exec                   , pamixer -t                                    "
        "          , XF86AudioMicMute      , exec                   , pactl set-source-mute @DEFAULT_SOURCE@ toggle "
        "          , XF86MonBrightnessDown , exec                   , brightnessctl set 10%-                        "
        "          , XF86MonBrightnessUp   , exec                   , brightnessctl set +10%                        "
        "          , XF86AudioPlay         , exec                   , playerctl play-pause                          "
        "          , XF86AudioNext         , exec                   , playerctl next                                "
        "          , XF86AudioPrev         , exec                   , playerctl previous                            "

      ];
      bindn = [ ];
      bindm = [
        "$mod      , mouse:272 , movewindow"
        "$modSHIFT , mouse:272 , resizewindow"
      ];
      bindt = [ ];
      bindi = [ ];

      exec-once = [
        "swaybg -i $HOME/.config/images/wallpaper.png                                                                                                                                         "
        "kanshi                                                                                                                                                                               "
        "~/.sources/create_kitty_scratchpad                                                                                                                                                   "
        "eww daemon                                                                                                                                                                           "
        "sleep 1 && eww open-many workspaces clock sys-info-panel backup                                                                                                                      "
        "sleep 5 && ~/.sources/launch_hyprlandevents                                                                                                                                          "
        "swaync                                                                                                                                                                               "
        "clight                                                                                                                                                                               "
        "nm-applet --indicator                                                                                                                                                                "
        "wl-paste -t text --watch clipman store                                                                                                                                               "
        "idle_manager                                                                                                                                                                         "
        "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP                                                                                                     "
        "systemctl --user import-environment DISPLAY WAYLAND_DISPLAY SWAYSOCK XDG_CURRENT_DESKTOP                                                                                             "
        "hash dbus-update-activation-environment 2>/dev/null && dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP "
      ];
    };
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
      QT_QPA_PLATFORM=wayland-egl
      QT_QPA_PLATFORM_PLUGIN_PATH=${pkgs.qt6.qtwayland.outPath}/lib/qt-6/plugins/platforms
      XDG_SESSION_DESKTOP=Hyprland
      MOZ_ENABLE_WAYLAND="1"
      XDG_CURRENT_DESKTOP=Hyprland
      XDG_SESSION_TYPE="wayland"
      GTK_USE_PORTAL=0
    '';
  };
  home.file.".pam_environmet" = {
    text = "XDG_CURRENT_DESKTOP_DEFAULT=Hyprland";
  };
  home.file.".xprofile" = {
    text = ''
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export QT_QPA_PLATFORM=wayland-egl
      export QT_QPA_PLATFORM_PLUGIN_PATH=${pkgs.qt6.qtwayland.outPath}/lib/qt-6/plugins/platforms
      export XDG_SESSION_DESKTOP=Hyprland
      export MOZ_ENABLE_WAYLAND="1"
      export XDG_CURRENT_DESKTOP=Hyprland
      export XDG_SESSION_TYPE="wayland"
      export GTK_USE_PORTAL=0
    '';
    executable = true;
  };
}
