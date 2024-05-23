{ pkgs-unstable, inputs, ... }:

{
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

      # Toolkit Backend Variables
      env = GDK_BACKEND,wayland,x11,*
      env = QT_QPA_PLATFORM,wayland;xcb
      env = SDL_VIDEODRIVER,wayland
      env = CLUTTER_BACKEND,wayland
      # XDG Specifications
      env = XDG_CURRENT_DESKTOP,Hyprland
      env = XDG_SESSION_TYPE,wayland
      env = XDG_SESSION_DESKTOP,Hyprland
      # Qt Variables
      env = QT_AUTO_SCREEN_SCALE_FACTOR,1
      env = QT_QPA_PLATFORM,wayland;xcb
      env = QT_WAYLAND_DISABLE_WINDOWDECORATION,1
      env = QT_QPA_PLATFORMTHEME,qt5ct
      # Others
      env = MOZ_ENABLE_WAYLAND,1
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
        "1"
        "2"
        "3"
        "4"
        "5"
        "6"
        "7"
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
        "$mod      , 1      , workspace                   ,  1                                       "
        "$mod      , 2      , workspace                   ,  2                                       "
        "$mod      , 3      , workspace                   ,  3                                       "
        "$mod      , 4      , workspace                   ,  4                                       "
        "$mod      , 5      , workspace                   ,  5                                       "
        "$mod      , 6      , workspace                   ,  6                                       "
        "$mod      , 7      , workspace                   ,  7                                       "
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
        "kanshi                                                                                                                                                                               "
        "~/.sources/create_kitty_scratchpad                                                                                                                                                   "
        "eww daemon                                                                                                                                                                           "
        "sleep 1 && eww open-many workspaces clock sys-info-panel backup                                                                                                                      "
        "sleep 5 && ~/.sources/launch_hyprlandevents                                                                                                                                          "
        "swaync                                                                                                                                                                               "
        "nm-applet --indicator                                                                                                                                                                "
        "wl-paste -t text --watch clipman store                                                                                                                                               "
      ];
    };
  };

  home.packages = with pkgs-unstable; [
    hyprpicker # colorpicker
    inputs.hyprland-contrib.packages.${pkgs.system}.hdrop
  ];

  services = {
    hyprpaper = {
      enable = true;
      settings = {
        preload = [
          "~/nix-dotfiles/home/common/images/wallpaper.png"
        ];
        wallpaper = [
          ",~/nix-dotfiles/home/common/images/wallpaper.png"
        ];
      };
    };

    hypridle = {
      enable = true;
      settings =
        {
          general = {
            lock_cmd = "pidof hyprlock || hyprlock"; # avoid starting multiple hyprlock instances.
            before_sleep_cmd = "loginctl lock-session"; # lock before suspend.
            after_sleep_cmd = "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
          };

          listener = [
            {
              timeout = 150; # 2.5min.
              on-timeout = "brightnessctl - s set 10"; # set monitor backlight to minimum, avoid 0 on OLED monitor.
              on-resume = "brightnessctl - r"; # monitor backlight restore.
            }
            {
              # turn off keyboard backlight
              timeout = 150; # 2.5min.
              on-timeout = "brightnessctl - sd rgb:kbd_backlight set 0"; # turn off keyboard backlight.
              on-resume = "brightnessctl - rd rgb:kbd_backlight"; # turn on keyboard backlight.
            }

            {
              timeout = 300; # 5min
              on-timeout = "loginctl lock-session"; # lock screen when timeout has passed
            }
            {
              timeout = 330; # 5.5min
              on-timeout = "hyprctl dispatch dpms off"; # screen off when timeout has passed
              on-resume = "hyprctl dispatch dpms on"; # screen on when activity is detected after timeout has fired.
            }
            {
              timeout = 1800; # 30min
              on-timeout = "systemctl suspend"; # suspend pc
            }
          ];
        };
    };
  };

  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        pam_module = "hyprlock";
      };
      background = [
        {
          path = "screenshot";
          blur_passes = 3;
          blur_size = 8;
        }
      ];
    };
  };
}
