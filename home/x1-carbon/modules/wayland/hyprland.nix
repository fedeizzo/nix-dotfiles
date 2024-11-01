{ pkgs, ... }:

{
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    package = pkgs.hyprland;
    plugins = [ ];

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
        # appearance
        border_size = 2;
        no_border_on_floating = true;
        gaps_in = 5;
        gaps_out = 5;
        # "col.active_border" = "0x55B48EAD";
        # "col.inactive_border" = "0xFF2E3440";

        # layout
        layout = "master";

        # functionality
        resize_on_border = true;
        extend_border_grab_area = 25;
        hover_icon_on_border = true;
      };

      decoration = {
        rounding = 5;
        active_opacity = 1;
        inactive_opacity = 1;
        drop_shadow = false;
        blur = {
          enabled = false;
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
          scroll_factor = 0.75;
        };
      };

      device = {
        name = "tpps/2-elan-trackpoint";
        sensitivity = 0.75;
      };

      gestures = {
        workspace_swipe = true;
        workspace_swipe_fingers = 4;
        workspace_swipe_create_new = false;
        workspace_swipe_forever = true;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_autoreload = true;
        enable_swallow = true;
        vfr = false;
      };

      master = {
        new_status = "master";
        new_on_top = true;
      };

      monitor = [
        ",preferred,auto,1"
      ];
      workspace = [ "1" "2" "3" "4" "5" "6" "7" ];

      windowrulev2 = [
        "float,class:^(pinentry-qt)$,title:"
        "float,class:^(firefox)$,title:^(Picture-in-Picture)$"
        "idleinhibit fullscreen,class:^(firefox)$,title:"
        "float,class:,title:^(Firefox — Sharing Indicator)$"
        "workspace 4 silent,class:^(telegramdesktop)$,title:^(Telegram*)$"
        "float,class:,title:Solanum"
        "float,class:blueberry.py,title:"
        "float,class:,title:^(Zoom*)$"
        "float,class:Bitwarden,title:"
        "float,class:anki,title:"
      ];

      bind = [
        # workspaces
        "$mod, 1, workspace, 1"
        "$mod, 2, workspace, 2"
        "$mod, 3, workspace, 3"
        "$mod, 4, workspace, 4"
        "$mod, 5, workspace, 5"
        "$mod, 6, workspace, 6"
        "$mod, 7, workspace, 7"
        "$modSHIFT, 1, movetoworkspacesilent, 1"
        "$modSHIFT, 2, movetoworkspacesilent, 2"
        "$modSHIFT, 3, movetoworkspacesilent, 3"
        "$modSHIFT, 4, movetoworkspacesilent, 4"
        "$modSHIFT, 5, movetoworkspacesilent, 5"
        "$modSHIFT, 6, movetoworkspacesilent, 6"
        "$modSHIFT, 7, movetoworkspacesilent, 7"

        # windows
        "$mod, Y, pin," # pin window in all monitors
        "$mod, Q, killactive,"
        "$mod, T, togglefloating,"
        "$mod, F, fullscreen,"
        "$mod, R, submap, resize"

        # windows movement
        "$mod, U, layoutmsg, swapwithmaster"
        "$mod, K, layoutmsg, cycleprev"
        "$mod, J, layoutmsg, cyclenext"
        "$modSHIFT, K, layoutmsg, swapprev"
        "$modSHIFT, J, layoutmsg, swapnext"

        # misc system
        "$mod, X, exec, hyprlock"
        "$mod, N, exec, swaync-client -t -sw"

        # exec programs
        "$mod&Control_L,I,exec,hdrop -f -p t kitty --class speciakitty"
        "$mod, RETURN, exec, kitty"
        "$mod, D     , exec, rofi -show drun"
        "$mod, B     , exec, rofi-rbw"
        # "$mod, W     , exec, kitty --class kittylf /home/fedeizzo/.sources/lfrun"
        "$mod, C     , exec, clipman pick -t rofi"
        "$mod, P     , exec, rofi -show \"sc\" -modi \"sc:~/.sources/rofi_screenshot\""

        # hyprland
        "$modSHIFT, R, exec, hyprctl reload"
        "$modSHIFT, ESCAPE , exit,"
        "$modSHIFT, C, forcerendererreload,"
      ];

      bindl = [ ];
      bindr = [ ];
      binde = [
        "$mod, H, resizeactive, -10 0"
        "$mod, L, resizeactive, 10 0"
        ", XF86AudioRaiseVolume, exec, swayosd-client --output-volume 5"
        ", XF86AudioLowerVolume, exec, swayosd-client --output-volume -5"
        ", XF86AudioMute, exec, swayosd-client --output-volume mute-toggle"
        ", XF86AudioMicMute, exec, pactl set-source-mute @DEFAULT_SOURCE@ toggle"
        ", XF86MonBrightnessUp, exec, swayosd-client --brightness +10"
        ", XF86MonBrightnessDown, exec, swayosd-client --brightness -10"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPrev, exec, playerctl previous"
        ", XF86Display, exec, wdisplays"
      ];
      bindn = [ ];
      bindm = [
        "$mod, mouse:272, movewindow"
        "$modSHIFT, mouse:272, resizewindow"
      ];
      bindt = [ ];
      bindi = [ ];

      exec-once = [
        "swaync" # notification center
        "hdrop --floating --position t --background kitty --class scratchpad" # scratchpad
        "wl-paste -t text --watch clipman store" # init clipboard
        "waybar"
      ];
    };
  };

  home.packages = with pkgs; [
    hyprpicker # colorpicker
    hdrop
  ];

  services = {
    hyprpaper = {
      enable = true;
      settings = {
        splash = false;
        ipc = false;
        preload = [
          "~/nix-dotfiles/home/common/images/wallpaper.png"
          "~/nix-dotfiles/home/common/images/wallpaper_ultrawide.png"
        ];
        wallpaper = [
          "eDP-1,~/nix-dotfiles/home/common/images/wallpaper.png"
          "desc:Dell Inc. DELL U3423WE HPWLMP3,~/nix-dotfiles/home/common/images/wallpaper_ultrawide.png"
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

    swayosd = {
      # show info while updating volume, brightness, etc.
      enable = true;
      package = pkgs.swayosd;
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
