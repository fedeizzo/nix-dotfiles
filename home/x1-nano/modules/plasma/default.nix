{ ... }:

{
  imports = [
    ./autogenerate.nix
  ];

  programs.plasma = {
    enable = true;
    immutableByDefault = true;
    overrideConfig = true;

    input = {
      touchpads = [
        {
          enable = true;
          middleButtonEmulation = true;
          name = "ELAN0670:00 04F3:3150 Touchpad";
          naturalScroll = true;
          vendorId = "04f3";
          productId = "3150";
        }
        {
          enable = true;
          middleButtonEmulation = true;
          name = "TPPS/2 Elan TrackPoint";
          vendorId = "0002";
          productId = "000a";
          pointerSpeed = -0.6;
        }
      ];
    };

    powerdevil = {
      general.pausePlayersOnSuspend = true;
      AC = {
        autoSuspend = {
          action = "sleep";
          idleTimeout = 600; # 10 minutes
        };
        dimDisplay = {
          enable = true;
          idleTimeout = 180; # 3 minutes
        };
        inhibitLidActionWhenExternalMonitorConnected = true;
        whenLaptopLidClosed = "sleep";
        powerButtonAction = "lockScreen";
        powerProfile = "performance";
        turnOffDisplay = {
          idleTimeout = 300; # 5 minutes
          idleTimeoutWhenLocked = 30;
        };
      };
      battery = {
        autoSuspend = {
          action = "sleep";
          idleTimeout = 300; # 5 minutes
        };
        dimDisplay = {
          enable = true;
          idleTimeout = 150; # 2.5 minutes
        };
        inhibitLidActionWhenExternalMonitorConnected = true;
        whenLaptopLidClosed = "sleep";
        powerButtonAction = "lockScreen";
        powerProfile = "balanced";
        turnOffDisplay = {
          idleTimeout = 180; # 3 minutes
          idleTimeoutWhenLocked = 30;
        };
      };
    };

    session.sessionRestore.restoreOpenApplicationsOnLogin = "startWithEmptySession";

    # keys
    hotkeys = {
      commands = {
        kitty = {
          command = "kitty";
          comment = "Run kitty";
          key = "Meta+Return";
        };
        screenshot = {
          command = "spectacle --region --copy-image  --background";
          comment = "Run spectacle in backgronud mode";
          key = "Meta+Shift+P";
        };
      };
    };
    spectacle.shortcuts.launchWithoutCapturing = "Meta+S";


    # appearance
    # fonts = { };
    panels = [{
      # dock
      location = "top";
      alignment = "center";
      floating = true;
      height = 32;
      hiding = "none";
      offset = 8;
      lengthMode = "fit";

      widgets = [
        "org.kde.plasma.icontasks"
        {
          pager = {
            general = {
              showOnlyCurrentScreen = true;
              showApplicationIconsOnWindowOutlines = true;
              navigationWrapsAround = true;
            };
          };
        }
        "org.kde.plasma.systemtray"
        {
          digitalClock = {
            date.format = "isoDate";
            time.format = "24h";
            calendar.firstDayOfWeek = "monday";
          };
        }
      ];
    }];
    workspace = {
      colorScheme = "BreezeDark";
      lookAndFeel = "org.kde.breezedark.desktop";
      theme = "breeze-dark";
      wallpaper = ../../../common/images/wallpaper.png;
    };

    # window manager
    kwin = {
      borderlessMaximizedWindows = true;
      # scripts.polonium = {
      #   enable = true;
      #   settings = {
      #     borderVisibility = "borderSelected";
      #     layout.engine = "half";
      #     layout.insertionPoint = "left";
      #   };
      # };
      virtualDesktops = {
        names = [ "Desktop 1" "Desktop 2" "Desktop 3" "Desktop 4" "Desktop 5" "Desktop 6" "Desktop 7" ];
        number = 7;
        rows = 1;
      };
      effects.snapHelper.enable = true;
    };

    # apps
    # konsole = {
    #   enable = true;
    # };

    krunner = {
      historyBehavior = "enableAutoComplete";
      position = "center";
    };

    kscreenlocker = {
      appearance.wallpaper = ../../../common/images/wallpaper.png;

      autoLock = true;
      lockOnResume = true;
      passwordRequired = true;
      timeout = 5; # minutes
    };
  };
}
