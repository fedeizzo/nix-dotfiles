{
  flake-file.inputs.niri.url = "github:sodiboo/niri-flake";
  flake-file.inputs.dms.url = "github:AvengeMedia/DankMaterialShell/stable";
  flake-file.inputs.dms-plugin-registry.url = "github:AvengeMedia/dms-plugin-registry";

  flake.modules.nixos.desktop-environment = { inputs, pkgs, ... }: {
    imports = [
      inputs.dms.nixosModules.greeter
    ];
    programs.dank-material-shell.greeter = {
      enable = true;
      compositor.name = "niri";
      compositor.customConfig = ''
        hotkey-overlay {
          skip-at-startup
        }
        environment {
          DMS_RUN_GREETER "1"
        }
        gestures {
          hot-corners {
            off
          }
        }
        layout {
          background-color "#000000"
        }
      '';
      logs = {
        save = true;
        path = "/tmp/dms-greeter.log";
      };
    };
    programs.dconf.enable = true;
    environment.pathsToLink = [ "/share/applications" "/share/xdg-desktop-portal" ];
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;
    };
  };

  flake.modules.homeManager.desktop-environment = { pkgs, config, username, pkgs-unstable, ... }: {
    programs.niri = {
      enable = true;
      package = pkgs.niri-unstable;

      settings = {
        # Input
        input = {
          # devices
          keyboard = {
            xkb = {
              layout = "us";
              variant = "";
            };
          };
          touchpad = {
            natural-scroll = true;
            scroll-method = "two-finger";
          };
          touch.map-to-output = "eDP-1";

          focus-follows-mouse.enable = true;

          mod-key = "Alt";
        };

        outputs = {
          "eDP-1" = {
            scale = 1.25;
          };
          "DP-3" = {
            scale = 1.25;
            mode = {
              width = 3440;
              height = 1440;
              refresh = 59.973;
            };
          };
          # hot-corners.top-left.enable = true;
          focus-at-start-up.enable = true;
        };

        # Layout
        layout = {
          gaps = 8;
          focus-ring.enable = true;
        };

        # Cursor
        cursor = {
          hide-when-typing = true;
          theme = "Bibata Modern Eyes";
        };

        # Binds
        binds = with config.lib.niri.actions; {
          "Super+Shift+Slash".action = show-hotkey-overlay;
          "Super+Return".action.spawn = "kitty";
          "Super+y".action.spawn = [ "dms" "ipc" "call" "plugins" "toggle" "aiAssistant" ];
          "Super+P".action.screenshot = { };

          # Display management
          "XF86Display".action.spawn = [
            "sh"
            "-c"
            ''
              if niri msg outputs | grep -qw "DP-3"; then
                niri msg output eDP-1 off
                niri msg output DP-3 on
              else
                niri msg output DP-3 off
                niri msg output eDP-1 on
              fi
            ''
          ];

          # Window management
          "Super+Q".action = close-window;
          "Super+Q".repeat = false;
          "Super+O".action = toggle-overview;
          "Super+O".repeat = false;
          "Super+Left".action = focus-column-left;
          "Super+H".action = focus-column-left;
          "Super+M".action = focus-column-left;
          "Super+Down".action = focus-workspace-down;
          "Super+J".action = focus-workspace-down;
          "Super+N".action = focus-workspace-down;
          "Super+Right".action = focus-column-right;
          "Super+L".action = focus-column-right;
          "Super+I".action = focus-column-right;
          "Super+Up".action = focus-workspace-up;
          "Super+K".action = focus-workspace-up;
          "Super+E".action = focus-workspace-up;
          "Super+Shift+Left".action = move-column-left;
          "Super+Shift+H".action = move-column-left;
          "Super+Shift+M".action = move-column-left;
          "Super+Shift+Down".action = move-window-down;
          "Super+Shift+J".action = move-window-down;
          "Super+Shift+Right".action = move-column-right;
          "Super+Shift+L".action = move-column-right;
          "Super+Shift+I".action = move-column-right;
          "Super+Shift+Up".action = move-window-up;
          "Super+Shift+K".action = move-window-up;

          "Super+F".action = maximize-column;
          "Super+Shift+F".action = fullscreen-window;
          "Super+Ctrl+F".action = expand-column-to-available-width;
          "Super+T".action = toggle-window-floating;

          "Super+U".action.set-window-width = "+10";
          "Super+Equal".action.set-window-width = "+10";
          "Super+Minus".action.set-window-width = "-10";
          "Super+Comma".action.set-window-width = "-10";
        };

        # Startup
        # spawn-at-startup = [
        #   { argv = [ "waybar" ]; }
        # ];

        # Animations
        animations = {
          enable = true;
        };

        # Gestures
        gestures = {
          hot-corners.enable = true;
        };

        # Environment
        environment = {
          QT_QPA_PLATFORM = "wayland";
          GTK_THEME = "Adwaita:dark";
        };
      };
    };
    # systemd.user.services.niri-flake-polkit.enable = false;
    programs.dank-material-shell = {
      enable = true;
      dgop.package = pkgs-unstable.dgop;
      niri = {
        enableKeybinds = true; # Sets static preset keybinds
        enableSpawn = true; # Auto-start DMS with niri, if enabled
      };

      settings = (import ./settings-dms.data { }).settings;
      session = (import ./settings-dms.data { }).session;


      clipboardSettings = {
        maxHistory = 25;
        maxEntrySize = 5242880;
        autoClearDays = 1;
        clearAtStartup = true;
        disabled = false;
        disableHistory = false;
        disablePersist = true;
      };

      plugins = {
        # nord.enable = true;
        aiAssistant = {
          enable = true;

          settings = {
            providers = {
              custom = {
                baseUrl = "https://llama.fedeizzo.dev/v1";
                model = "qwen36-27b-realtime";
                apiKey = "placeholder";
                saveApiKey = false;
                apiKeyEnvVar = "";
                temperature = 0.7;
                maxTokens = 32768;
                timeout = 120;
              };
            };
            provider = "custom";
            baseUrl = "https://llama.fedeizzo.dev/v1";
            model = "qwen36-27b-realtime";
            apiKey = "";
            saveApiKey = false;
            apiKeyEnvVar = "";
            temperature = 0.7;
            maxTokens = 32768;
            timeout = 30;
            geminiWebSearch = false;
          };
        };
      };
    };

    # plugin dependencies
    home.packages = with pkgs; [
      wl-clipboard
    ];
  };
}
