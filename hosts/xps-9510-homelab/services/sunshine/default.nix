{ pkgs, hostname, config, ... }:

let
  sway-steam-deck-resolution = pkgs.writers.writeBashBin "steam-deck-res" ''
    ${pkgs.sway}/bin/sway 'output eDP-1 mode --custom 1280x800@60.00Hz'
    ${pkgs.sway}/bin/sway 'workspace 2'
  '';
  sway-full-hd-resolution = pkgs.writers.writeBashBin "full-hd-res" ''
    ${pkgs.sway}/bin/sway 'output eDP-1 mode --custom 1920x1080@60.00Hz'
    ${pkgs.sway}/bin/sway 'workspace 2'
  '';
  # sway-ultrawide-resolution = pkgs.writers.writeBashBin "ultrawide-res" ''
  #   ${pkgs.sway}/bin/sway 'output eDP-1 mode --custom 3440x1440@60.00Hz'
  #   ${pkgs.sway}/bin/sway 'workspace 2'
  # '';
  sway-native-resolution = pkgs.writers.writeBashBin "native-res" ''
    ${pkgs.sway}/bin/sway 'output eDP-1 mode 1920x1200@60.00Hz'
    ${pkgs.sway}/bin/sway 'workspace 1'
  '';
  sunshineSettingsFile = (pkgs.formats.keyValue { }).generate "sunshine.conf" config.services.sunshine.settings;
in
{
  users.users.fridge = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [
      "wheel"
      "input"
      "video"
      "bumblebee"
      "users"
      "networkmanager"
      "audio"
      "gamemode"
    ];
  };

  services = {
    sunshine = {
      enable = true;
      openFirewall = true;
      capSysAdmin = true;
      autoStart = true;
      settings = {
        port = 47989; # the following ports will be opened: https://docs.lizardbyte.dev/projects/sunshine/en/latest/about/advanced_usage.html#port
        locale = "en";
        sunshine_name = "${hostname}";
        min_log_level = "info";

        upnp = "off";
      };

      applications = {
        # env = { };
        apps = [
          {
            name = "Desktop";
          }
          {
            name = "FullHD";
            prep-cmd = [
              {
                do = "${sway-full-hd-resolution}/bin/full-hd-res";
                undo = "${sway-native-resolution}/bin/native-res";
              }
            ];
          }
          {
            name = "Ultrawide";
            prep-cmd = [
              {
                do = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.$SUNSHINE_CLIENT_WIDTHx$SUNSHINE_CLIENT_HEIGHT@$SUNSHINE_CLIENT_FPS";
                undo = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.1920x1200@60";
              }
            ];
            exclude-global-prep-cmd = "false";
            auto-detach = "true";
          }
          {
            name = "Steam Big Picture";
            prep-cmd = [
              {
                do = "${sway-steam-deck-resolution}/bin/steam-deck-res";
                undo = "${sway-native-resolution}/bin/native-res";
              }
            ];
            detached = [
              "${pkgs.steam}/bin/steam steam://open/bigpicture"
            ];
            image-path = "steam.png";
          }
        ];
      };
    };

    pipewire = {
      enable = true;
      socketActivation = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
    };
  };

  programs = {
    steam = {
      enable = true;
      package = pkgs.steam;
      remotePlay.openFirewall = false;
      dedicatedServer.openFirewall = false;
      localNetworkGameTransfers.openFirewall = false;
    };

    gamemode = {
      enable = true;
      enableRenice = true;
    };

    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };
  };

  # to automatically start sunshine with systemd
  systemd.user.targets.sway-session = {
    unitConfig = {
      Description = "sway compositor session";
      Documentation = [ "man:systemd.special(7)" ];
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session-pre.target" ];
      After = [ "graphical-session-pre.target" ];
    };
  };

  # KDE
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  # hardware.pulseaudio.enable = true;
  services.displayManager.sddm.wayland.enable = true;

  services.getty.autologinUser = "fridge";
  environment = {
    systemPackages = with pkgs; [ protonup-qt firefox ];
    # in .local/bin/sway there is a custom binary with cap_sys_nice capabilities
    # in this way each game should re-nice itself to improve the gaming experience
    # loginShellInit = ''
    #   [[ "$(tty)" = "/dev/tty1" ]] && sway --unsupported-gpu
    # '';
    etc."sunshine/sunshine.conf".source = sunshineSettingsFile;
  };
}
