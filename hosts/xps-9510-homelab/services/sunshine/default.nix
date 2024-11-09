{ pkgs, hostname, ... }:

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
        apps = [
          {
            name = "Steam Big Picture - steam deck";
            prep-cmd = [
              {
                do = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.1280x800@60";
                undo = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.1920x1200@60";
              }
            ];
            detached = [
              "${pkgs.steam}/bin/steam steam://open/bigpicture"
            ];
            image-path = "steam.png";
            exclude-global-prep-cmd = "false";
            auto-detach = "true";
          }
          {
            name = "Desktop";
            prep-cmd = [
              {
                do = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.1920x1080@60";
                undo = "${pkgs.kdePackages.libkscreen}/bin/kscreen-doctor output.eDP-1.mode.1920x1200@60";
              }
            ];
            exclude-global-prep-cmd = "false";
            auto-detach = "true";
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
  };

  # KDE
  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;
  services.displayManager.sddm.wayland.enable = true;

  environment = {
    systemPackages = with pkgs; [ protonup-qt firefox ];
  };
}
