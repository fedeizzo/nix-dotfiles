_:

{
  services.kanshi = {
    enable = true;
    systemdTarget = "hyprland-session.target";

    settings = [
      {
        profile = {
          name = "undocked";
          outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
              mode = "1920x1200@59.950Hz";
            }
          ];
          exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup workspaces" ];
        };
      }
      {
        profile = {
          name = "docked";
          outputs = [
            {
              criteria = "eDP-1";
              status = "disable";
            }
            {
              criteria = "DP-3";
              status = "enable";
              mode = "3440x1440@60.0Hz";
            }
          ];
          exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup workspaces" ];
        };
      }
      {
        profile = {
          name = "docked2";
          outputs = [
            {
              criteria = "eDP-1";
              status = "disable";
            }
            {
              criteria = "DP-1";
              status = "enable";
              mode = "3440x1440@60.0Hz";
            }
          ];
          exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup workspaces" ];
        };
      }
    ];
  };
}
