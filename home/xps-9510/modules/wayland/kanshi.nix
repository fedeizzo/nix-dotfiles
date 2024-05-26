{ ... }:

{
  services.kanshi = {
    enable = true;
    systemdTarget = "hyprland-session.target";

    settings = [
      {
        profile.name = "undocked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1200@59.950Hz";
          }
        ];
        profile.exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup" ];
      }
      {
        profile.name = "docked";
        profile.outputs = [
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
        profile.exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup" ];
      }
      {
        profile.name = "docked2";
        profile.outputs = [
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
        profile.exec = [ "eww --no-daemonize open-many clock sys-info-panel bluetooth-info-panel backup" ];
      }
    ];
  };
}
