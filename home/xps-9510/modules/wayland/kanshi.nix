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
      }
      {
        profile.name = "docked";
        profile.outputs = [
          {
            criteria = "eDP-1";
            status = "disable";
            mode = "1920x1200@60.0Hz";
          }
          {
            criteria = "DP-3";
            status = "enable";
            mode = "3440x1440@60.0Hz";
          }
        ];
      }
    ];
  };
}
