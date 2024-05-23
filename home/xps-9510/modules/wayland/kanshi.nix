{ ... }:

{
  services.kanshi = {
    enable = true;
    systemdTarget = "hyprland-session.target";

    profiles = {
      undocked = {
        outputs = [
          {
            criteria = "eDP-1";
            status = "enable";
            mode = "1920x1200@59.950Hz";
          }
        ];
      };
    };
  };
}
