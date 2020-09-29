{ config, pkgs, libs, ... }:

{
  systemd.user.services = {
    mpris-proxy = {
      Unit = {
        Description = "Forward bluetooth midi controls via mpris2 so they are picked up by supporting media players";
      };
      Service = {
        Type = "simple";
        ExecStart = "/usr/bin/mpris-proxy";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
    greenclip = {
      Unit = {
        Description = "Greenclip agent";
        After = "display-manager.service";
      };
      Service = {
        Type = "simple";
        ExecStart = "/usr/bin/greenclip daemon";
        Restart = "always";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
