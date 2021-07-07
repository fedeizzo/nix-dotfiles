{ config, pkgs, libs, ... }:

{
  services.lorri.enable = true;
  services.network-manager-applet.enable = true;
  systemd.user.services = {
    mpris-proxy = {
      Unit = {
        Description = "Forward bluetooth midi controls via mpris2 so they are picked up by supporting media players";
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.mpd-mpris}/bin/mpris-proxy";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
