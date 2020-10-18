{ config, pkgs, libs, ... }:

{
  services.lorri.enable = true;
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
    # greenclip = {
    #   Unit = {
    #     Description = "Greenclip agent";
    #     After = "display-manager.service";
    #   };
    #   Service = {
    #     Type = "simple";
    #     ExecStart = "${pkgs.haskellPackages.greenclip}/bin/greenclip daemon";
    #     Restart = "always";
    #   };
    #   Install = {
    #     WantedBy = [ "default.target" ];
    #   };
    # };
    # spotifyd = {
    #   Unit = {
    #     Description = "A spotify playing daemon";
    #     After = [ "sound.target" "network.target" ];
    #     Wants = [ "sound.target" "network.target" ];
    #   };
    #   Service = {
    #     ExecStart = "${pkgs.spotifyd}/bin/spotifyd --no-daemon";
    #     Restart = "always";
    #     RestartSec = "12";
    #   };
    #   Install = {
    #     WantedBy = [ "default.target" ];
    #   };
    # };
  };
}
