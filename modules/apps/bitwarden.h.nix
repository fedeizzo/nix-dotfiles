{
  flake.modules.homeManager.bitwarden = { config, pkgs, ... }: {
    home.packages = [ pkgs.bitwarden-desktop ];
    home.sessionVariables.SSH_AUTH_SOCK = "${config.home.homeDirectory}/.bitwarden-ssh-agent.sock";

    systemd.user.services.bitwarden = {
      Unit = {
        Description = "Bitwarden desktop and SSH agent";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };
      Service = {
        ExecStart = "${pkgs.bitwarden-desktop}/bin/bitwarden";
        Restart = "on-failure";
        RestartSec = 5;
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
