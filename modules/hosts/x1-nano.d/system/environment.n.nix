{ pkgs, ... }:

{
  flake.modules.nixos.x1-nano = {
    programs.localsend = {
      enable = true;
      openFirewall = true;
    };

    environment = {
      shells = [ pkgs.fish ];
      sessionVariables = {
        "GTK_USE_PORTAL" = "1";
      };
      pathsToLink = [
        "/share/fish"
        # to fix xournal problem
        "/share/icons"
        "/share/mime"
      ];
      variables = {
        "READER" = "zathura";
        "CODEEDITOR" = "vim";
        "TERMINAL" = "kitty";
        "BROWSER" = "zen";
        "COLORTERM" = "truecolor";
      };
      shellAliases = {
        "open" = "xdg-open";
      };
      systemPackages = with pkgs; [
        libnotify
        seatd
        adwaita-icon-theme
        shared-mime-info
      ];
    };

    systemd.services = {
      seatd = {
        enable = true;
        description = "Seat management daemon";
        script = "${pkgs.seatd}/bin/seatd -g wheel";
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          RestartSec = "1";
        };
        wantedBy = [ "multi-user.target" ];
      };
    };
  };
}
