{ pkgs, ... }:

{
  programs = {
    bash = {
      completion.enable = true;
      enableLsColors = true;
    };
    fish.enable = true;
    localsend = {
      enable = true;
      openFirewall = true;
    };
  };
  environment = {
    sessionVariables = {
      "GTK_USE_PORTAL" = "1";
    };
    shells = [ pkgs.bash pkgs.fish ];
    pathsToLink = [
      "/share/fish"
      # to fix xournal problem
      "/share/icons"
      "/share/mime"
    ];
    variables = {
      "EDITOR" = "vim";
      "READER" = "zathura";
      "VISUAL" = "vim";
      "CODEEDITOR" = "vim";
      "TERMINAL" = "kitty";
      "BROWSER" = "zen";
      "COLORTERM" = "truecolor";
    };
    shellAliases = {
      # cp optimized for btrfs
      "cp" = "cp --reflink=auto -i";
      "open" = "xdg-open";
      "SS" = "systemctl";
    };
    systemPackages = with pkgs; [
      vim
      curl
      bc
      killall
      libnotify
      neofetch
      wget
      git
      highlight

      seatd
      pkgs.adwaita-icon-theme
      pkgs.shared-mime-info
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

  virtualisation.oci-containers.backend = "docker";
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      enableNvidia = false;
    };
    podman.enable = false;
  };
}
