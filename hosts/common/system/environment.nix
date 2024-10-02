{ pkgs, ... }:

{
  programs.hyprland.enable = true;
  environment.sessionVariables = {
    "GTK_USE_PORTAL" = "1";
  };
  environment.shells = [ pkgs.bash pkgs.fish ];
  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };
  programs.fish.enable = true;
  environment.pathsToLink = [
    "/share/fish"
    # to fix xournal problem
    "/share/icons"
    "/share/mime"
  ];
  environment.variables = {
    "EDITOR" = "vim";
    "READER" = "zathura";
    "VISUAL" = "vim";
    "CODEEDITOR" = "vim";
    "TERMINAL" = "kitty";
    "BROWSER" = "firefox";
    "COLORTERM" = "truecolor";
  };
  environment.shellAliases = {
    # cp optimized for btrfs
    "cp" = "cp --reflink=auto -i";
    "open" = "xdg-open";
    "SS" = "systemctl";
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

  environment.systemPackages = with pkgs; [
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
    pkgs.gnome.adwaita-icon-theme
    pkgs.shared-mime-info
  ];
}
