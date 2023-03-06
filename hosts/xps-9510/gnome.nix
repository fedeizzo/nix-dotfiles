{ config, pkgs, nixpkgs-old, ... }:

{
  # GNOME
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.gnome.gnome-keyring.enable = true;
  # TOOLS
  programs.gnome-documents.enable = false;
  services.gnome.gnome-user-share.enable = false;
  services.gnome.gnome-online-accounts.enable = false;
  services.gnome.games.enable = false;
  programs.seahorse.enable = false;
  services.gnome.gnome-browser-connector.enable = false;
  programs.gnome-terminal.enable = false;
  services.gnome.core-utilities.enable = true;
  services.gnome.core-shell.enable = true;
  services.gnome.core-developer-tools.enable = true;
  services.gnome.core-os-services.enable = true;
  environment.gnome.excludePackages = with pkgs; [
    orca # blind help
    gnome.gnome-user-docs # useles
    baobab # disk analyzer
    gnome.cheese # game
    epiphany # web browser
    gnome-text-editor
    gnome.gnome-calculator
    gnome.gnome-calendar
    gnome.gnome-characters
    gnome.gnome-clocks
    gnome-console
    gnome.gnome-contacts
    gnome.gnome-font-viewer
    gnome.gnome-logs
    gnome.gnome-maps
    gnome.gnome-music
    gnome.gnome-photos
    gnome.gnome-system-monitor
    gnome.weather
    gnome-connections
    gnome.simple-scan
    gnome.totem
    gnome.yelp
    gnome.devhelp
    gnome-builder
  ];
  programs.geary.enable = false;
  programs.evince.enable = false;
  services.gnome.gnome-initial-setup.enable = false;
  programs.file-roller.enable = false;
  services.gnome.gnome-online-miners.enable = false;
  programs.gnome-disks.enable = true;
  programs.calls.enable = false;
  programs.kdeconnect.enable = false;
  services.gnome.tracker.enable = false;
  services.gnome.tracker-miners.enable = false;
  services.gnome.sushi.enable = true;
  services.gnome.rygel.enable = false;
  services.gnome.gnome-remote-desktop.enable = false;
  services.gnome.evolution-data-server.enable = false;
}
