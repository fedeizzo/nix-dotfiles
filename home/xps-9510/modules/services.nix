{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    networkmanager_dmenu
    blueberry
    (makeDesktopItem {
      name = "bluetooth";
      exec = "blueberry";
      terminal = false;
      desktopName = "bluetooth";
      type = "Application";
      categories = [ "GTK" "GNOME" "Settings" "HardwareSettings" "X-XFCE-SettingsDialog" "X-XFCE-HardwareSettings" ];
      icon = "bluetooth";
    })
  ];
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  services.lorri.enable = false;
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
