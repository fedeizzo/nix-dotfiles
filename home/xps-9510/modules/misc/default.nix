{ pkgs, pkgs-unstable, ... }:

{
  home.packages = with pkgs; [
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
    # fonts
    joypixels
    (nerdfonts.override {
      fonts = [
        "Meslo"
        "RobotoMono"
      ];
    })

    pavucontrol
    vlc
    gimp
    pkgs-unstable.calibre
    yubikey-manager
    yubikey-manager-qt
    pkgs-unstable.bitwarden
    pkgs-unstable.tdesktop
  ];

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
