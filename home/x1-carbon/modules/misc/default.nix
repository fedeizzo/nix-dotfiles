{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # blueberry
    # (makeDesktopItem {
    #   name = "bluetooth";
    #   exec = "blueberry";
    #   terminal = false;
    #   desktopName = "bluetooth";
    #   type = "Application";
    #   categories = [ "GTK" "GNOME" "Settings" "HardwareSettings" "X-XFCE-SettingsDialog" "X-XFCE-HardwareSettings" ];
    #   icon = "bluetooth";
    # })
    pavucontrol
    vlc
    bitwarden
    tdesktop
  ];
}
