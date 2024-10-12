{ pkgs, ... }:

{

  home.packages = with pkgs; [
    rbw
    rofi-rbw
    wtype
  ];
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    theme = ./nord.rasi;
    terminal = "${pkgs.kitty}/bin/kitty";
    extraConfig = {
      modi = "drun";
    };
  };
  xdg.configFile."rofi-rbw.rc" = {
    source = ./rofi-rbw.rc;
  };
}
