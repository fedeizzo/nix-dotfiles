{  config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    slack
    unstable.tdesktop
    zoom-us
    discord
  ];
}
