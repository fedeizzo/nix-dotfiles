{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    slack
    tdesktop
  ];
}

