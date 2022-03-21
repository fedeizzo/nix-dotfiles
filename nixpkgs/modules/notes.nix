{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    zotero
    anki
    inkscape
  ];
}
