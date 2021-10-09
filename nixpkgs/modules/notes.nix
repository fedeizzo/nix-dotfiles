{ config, pkgs, libs, ... }:

{
  home.packages = with pkgs; [
    obsidian
    zotero
    anki
    inkscape
  ];
}
