{ pkgs, lib, ... }:

{
  imports = [
    ../../../common/firefox
  ];

  programs.firefox.package = lib.mkDefault null;
}
