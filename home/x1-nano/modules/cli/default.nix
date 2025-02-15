{ pkgs, ... }:

{
  imports = [ ../../../common/cli/linux ];

  home.packages = [
    pkgs.gcc
    pkgs.jujutsu
  ];
}
