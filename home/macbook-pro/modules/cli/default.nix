{ pkgs, ... }:

{
  imports = [ ../../../common/cli/default.nix ];
  home.packages = [ pkgs.direnv ];
}
