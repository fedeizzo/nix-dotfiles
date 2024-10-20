{ inputs, pkgs, ... }:

{
  programs.yazi = {
    enable = true;
    package = inputs.yazi.packages.${pkgs.system}.default;
  };
}
