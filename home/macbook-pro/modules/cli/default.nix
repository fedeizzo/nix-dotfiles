{ pkgs, inputs, ... }:

{
  imports = [ ../../../common/cli/default.nix ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
