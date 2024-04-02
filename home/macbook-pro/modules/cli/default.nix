{ pkgs, inputs, ... }:

{
  imports = [ ../../../common/cli/default.nix ];
  home.packages = with pkgs; [
    marp-cli # markdowm to slides
  ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
