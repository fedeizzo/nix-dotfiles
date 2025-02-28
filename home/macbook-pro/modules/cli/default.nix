{ pkgs, ... }:

{
  imports = [ ../../../common/cli/default.nix ];
  home.packages = with pkgs; [
    marp-cli # markdowm to slides
    jujutsu
  ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };
}
