{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/default.nix
    ../../../common/jujutsu
  ];
  home.packages = with pkgs; [
    marp-cli # markdowm to slides
  ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.jujutsu.settings.user.email = "federico.izzo@datadoghq.com";
}
