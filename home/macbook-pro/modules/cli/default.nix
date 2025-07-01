{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/default.nix
    ../../../common/jujutsu
  ];
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.jujutsu.settings.user.email = "federico.izzo@datadoghq.com";
  programs.jujutsu.settings.core = {
    fsmonitor = "watchman";
    watchman.register-snapshot-trigger = true;
  };

  home.packages = [ pkgs.watchman pkgs.gg ];
}
