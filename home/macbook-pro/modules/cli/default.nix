{ pkgs, ... }:

{
  imports = [
    ../../../common/cli/default.nix
    ../../../common/jujutsu
  ];
  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    jujutsu.settings.user.email = "federico.izzo@datadoghq.com";
    jujutsu.settings.core = {
      fsmonitor = "watchman";
      watchman.register-snapshot-trigger = true;
    };
  };

  home.packages = [ pkgs.watchman ];
}
