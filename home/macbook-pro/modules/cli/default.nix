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

    jujutsu.settings = {
      user.email = "federico.izzo@datadoghq.com";
      fsmonitor = {
        watchman.register-snapshot-trigger = true;
        backend = "watchman";
      };
    };
  };
  
  home.packages = [ pkgs.watchman ];
}
