{
  flake.modules.homeManager.profile-work = { pkgs, ... }: {
    programs.jujutsu.settings.user.email = "federico.izzo@datadoghq.com";
    programs.jujutsu.settings.fsmonitor = {
      "watchman.register-snapshot-trigger" = true;
      backend = "watchman";
    };
    home.packages = [ pkgs.watchman ];
  };
}
