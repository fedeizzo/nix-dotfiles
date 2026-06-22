{
  flake.modules.homeManager.profile-work = { pkgs, ... }: {
    programs.jujutsu.settings = {
      aliases.push = [
        "util"
        "exec"
        "--"
        "bash"
        "-c"
        "bzl test //tools/format:format_go_check && jj git push"
      ];
      user.email = "federico.izzo@datadoghq.com";
      fsmonitor = {
        "watchman.register-snapshot-trigger" = true;
        backend = "watchman";
      };
      remotes.origin = {
        push-new-bookmarks = true;
        auto-track-bookmarks = "fedeizzo/*";
      };
    };
    home.packages = [ pkgs.watchman ];
  };
}
