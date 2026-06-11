{
  flake.modules.homeManager.jujutsu = { pkgs, lib, pkgs-unstable ? pkgs, ... }: {
    programs.jujutsu = {
      enable = true;
      package = pkgs-unstable.jujutsu;
      settings = {
        user = {
          name = "Federico Izzo";
        };
        ui = {
          paginate = "never";
          default-command = "status";
        };
        aliases = {
          pre-commit = [
            "util"
            "exec"
            "--"
            "bash"
            "-c"
            "jj diff -r @ --name-only --no-pager | xargs pre-commit run --files"
          ];
          fnew = [ "sh" "-c" "jj git fetch -b $1 && jj new $1" "--" ];
          bct = [ "sh" "-c" "jj b c $1 && jj b t $1" "--" ];
          mine = [ "log" "-r" "mine()" "-n" "10" ];
          sp = [ "sh" "-c" "jj squash && jj git push" "--" ];
          bmp = [ "sh" "-c" "jj b m $1 && jj git push" "--" ];
          fetch-main = [ "sh" "-c" "jj git fetch -b main && jj b l main" "--" ];
          bml = [ "sh" "-c" "jj b m $1 --to @- --allow-backwards" "--" ];
        };
      };
    };
  };
}
