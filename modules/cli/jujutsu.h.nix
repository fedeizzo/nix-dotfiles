{
  flake.modules.homeManager.jujutsu = { pkgs, lib, pkgs-unstable ? pkgs, ... }: {
    home.packages = [ pkgs.mergiraf ];
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
          merge-editor = "mergiraf";
          diff-formatter = ":git";
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
          push = [
            "util"
            "exec"
            "--"
            "bash"
            "-c"
            "jj diff -r @ --name-only --no-pager | xargs pre-commit run --files && jj git push"
          ];
          fnew = [ "util" "exec" "--" "sh" "-c" "jj git fetch -b $1 && jj new $1" "--" ];
          bct = [ "util" "exec" "--" "sh" "-c" "jj b c $1 && jj b t $1" "--" ];
          mine = [ "log" "-r" "mine()" "-n" "10" ];
          sp = [ "util" "exec" "--" "sh" "-c" "jj squash && jj git push" ];
          bmp = [ "util" "exec" "--" "sh" "-c" "jj b m $1 && jj git push" "--" ];
          fetch-main = [ "util" "exec" "--" "sh" "-c" "jj git fetch -b main && jj b l main" ];
          bml = [ "util" "exec" "--" "sh" "-c" "jj b m $1 --to @- --allow-backwards" "--" ];
        };
      };
    };
  };
}
