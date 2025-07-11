{ pkgs-unstable, ... }:

{
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
      };
    };
  };
}
