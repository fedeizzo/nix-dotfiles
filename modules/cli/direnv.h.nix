{
  flake.modules.homeManager.direnv = { config, lib, pkgs, ... }:
    let
      # Pre-generated `direnv hook zsh` (avoids an exec on every shell start).
      zshHook = pkgs.runCommand "direnv-hook.zsh" { } ''
        HOME=$TMPDIR ${lib.getExe config.programs.direnv.package} hook zsh > $out
      '';
    in
    {
      programs.direnv = {
        enable = true;
        enableZshIntegration = false; # sourced pre-generated below
        nix-direnv.enable = true;
      };
      programs.zsh.initContent = ''
        source ${zshHook}
      '';
    };
}
