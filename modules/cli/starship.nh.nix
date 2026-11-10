let
  sharedSettings = {
    add_newline = false;
    character = {
      success_symbol = "[λ](green)";
      vicmd_symbol = "[V](green)";
      error_symbol = "[✖](red)";
    };
    package.disabled = true;
    directory.truncation_length = 8;
    cmd_duration = {
      min_time = 20000;
      format = "  [$duration](bold yellow)";
    };
    kubernetes = {
      disabled = false;
      symbol = "󱃾";
    };
  };
in
{
  flake.modules.homeManager.starship = { config, lib, pkgs, ... }:
    let
      cfg = config.programs.starship;
      starship = lib.getExe cfg.package;
      configFile = (pkgs.formats.toml { }).generate "starship.toml" cfg.settings;
      # Pre-generated full init: avoids `starship init` + `starship prompt
      # --continuation` execs on every shell start, and drops the RPROMPT exec
      # on every prompt when no right_format is configured.
      zshInit = pkgs.runCommand "starship-init.zsh" { } ''
        export HOME=$TMPDIR STARSHIP_CONFIG=${configFile} STARSHIP_SHELL=zsh
        ${starship} init zsh --print-full-init \
          | sed -e '/^PROMPT2=/d' ${lib.optionalString (!(cfg.settings ? right_format)) "-e '/^RPROMPT=/d'"} > $out
        printf 'PROMPT2=%q\n' "$(${starship} prompt --continuation)" >> $out
      '';
    in
    {
      programs.starship = {
        enable = true;
        enableFishIntegration = true;
        enableZshIntegration = false; # sourced pre-generated below
        settings = sharedSettings;
      };
      programs.zsh.initContent = ''
        if [[ $TERM != "dumb" ]]; then
          source ${zshInit}
        fi
      '';
    };

  flake.modules.nixos.starship = { ... }: {
    programs.starship = {
      enable = true;
      interactiveOnly = true;
      settings = sharedSettings;
    };
  };
}
