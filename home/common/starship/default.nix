{ pkgs, config, lib, ... }:

{
  config = {
    programs.starship = {
      enable = true;
      enableFishIntegration = true;
      settings = {
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
    };
  };
}
