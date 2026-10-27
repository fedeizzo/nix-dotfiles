{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.claude = { config, pkgs, lib, inputs, ... }:
    let
      claudeDir = "${config.home.homeDirectory}/.claude";
      statusLine = pkgs.writeShellScript "claude-statusline" ''
        export PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.jq ]}:"$PATH"
        ${builtins.readFile ./config/statusline-command.sh}
      '';
    in
    {
      programs.claude-code = {
        enable = true;
        package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
        settings = builtins.fromJSON (builtins.readFile ./config/settings.json) // {
          statusLine = {
            type = "command";
            command = "${statusLine}";
          };
          hooks.SessionStart = [
            {
              hooks = [
                {
                  type = "command";
                  command = "${pkgs.nodejs}/bin/node \"${claudeDir}/hooks/context-mode-cache-heal.mjs\"";
                }
              ];
            }
          ];
        };
        hooks = {
          "context-mode-cache-heal.mjs" = builtins.readFile ./config/hooks/context-mode-cache-heal.mjs;
          # Kept available without enabling notifications, matching the local settings.
          "notify_ntfy.py" = builtins.readFile ./config/hooks/notify_ntfy.py;
        };
      };
    };
}
