{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.codex = { config, pkgs, lib, inputs, ... }:
    let
      configDir = if config.home.preferXdgDirectories then "${config.xdg.configHome}/codex" else "${config.home.homeDirectory}/.codex";
      configRelativeDir = lib.removePrefix "${config.home.homeDirectory}/" configDir;
      python = pkgs.python3.withPackages (ps: [ ps.tomlkit ]);
      defaults = pkgs.writeText "codex-defaults.json" (builtins.toJSON config.programs.codex.settings);
    in
    {
      programs.codex = {
        enable = true;
        package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.codex;
        settings = {
          model = "gpt-6.1-sol";
          model_reasoning_effort = "low";
          approval_policy = "never";
          approvals_reviewer = "auto_review";
          dangerously_bypass_approvals_and_sandbox = true;
          sandbox_mode = "danger-full-access";
          sandbox_workspace_write.network_access = true;
          tools.web_search = true;

          tui = {
            status_line = [
              "model-with-reasoning"
              "current-dir"
              "context-remaining"
              "five-hour-limit"
              "weekly-limit"
              "context-window-size"
              "used-tokens"
              "total-input-tokens"
              "total-output-tokens"
              "task-progress"
            ];
            status_line_use_colors = true;
          };
          notice.hide_rate_limit_model_nudge = true;
          features = {
            plugin_hooks = true;
            hooks = true;
          };
          # Preserve the existing disabled notification hook without a machine-specific trust hash.
          hooks.state."${configDir}/hooks.json:stop:0:0".enabled = false;
        };
        rules.default = ./config/default.rules;
      };

      # Codex persists project trust and other runtime preferences in this file.
      home.file."${configRelativeDir}/config.toml".enable = lib.mkForce false;
      home.activation.codexConfig = lib.hm.dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
        run ${python}/bin/python3 ${./config/merge-settings.py} \
          ${lib.escapeShellArg "${configDir}/config.toml"} ${defaults}
      '';

      home.file = {
        "${configDir}/hooks/notify_ntfy.py".source = ./config/hooks/notify_ntfy.py;
        "${configDir}/hooks.json".text = builtins.toJSON {
          description = "Notify ntfy when Codex finishes a turn.";
          hooks.Stop = [
            {
              hooks = [
                {
                  type = "command";
                  command = "${pkgs.python3}/bin/python3 \"${configDir}/hooks/notify_ntfy.py\"";
                  statusMessage = "Notifying ntfy";
                  timeout = 5;
                }
              ];
            }
          ];
        };
      };
    };
}
