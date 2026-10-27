{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.codex = { config, pkgs, inputs, ... }:
    let
      configDir = if config.home.preferXdgDirectories then "${config.xdg.configHome}/codex" else "${config.home.homeDirectory}/.codex";
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
