{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.homeManager.claude = { config, pkgs, lib, inputs, ... }:
    let
      claudeDir = "${config.home.homeDirectory}/.claude";
      statusLine = pkgs.writeShellScript "claude-statusline" ''
        export PATH=${lib.makeBinPath [ pkgs.coreutils pkgs.jq ]}:"$PATH"
        ${builtins.readFile ./config/statusline-command.sh}
      '';
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
      settingsFile = (pkgs.formats.json { }).generate "claude-code-settings.json" settings;
    in
    {
      programs.claude-code = {
        enable = true;
        package = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code;
        skills = builtins.mapAttrs (name: _: ../../../.agents/skills/${name}) (
          lib.filterAttrs (_: type: type == "directory") (builtins.readDir ../../../.agents/skills)
        );
        hooks = {
          "context-mode-cache-heal.mjs" = builtins.readFile ./config/hooks/context-mode-cache-heal.mjs;
          # Kept available without enabling notifications, matching the local settings.
          "notify_ntfy.py" = builtins.readFile ./config/hooks/notify_ntfy.py;
        };
      };

      # Claude Code rewrites settings.json at runtime (/model, /effort, ...), so it
      # can't be a read-only store symlink. Merge the Nix settings into a real,
      # writable file on every switch: Nix-declared keys win, runtime keys persist.
      home.activation.claudeSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        target="${claudeDir}/settings.json"
        if [ -z "''${DRY_RUN:-}" ]; then
          mkdir -p "${claudeDir}"
          [ -L "$target" ] && rm "$target"
          if [ -s "$target" ] && ${pkgs.jq}/bin/jq -e . "$target" >/dev/null 2>&1; then
            ${pkgs.jq}/bin/jq -s '.[0] * .[1]' "$target" ${settingsFile} > "$target.tmp"
          else
            cp ${settingsFile} "$target.tmp"
          fi
          chmod 600 "$target.tmp"
          mv "$target.tmp" "$target"
        else
          echo "Would merge ${settingsFile} into $target"
        fi
      '';
    };
}
