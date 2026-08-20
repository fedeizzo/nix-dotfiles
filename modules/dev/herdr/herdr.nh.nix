{
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  flake.modules.nixos.herdr = {
    fi.services = [
      {
        name = "collie";
        port = 8787;
        dashboardSection = "Tools";
        authType = "none";
      }
    ];
  };

  flake.modules.homeManager.herdr = { pkgs, lib, ... }:
    let
      herdr = pkgs.llm-agents.herdr;

      plugins = [
        "persiyanov/herdr-reviewr"
        "jeffarese/herdr-bar"
        "smarzban/herdr-file-viewer"
        "mroth/herdr-jj-status"
        "AltanS/collie"
      ];

      herdrWithPlugins = pkgs.writeShellApplication {
        name = "herdr";
        runtimeInputs = [
          pkgs.bun
          pkgs.jq
        ];
        text = ''
          if [[ -z "''${HERDR_SKIP_PLUGIN_BOOTSTRAP:-}" ]]; then
            export HERDR_SKIP_PLUGIN_BOOTSTRAP=1

            if installed_plugins="$(${herdr}/bin/herdr plugin list --json)"; then
              for plugin in ${lib.escapeShellArgs plugins}; do
                if ! jq -e --arg repository "$plugin" \
                  'any(.result.plugins[]?; .source.kind == "github" and ((.source.owner + "/" + .source.repo) == $repository))' \
                  >/dev/null <<< "$installed_plugins"; then
                  echo "herdr: installing plugin $plugin..." >&2

                  if ! ${herdr}/bin/herdr plugin install "$plugin" --yes; then
                    echo "herdr: warning: failed to install $plugin; continuing without it" >&2
                  fi
                fi
              done
            else
              echo "herdr: warning: failed to list plugins; skipping plugin bootstrap" >&2
            fi
          fi

          exec ${herdr}/bin/herdr "$@"
        '';
      };

      collie = pkgs.writeShellApplication {
        name = "collie";
        runtimeInputs = [
          pkgs.bun
          pkgs.git
          pkgs.jq
        ];
        text = ''
          config_home="''${XDG_CONFIG_HOME:-$HOME/.config}"
          config_dir="$config_home/herdr/plugins/config/herdr.collie"

          if [[ -f "$config_dir/.env" ]]; then
            set -a
            source "$config_dir/.env"
            set +a
          fi

          export HERDR_PLUGIN_CONFIG_DIR="$config_dir"
          export HERDR_SOCKET_PATH="''${HERDR_SOCKET_PATH:-$config_home/herdr/herdr.sock}"

          plugin_root="$(
            ${herdrWithPlugins}/bin/herdr plugin list --json \
              | jq -er 'first(.result.plugins[]? | select(.plugin_id == "herdr.collie") | .plugin_root)'
          )"

          cd "$plugin_root"
          exec bun run "$plugin_root/bridge/index.ts"
        '';
      };

      herdrUpdatePlugins = pkgs.writeShellApplication {
        name = "herdr-update-plugins";
        runtimeInputs = [
          pkgs.bun
          pkgs.jq
        ];
        text = ''
          installed_plugins="$(${herdr}/bin/herdr plugin list --json)"
          github_plugins="$(
            jq -r '
              .result.plugins[]?
              | select(.source.kind == "github")
              | [.plugin_id, (.source.owner + "/" + .source.repo)]
              | @tsv
            ' <<< "$installed_plugins"
          )"

          if [[ -z "$github_plugins" ]]; then
            echo "herdr: no GitHub plugins installed"
            exit 0
          fi

          failed=0

          while IFS=$'\t' read -r plugin_id repository; do
            echo "herdr: updating $plugin_id from $repository..."

            if ! ${herdr}/bin/herdr plugin uninstall "$plugin_id"; then
              echo "herdr: warning: failed to uninstall $plugin_id" >&2
              failed=1
              continue
            fi

            if ! ${herdr}/bin/herdr plugin install "$repository" --yes; then
              echo "herdr: warning: failed to reinstall $plugin_id" >&2
              failed=1
            fi
          done <<< "$github_plugins"

          exit "$failed"
        '';
      };

      config = (pkgs.formats.toml { }).generate "herdr-config.toml" {
        onboarding = false;

        keys.command = [
          {
            key = "alt+r";
            type = "plugin_action";
            command = "persiyanov.reviewr.toggle";
            description = "review bar";
          }
          {
            key = "alt+t";
            type = "plugin_action";
            command = "herdr-bar.open";
            description = "command bar";
          }
          # Open in a split beside your work.
          {
            key = "alt+f";
            type = "shell";
            command = "herdr plugin action invoke open-file-viewer --plugin herdr-file-viewer";
          }
          # Open in its own tab.
          {
            key = "alt+shift+f";
            type = "shell";
            command = "herdr plugin action invoke open-file-viewer-tab --plugin herdr-file-viewer";
          }
        ];

        ui = {
          agent_panel_sort = "spaces";

          sidebar.spaces.rows = [
            [
              "state_icon"
              "workspace"
            ]
            [
              "branch"
              "git_status"
            ]
            [
              "$jj_bookmark"
              "$jj_status"
            ]
          ];

          sound.enabled = false;
          toast.delivery = "system";
        };
      };
    in
    {
      home.packages = [
        herdrWithPlugins
        herdrUpdatePlugins
        pkgs.glow
        pkgs.delta
        pkgs.bat
      ];

      xdg.configFile = {
        "herdr/config.toml".source = config;
        "herdr/plugins/config/herdr.collie/.env".text = ''
          COLLIE_SKIP_SERVE=1
          COLLIE_PORT=8787
          COLLIE_PUBLIC_HOSTS=collie.fedeizzo.dev
          COLLIE_ALLOWED_ORIGINS=https://collie.fedeizzo.dev
          COLLIE_PUBLIC_URL=https://collie.fedeizzo.dev
        '';
      };

      systemd.user.services.collie = lib.mkIf pkgs.stdenv.isLinux {
        Unit = {
          Description = "Collie Herdr web bridge";
          Wants = [ "network-online.target" ];
          After = [ "network-online.target" ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${collie}/bin/collie";
          Restart = "on-failure";
          RestartSec = 5;
          NoNewPrivileges = true;
          PrivateTmp = true;
        };
        Install.WantedBy = [ "default.target" ];
      };
    };
}
