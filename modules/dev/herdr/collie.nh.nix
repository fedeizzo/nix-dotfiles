# Collie: web bridge for herdr, served on homelab (collie) and forge (collief).
# Traefik on homelab is the single entry point for both.
{
  flake.modules.nixos.collie = {
    fi.services = [
      {
        name = "collie";
        port = 8787;
        dashboardSection = "Tools";
        authType = "none";
      }
      {
        name = "collief";
        dashboardIcon = "collie";
        host = "192.168.1.89"; # forge
        port = 8787;
        dashboardSection = "Tools";
        authType = "none";
      }
    ];
  };

  flake.modules.homeManager.collie = { pkgs, lib, config, ... }:
    let
      cfg = config.fi.collie;

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
            # shellcheck disable=SC1091
            source "$config_dir/.env"
            set +a
          fi

          export HERDR_PLUGIN_CONFIG_DIR="$config_dir"
          export HERDR_SOCKET_PATH="''${HERDR_SOCKET_PATH:-$config_home/herdr/herdr.sock}"

          plugin_root="$(
            ${config.fi.herdr.package}/bin/herdr plugin list --json \
              | jq -er 'first(.result.plugins[]? | select(.plugin_id == "herdr.collie") | .plugin_root)'
          )"

          cd "$plugin_root"
          exec bun run "$plugin_root/bridge/index.ts"
        '';
      };
    in
    {
      options.fi.collie = {
        publicHost = lib.mkOption {
          type = lib.types.str;
          default = "collie.fedeizzo.dev";
        };
        bindHost = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "Non-loopback address to listen on, for hosts proxied by homelab's traefik.";
        };
      };

      config = {
        fi.herdr.plugins = [ "AltanS/collie" ];

        xdg.configFile."herdr/plugins/config/herdr.collie/.env".text = ''
          COLLIE_SKIP_SERVE=1
          COLLIE_PORT=8787
          COLLIE_PUBLIC_HOSTS=${cfg.publicHost}
          COLLIE_ALLOWED_ORIGINS=https://${cfg.publicHost}
          COLLIE_PUBLIC_URL=https://${cfg.publicHost}
        '' + lib.optionalString (cfg.bindHost != null) ''
          COLLIE_HOST=${cfg.bindHost}
          COLLIE_ALLOW_NON_LOOPBACK_BIND=1
        '';

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
    };
}
