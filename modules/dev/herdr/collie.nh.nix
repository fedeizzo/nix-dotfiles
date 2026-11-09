# Collie: web bridge for Herdr-core, served on homelab (collie) and forge (collief).
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

  flake.modules.homeManager.collie = { pkgs, lib, config, inputs, ... }:
    let
      cfg = config.fi.collie;

      collieSrc = pkgs.fetchFromGitHub {
        owner = "AltanS";
        repo = "collie";
        rev = "b178eb63f2255637f6326bb942a2505e2004c31f";
        hash = "sha256-CqP7fCXzzp+IKx/UycMM2Lpd64Fc8dxEbZhKVEWDQPY=";
      };
      collie = pkgs.callPackage "${collieSrc}/packaging/nix/collie.nix" { };
    in
    {
      options.fi.collie = {
        modSource = lib.mkOption {
          type = lib.types.path;
          default = "${collieSrc}/herdr-app-mod/collie";
          readOnly = true;
          description = "Collie panel for Herdr-app.";
        };
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
        home.packages = [
          collie
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.herdr-core
        ];

        xdg.configFile."collie/.env".text = ''
          COLLIE_MUX=herdr-core
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
            ExecStart = "${collie}/bin/collie _exec-bridge";
            Environment = [ "HERDR_PLUGIN_CONFIG_DIR=${config.xdg.configHome}/collie" ];
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
