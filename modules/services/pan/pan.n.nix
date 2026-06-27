{
  flake.modules.nixos.pan = { config, lib, pkgs, ... }:
  let
    cfg = config.services.pan;
    format = pkgs.formats.yaml { };

    panPackage = pkgs.buildGoModule {
      pname = "pan";
      version = "0.1.0";
      src = lib.cleanSource ./.;
      
      vendorHash = "sha256-73qCTSkQn+aKFyBxyig4rTEQEpOyGYrqT4BM1B1nKrk=";
    };
  in {
    options.services.pan = {
      enable = lib.mkEnableOption "Pan service";

      package = lib.mkOption {
        type = lib.types.package;
        default = panPackage;
        description = "The pan package to use.";
      };

      settings = lib.mkOption {
        type = format.type;
        default = { };
        description = ''
          Configuration for Pan, written to config.yaml.
        '';
      };

      dataDir = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/pan";
        description = "Data directory for Pan, used as the working directory.";
      };

      environmentFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          Path to an environment file containing secrets like
          OPENAI_API_KEY, MATRIX_PASSWORD_FILE, or FASTMAIL_API_FILE.
        '';
      };
    };

    config = lib.mkIf cfg.enable {
      systemd.services.pan = {
        description = "Pan Service";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          ExecStart = "${cfg.package}/bin/pan";
          WorkingDirectory = cfg.dataDir;
          StateDirectory = "pan";
          User = "pan";
          Group = "pan";
          Restart = "on-failure";
          EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;
        };

        preStart = ''
          cp ${format.generate "pan-config.yaml" cfg.settings} ${cfg.dataDir}/config.yaml
          chmod 600 ${cfg.dataDir}/config.yaml
        '';
      };

      users.users.pan = {
        isSystemUser = true;
        group = "pan";
        home = cfg.dataDir;
      };

      users.groups.pan = { };

      fi.services = [
        {
          name = "pan";
          dashboardSection = "Services";
          toPersist = [
            {
              directory = cfg.dataDir;
              user = "pan";
              group = "pan";
              mode = "u=rwx,g=,o=";
            }
          ];
          toBackup = [
            "/persist${cfg.dataDir}"
          ];
        }
      ];
    };
  };
}
