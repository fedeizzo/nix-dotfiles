{ config, lib, pkgs, ... }:

let
  cfg = config.services.subtrackr;
  subtrackr-pkg = pkgs.callPackage ./build.nix { };
in
{
  options.services.subtrackr = {
    enable = lib.mkEnableOption "Subtrackr subscription tracker";

    port = lib.mkOption {
      type = lib.types.port;
      default = 8080;
      description = "The port the server will listen on.";
    };

    databasePath = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/subtrackr/subtrackr.db";
      description = "The absolute path to the SQLite database file.";
    };

    fixerApiKeyFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to an EnvironmentFile containing the FIXER_API_KEY.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.subtrackr = {
      description = "Subtrackr: Self-hosted Subscription Tracker";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${subtrackr-pkg}/bin/subtrackr";

        # StateDirectory creates /var/lib/subtrackr if the default path is used.
        # If the user provides a custom path, they are responsible for directory permissions.
        StateDirectory = lib.mkIf (lib.hasPrefix "/var/lib/subtrackr" cfg.databasePath) "subtrackr";

        DynamicUser = true;

        # Environment variables
        Environment = [
          "PORT=${toString cfg.port}"
          "DATABASE_PATH=${cfg.databasePath}"
          "GIN_MODE=release"
        ];

        EnvironmentFile = lib.optional (cfg.fixerApiKeyFile != null) cfg.fixerApiKeyFile;

        # Hardening
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];

        # Restart settings
        Restart = "always";
        RestartSec = "5s";

        # If using a custom path outside /var/lib, we must allow the service to write there
        ReadWritePaths = [ (dirOf cfg.databasePath) ];
      };
    };
  };
}
