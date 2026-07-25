{
  flake.modules.nixos.dawarich = { inputs, pkgs, config, lib, pkgs-unstable ? pkgs, ... }: {
    services.dawarich = {
      enable = true;

      localDomain = "timeline.fedeizzo.dev";
      webPort = 55224;

      user = "dawarich";
      group = "dawarich";

      database = {
        name = "dawarich";
        user = "dawarich";
        host = "127.0.0.1";
        passwordFile = config.sops.secrets.dawarich-postgres-password.path;
        createLocally = true;
      };

      secretKeyBaseFile = config.sops.secrets.secret-key-base.path;

      configureNginx = false;

      environment = {
        PHOTON_API_HOST = "app.chibigeo.com/v1/photon";
        PHOTON_API_USE_HTTPS = "true";
      };
      extraEnvFiles = [
        config.sops.secrets.dawarich-env-secrets.path
      ];
    };

    sops.secrets.dawarich-postgres-password = lib.mkIf config.services.dawarich.enable {
      format = "yaml";
      mode = "0400";
      owner = config.users.users.dawarich.name;
      group = config.users.groups.dawarich.name;
      # restartUnits = [ "dawarich.service" ];
      sopsFile = ./dawarich-homelab-secrets.yaml;
    };

    sops.secrets.secret-key-base = lib.mkIf config.services.dawarich.enable {
      format = "yaml";
      mode = "0400";
      owner = config.users.users.dawarich.name;
      group = config.users.groups.dawarich.name;
      # restartUnits = [ "dawarich.service" ];
      sopsFile = ./dawarich-homelab-secrets.yaml;
    };

    sops.secrets.dawarich-env-secrets = lib.mkIf config.services.dawarich.enable {
      format = "yaml";
      mode = "0400";
      owner = config.users.users.dawarich.name;
      group = config.users.groups.dawarich.name;
      restartUnits = [ "dawarich.service" ];
      sopsFile = ./dawarich-homelab-secrets.yaml;
    };

    sops.secrets.garmin-sync-env = lib.mkIf config.services.dawarich.enable {
      format = "yaml";
      mode = "0400";
      owner = config.users.users.dawarich.name;
      group = config.users.groups.dawarich.name;
      sopsFile = ./dawarich-homelab-secrets.yaml;
    };

    systemd.services.garmin-to-dawarich =
      let
        pythonEnv = pkgs-unstable.python3.withPackages (ps: with ps; [ garminconnect requests curl-cffi ]);
      in
      lib.mkIf config.services.dawarich.enable {
        description = "Garmin Connect to Dawarich Sync Service";
        after = [ "network.target" "dawarich.service" ];
        wants = [ "network-online.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pythonEnv}/bin/python ${./garmin_sync.py}";
          User = config.users.users.dawarich.name;
          Group = config.users.groups.dawarich.name;
          StateDirectory = "garmin-sync";
          EnvironmentFile = config.sops.secrets.garmin-sync-env.path;
        };
      };

    systemd.timers.garmin-to-dawarich = lib.mkIf config.services.dawarich.enable {
      description = "Daily Garmin Connect to Dawarich Sync Timer";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "03:00";
        Persistent = true;
      };
    };

    fi.services = [
      {
        name = "dawarich";
        shouldMonitorUptime = true;
        subdomain = "timeline";
        port = config.services.dawarich.webPort;
        dashboardSection = "Personal";
        toPersist = [
          {
            directory = "/var/lib/dawarich";
            user = "sparkyfitnes";
            group = "sparkyfitnes";
            mode = "u=rwx,g=rx,o=";
          }
          {
            directory = "/var/lib/garmin-sync";
            user = config.users.users.dawarich.name;
            group = config.users.groups.dawarich.name;
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/dawarich"
          "/persist/var/lib/garmin-sync"
        ];
      }
    ];

    users.users.dawarich = {
      uid = 974;
      group = "dawarich";
    };
    users.groups.dawarich.gid = 974;
  };
}
