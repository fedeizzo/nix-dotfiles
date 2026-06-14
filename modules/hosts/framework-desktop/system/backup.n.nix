{
  flake.modules.nixos.framework-desktop = { pkgs, config, lib, ... }:
    let
      backupFromServices = builtins.concatLists (map (el: el.toBackup) config.fi.services);
    in
    {
    services.restic.backups = rec {
      local = backblaze // {
        repository = "/games/local-restic-backup";
        environmentFile = null;
        runCheck = true;
        passwordFile = config.sops.secrets.local-restic-password.path;
        timerConfig = {
          OnCalendar = "hourly";
          Persistent = true;
          RandomizeDelaySec = "3m";
        };
      };
      backblaze = {
        user = "root";
        initialize = true;
        runCheck = false; # there is already another job with a grafana alert
        environmentFile = config.sops.secrets.backblaze-credentials.path;
        repository = "b2:fedeizzo-homelab-backup";
        passwordFile = config.sops.secrets.restic-password.path;
        createWrapper = true;
        extraBackupArgs = [
          "--compression max"
        ];
        exclude = [
          "**/*.log"
          "**/log/**"
          "**/cache/**"
          "**/.cache/**"
          "**/garth_session"
          "**/GarminConnectConfig.json"
        ];
        paths = [
          # directories
          "/persist/var/container_envs"
          "/persist${config.services.postgresqlBackup.location}"
          ## streaming
          "/persist/var/lib/garmindb"
          "/persist/var/lib/postgresql"
          "/persist/var/lib/tindeq"
          ## observability
          "/persist/var/lib/influxdb"

          # files
          "/persist/var/volumes/promtail/GeoLite2-City.mmdb"
          "/persist/var/volumes/grafana/data/grafana.db"
          "/persist/var/volumes/net_worth_nocodb/noco.db"
          "/persist/var/volumes/traefik/acme.json"
        ] ++ backupFromServices;
        pruneOpts = [
          "--keep-last 30" # keep last 30 days
        ];
      };
    };

    sops.secrets.backblaze-credentials = {
      format = "dotenv";
      mode = "0400";
      sopsFile = ./backblaze-homelab-secrets.env;
      key = ""; # to map the whole file as a secret
    };
  };
}
