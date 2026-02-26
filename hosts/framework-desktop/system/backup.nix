{ pkgs, config, lib, ... }:

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
      # Env file structure
      # B2_ACCOUNT_ID=
      # B2_ACCOUNT_KEY=
      environmentFile = "/root/.restic_backup_env";
      repository = "b2:fedeizzo-homelab-backup";
      # repositoryFile = config.sops.secrets.restic-repository.path;
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

  environment.systemPackages = [ pkgs.backrest ];
  systemd.services.backrest = {
    description = "Backrest";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      User = "root";
      ExecStart = lib.getExe pkgs.backrest;
    };
    environment = {
      BACKREST_PORT = "0.0.0.0:9898";
      BACKREST_CONFIG = "/root/.config/backrest/config.json";
      BACKREST_DATA = "/root/.local/backrest";
      XDG_CONFIG_HOME = "/root/.config";
      XDG_DATA_HOME = "/root/.local/share";
      XDG_CACHE_HOME = "/root/.cache";
    };
  };
}
