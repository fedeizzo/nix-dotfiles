{ pkgs, config, lib, ... }:

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
        "/persist/var/volumes/grafana/plugins"
        "/persist${config.services.postgresqlBackup.location}"
        "/persist${config.services.immich.mediaLocation}/backups"
        "/persist${config.services.immich.mediaLocation}/library"
        "/persist${config.services.immich.mediaLocation}/upload"
        "/persist${config.services.immich.mediaLocation}/profile"
        "/persist${config.services.home-assistant.configDir}"
        "/persist${config.services.mosquitto.dataDir}"
        "/persist${config.services.zigbee2mqtt.dataDir}"
        ## streaming
        "/persist${config.services.jellyfin.dataDir}"
        "/persist${config.services.radarr.dataDir}"
        "/persist${config.services.sonarr.dataDir}"
        "/persist/var/lib/bazarr"
        "/persist/var/lib/private/prowlarr"
        "/persist/var/lib/private/jellyseerr"
        "/persist${config.services.deluge.dataDir}/.config"
        "/persist${config.services.nextcloud.datadir}"
        "/persist${config.services.paperless.dataDir}"
        "/persist/var/lib/authentik/media"
        "/persist${config.services.open-webui.stateDir}/data"
        "/persist/var/lib/paperless-ai"
        "/persist${config.services.uptime-kuma.settings.DATA_DIR}"
        "/persist/var/lib/garmindb"
        "/persist/var/lib/affine"
        "/persist${config.services.calibre-web.options.calibreLibrary}"
        "/persist/var/lib/postgresql"
        "/persist/var/lib/tindeq"

        # files
        "/persist/var/volumes/promtail/GeoLite2-City.mmdb"
        "/persist/var/volumes/grafana/data/grafana.db"
        "/persist/var/volumes/net_worth_nocodb/noco.db"
        "/persist/var/volumes/traefik/acme.json"
        "/persist${config.services.subtrackr.databasePath}"
      ];
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
    };
  };
}
