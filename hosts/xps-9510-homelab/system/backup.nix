{ config, ... }:

{
  services.restic.backups = {
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
      ];
      paths = [
        # directories
        "/var/container_envs"
        "/var/volumes/grafana/plugins"
        "${config.services.sftpgo.dataDir}"
        "${config.services.postgresqlBackup.location}"
        "${config.services.immich.mediaLocation}/library"
        "${config.services.immich.mediaLocation}/upload"
        "${config.services.immich.mediaLocation}/profile"
        "${config.services.home-assistant.configDir}"
        "${config.services.mosquitto.dataDir}"
        "${config.services.zigbee2mqtt.dataDir}"
        ## streaming
        "${config.services.jellyfin.dataDir}"
        "${config.services.radarr.dataDir}"
        "${config.services.sonarr.dataDir}"
        "/var/lib/bazarr"
        "/var/lib/private/prowlarr"
        "/var/lib/private/jellyseerr"
        "${config.services.deluge.dataDir}/.config"
        "${config.services.nextcloud.datadir}"
        "${config.services.paperless.dataDir}"

        # files
        "/var/volumes/promtail/GeoLite2-City.mmdb"
        "/var/volumes/grafana/data/grafana.db"
        "/var/volumes/net_worth_nocodb/noco.db"
        "/var/volumes/traefik/acme.json"
      ];
      pruneOpts = [
        "--keep-last 30" # keep last 30 days
      ];
    };
  };
}
