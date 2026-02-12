{ config, ... }:

{
  environment.persistence."/persist" = {
    hideMounts = true;

    directories = [
      # ─────────────────────────────
      # System identity / config
      # ─────────────────────────────
      "/etc/nixos"
      "/etc/ssh"
      "/var/lib/nixos"
      "/var/lib/systemd/linger"
      "/var/lib/systemd/timers"


      # Network (if using NetworkManager)
      "/etc/NetworkManager/system-connections"
      "/var/lib/NetworkManager"

      # Root user state
      "/root/.ssh"
      "/root/.cache"
      "/root/.local"

      # ─────────────────────────────
      # Container / runtime (persist but don't backup)
      # ─────────────────────────────
      "/var/lib/containers"
      "/var/lib/docker"
      "/var/lib/cni"

      # Optional but often useful
      "/var/cache"

      # ─────────────────────────────
      # Homelab platform state (also backed up)
      # ─────────────────────────────
      "/var/container_envs"
      "/var/volumes"

      config.services.postgresqlBackup.location

      config.services.home-assistant.configDir
      config.services.mosquitto.dataDir
      config.services.zigbee2mqtt.dataDir

      config.services.jellyfin.dataDir
      config.services.radarr.dataDir
      config.services.sonarr.dataDir
      "/var/lib/bazarr"
      "/var/lib/private/prowlarr"
      "/var/lib/private/jellyseerr"
      "${config.services.deluge.dataDir}/.config"

      config.services.nextcloud.datadir
      config.services.paperless.dataDir
      "/var/lib/paperless-ai"
      "/var/lib/authentik/media"
      "${config.services.open-webui.stateDir}/data"
      config.services.uptime-kuma.settings.DATA_DIR
      "/var/lib/garmindb"
      "/var/lib/affine"
      config.services.calibre-web.options.calibreLibrary
      "/var/lib/fail2ban"
      "/var/lib/fwupd"
      "/var/lib/garmin-fetch-data"
      config.services.immich.mediaLocation
      "/var/lib/influxdb"
      "/var/lib/postgresql"
      "/var/lib/tindeq"

      "/var/lib/redis-affine"
      "/var/lib/redis-authentik"
      "/var/lib/redis-immich"
      "/var/lib/redis-nextcloud"
      "/var/lib/redis-paperless"
    ];

    files = [
      # System identity / config
      "/root/.bash_history"

      "/etc/machine-id"
      "/etc/adjtime"

      # App single-file state
      # "/var/volumes/promtail/GeoLite2-City.mmdb"
      # "/var/volumes/grafana/data/grafana.db"
      # "/var/volumes/net_worth_nocodb/noco.db"
      # "/var/volumes/traefik/acme.json"
      config.services.subtrackr.databasePath
      "/var/lib/private/dns-updater/cache.json"
      "/var/lib/logrotate.status"
    ];
  };
}
