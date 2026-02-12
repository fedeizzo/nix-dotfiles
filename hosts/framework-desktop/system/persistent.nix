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
      {
        directory = "/etc/NetworkManager/system-connections";
        mode = "u=rwx,g=,o="; # 0700
      }
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

      # Home automation services
      {
        directory = config.services.home-assistant.configDir;
        user = "hass";
        group = "hass";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = config.services.mosquitto.dataDir;
        user = "mosquitto";
        group = "mosquitto";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = config.services.zigbee2mqtt.dataDir;
        user = "zigbee2mqtt";
        group = "zigbee2mqtt";
        mode = "u=rwx,g=,o=";
      }

      # Media apps
      {
        directory = config.services.jellyfin.dataDir;
        user = "media";
        group = "media";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = config.services.radarr.dataDir;
        user = "media";
        group = "media";
        mode = "u=rwx,g=rx,o=rx";
      }
      {
        directory = config.services.sonarr.dataDir;
        user = "media";
        group = "media";
        mode = "u=rwx,g=rx,o=rx";
      }
      {
        directory = "/var/lib/bazarr";
        user = "media";
        group = "media";
        mode = "u=rwx,g=rx,o=rx";
      }
      {
        directory = "/var/lib/private/prowlarr";
        user = "prowlarr";
        group = "prowlarr";
        mode = "u=rwx,g=rx,o=rx";
      }
      {
        directory = "/var/lib/private/jellyseerr";
        user = "jellyseerr";
        group = "jellyseerr";
        mode = "u=rwx,g=rx,o=rx";
      }

      # Nextcloud / Paperless / Open-WebUI
      {
        directory = config.services.nextcloud.datadir;
        user = "nextcloud";
        group = "nextcloud";
        mode = "u=rwx,g=rx,o=";
      }
      {
        directory = config.services.paperless.dataDir;
        user = "paperless";
        group = "paperless";
        mode = "u=rwx,g=rx,o=rx";
      }
      "/var/lib/paperless-ai"
      {
        directory = "/var/lib/authentik/media";
        user = "nobody";
        group = "nogroup";
        mode = "u=rwx,g=rx,o=rx";
      }
      {
        directory = "${config.services.open-webui.stateDir}/data";
        user = "nobody";
        group = "nogroup";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = config.services.uptime-kuma.settings.DATA_DIR;
        user = "nobody";
        group = "nogroup";
        mode = "u=rwx,g=rx,o=";
      }
      {
        directory = "/var/lib/garmindb";
        user = "garmindb";
        group = "garmindb";
        mode = "u=rwx,g=rx,o=rx";
      }
      "/var/lib/affine"
      {
        directory = config.services.calibre-web.options.calibreLibrary;
        user = "calibre-server";
        group = "calibre-server";
        mode = "u=rwx,g=rx,o=rx";
      }

      # System / root services
      "/var/lib/fail2ban"
      "/var/lib/fwupd"
      "/var/lib/garmin-fetch-data"

      # Immich (pinned UID/GID)
      {
        directory = config.services.immich.mediaLocation;
        user = "immich";
        group = "immich";
        mode = "u=rwx,g=rx,o=";
      }

      # DB services
      {
        directory = "/var/lib/influxdb";
        user = "influxdb";
        group = "influxdb";
        mode = "u=rwx,g=rwx,o=";
      }
      {
        directory = "/var/lib/postgresql";
        user = "postgres";
        group = "postgres";
        mode = "u=rwx,g=rx,o=";
      }

      {
        directory = "/var/lib/tindeq";
        user = "nextcloud";
        group = "nextcloud";
        mode = "u=rwx,g=rx,o=rx";
      }

      # Redis (persist but no backup)
      {
        directory = "/var/lib/redis-affine";
        user = "redis-affine";
        group = "redis-affine";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = "/var/lib/redis-authentik";
        user = "nobody";
        group = "nogroup";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = "/var/lib/redis-immich";
        user = "redis-immich";
        group = "redis-immich";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = "/var/lib/redis-nextcloud";
        user = "nextcloud";
        group = "nextcloud";
        mode = "u=rwx,g=,o=";
      }
      {
        directory = "/var/lib/redis-paperless";
        user = "redis-paperless";
        group = "redis-paperless";
        mode = "u=rwx,g=,o=";
      }
    ];

    files = [
      # System identity / config
      "/root/.bash_history"
      "/etc/machine-id"
      "/etc/adjtime"

      # App single-file state
      config.services.subtrackr.databasePath
      "/var/lib/private/dns-updater/cache.json"
      "/var/lib/logrotate.status"
    ];
  };
}
