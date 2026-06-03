{ lib, config, ... }:

{
  imports = [
    ./module.nix
    ./users.nix

    ./affine
    ./authentik
    ./blocky
    ./calibre
    ./climbing-lab
    ./docs
    ./fedeizzo.dev
    ./fusion
    ./garmindb
    ./gatus
    ./glance
    ./grafana
    ./hass
    ./hermes
    ./immich
    ./influxdb
    ./lemonade
    ./llama-swap
    ./logrotate
    # ./loki
    ./net-worth
    ./n8n
    ./neo4j
    ./nextcloud
    ./ntfy
    ./open-webui
    # ./opencloud
    ./paperless
    ./postgres
    ./prometheus
    ./searx
    # ./sunshine
    ./streaming
    ./traefik
  ];

  fi.services = [
    # Exposed
    { name = "fedeizzodev"; subdomain = null; port = 50001; isExposed = true; dashboardSection = "Exposed"; dashboardIcon = "hugo"; }
    { name = "nix-dotfiles"; port = 34999; isExposed = true; dashboardSection = "Exposed"; dashboardIcon = "mkdocs"; }

    # Exposed media
    {
      name = "jellyfin";
      port = 8096;
      isExposed = true;
      dashboardSection = "Exposed media";
      toPersist = [
        {
          directory = config.services.jellyfin.dataDir;
          user = "media";
          group = "media";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist${config.services.jellyfin.dataDir}"
      ];
    }
    {
      name = "jellyseerr";
      port = 5055;
      isExposed = true;
      dashboardSection = "Exposed media";
      toPersist = [
        {
          directory = "/var/lib/private/jellyseerr";
          user = "jellyseerr";
          group = "jellyseerr";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist/var/lib/private/jellyseerr"
      ];
    }
    {
      name = "nextcloud";
      port = 8180;
      isExposed = true;
      dashboardSection = "Exposed media";
      toPersist = [
        {
          directory = config.services.nextcloud.datadir;
          user = "nextcloud";
          group = "nextcloud";
          mode = "u=rwx,g=rx,o=";
        }
      ];
      toBackup = [
        "/persist${config.services.nextcloud.datadir}"
      ];
    }
    {
      name = "collabora"; inherit (config.services.collabora-online) port; isExposed = true;
      dashboardSection = "Exposed media";
      dashboardIcon = "collabora-online";
    }
    {
      name = "affine";
      port = 3010;
      isExposed = true;
      dashboardSection = "Exposed media";
      toPersist = [
        {
          directory = "/var/lib/affine";
          user = "root";
          group = "root";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/affine"
        "/persist/var/lib/redis-affine"
      ];
    }

    # Media
    {
      name = "immich";
      subdomain = "photo"; inherit (config.services.immich) port; dashboardSection = "Media";
      toPersist = [
        {
          directory = config.services.immich.mediaLocation;
          user = "immich";
          group = "immich";
          mode = "u=rwx,g=rx,o=";
        }
      ];
      toBackup = [
        "/persist${config.services.immich.mediaLocation}/backups"
        "/persist${config.services.immich.mediaLocation}/library"
        "/persist${config.services.immich.mediaLocation}/upload"
        "/persist${config.services.immich.mediaLocation}/profile"
      ];
    }
    {
      name = "paperless"; inherit (config.services.paperless) port; dashboardSection = "Media";
      toPersist = [
        {
          directory = config.services.paperless.dataDir;
          user = "paperless";
          group = "paperless";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist${config.services.paperless.dataDir}"
      ];
    }
    {
      name = "fusion";
      port = 51000;
      dashboardSection = "Media";
      dashboardIcon = "tinytinyrss";
      toPersist = [
        {
          directory = "/var/lib/fusion";
          user = "root";
          group = "root";
          mode = "u=rwx,g=rx,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/fusion"
      ];
    }
    {
      name = "sonarr"; inherit (config.services.sonarr.settings.server) port; dashboardSection = "Media";
      toPersist = [
        {
          directory = config.services.sonarr.dataDir;
          user = "media";
          group = "media";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist${config.services.sonarr.dataDir}"
      ];
    }
    {
      name = "radarr"; inherit (config.services.radarr.settings.server) port; dashboardSection = "Media";
      toPersist = [
        {
          directory = config.services.radarr.dataDir;
          user = "media";
          group = "media";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist${config.services.radarr.dataDir}"
      ];
    }
    {
      name = "prowlarr"; inherit (config.services.prowlarr.settings.server) port; dashboardSection = "Media";
      toPersist = [
        {
          directory = "/var/lib/private/prowlarr";
          user = "prowlarr";
          group = "prowlarr";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist/var/lib/private/prowlarr"
      ];
    }
    {
      name = "deluge"; inherit (config.services.deluge.web) port; dashboardSection = "Media";
      toBackup = [
        "${config.services.deluge.dataDir}/.config"
      ];
    }
    {
      name = "bazarr";
      port = config.services.bazarr.listenPort;
      dashboardSection = "Media";
      toPersist = [
        {
          directory = "/var/lib/bazarr";
          user = "media";
          group = "media";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist/var/lib/bazarr"
      ];
    }
    {
      name = "calibre";
      port = config.services.calibre-web.listen.port;
      dashboardSection = "Media";
      toPersist = [
        {
          directory = config.services.calibre-web.options.calibreLibrary;
          user = "calibre-server";
          group = "calibre-server";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist${config.services.calibre-web.options.calibreLibrary}"
      ];
    }

    # Tools
    {
      name = "llama";
      dashboardIcon = "codellm";
      port = config.services.llama-swap.port;
      dashboardSection = "Tools";
      toPersist = [ ];
      toBackup = [ ];
    }
    {
      name = "lemonade";
      port = config.hardware.amd-npu.lemonade.port;
      dashboardSection = "Tools";
      toPersist = [ ];
      toBackup = [
        "/root/.cache/lemonade"
      ];
    }
    {
      name = "ntfy";
      port = 23445;
      dashboardSection = "Tools";
      toPersist = [
        {
          directory = "/var/cache/ntfy-sh";
          user = "ntfy-sh";
          group = "ntfy-sh";
          mode = "u=rwx,g=,o=";
        }
        {
          directory = "/var/lib/ntfy-sh";
          user = "ntfy-sh";
          group = "ntfy-sh";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/ntfy-sh"
      ];
    }
    {
      name = "open-webui";
      port = config.services.open-webui.port;
      dashboardSection = "Tools";
      toPersist = [
        # {
        #   directory = "${config.services.open-webui.stateDir}";
        #   user = "open-webui";
        #   group = "open-webui";
        #   mode = "u=rwx,g=,o=";
        # }
      ];
      toBackup = [
        "/persist/var/lib/open-webui/data"
      ];
    }
    {
      name = "hermes";
      port = 8642; # placeholder not used
      shouldMonitorUptime = false;
      shouldBehindReverseProxy = false;
      shouldBeInDashboard = false;
      toPersist = [
        {
          directory = "/var/lib/hermes";
          user = "hermes";
          group = "hermes";
          mode = "u=rwx,g=rwx,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/hermes"
      ];
    }
    {
      name = "signal-cli";
      port = 7583;
      shouldMonitorUptime = false;
      shouldBehindReverseProxy = false;
      shouldBeInDashboard = false;
      toPersist = [
        {
          directory = "/var/lib/signal-cli";
          user = "hermes";
          group = "hermes";
          mode = "u=rwx,g=rwx,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/signal-cli"
      ];
    }
    {
      name = "backrest";
      port = 9898;
      dashboardSection = "Tools";
      authType = "proxy";
      toPersist = [
        {
          directory = "/root/.config/backrest";
          user = "root";
          group = "root";
          mode = "u=rwx,g=,o=";
        }
        {
          directory = "/root/.local/backrest";
          user = "root";
          group = "root";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/root/.config/backrest"
      ];
    }
    { name = "paperless-gpt"; port = 28982; dashboardSection = "Tools"; authType = "proxy"; }
    {
      name = "n8n";
      port = (lib.strings.toInt config.services.n8n.environment.N8N_PORT);
      dashboardSection = "Tools";
      authType = "proxy";
      toPersist = [
        {
          directory = "/var/lib/n8n";
          user = "root";
          group = "root";
          mode = "u=rwx,g=rx,o=";
        }
        {
          directory = "/var/lib/redis-n8n";
          user = "redis-n8n";
          group = "redis-n8n";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/n8n"
        "/persist/var/lib/redis-n8n"
      ];
    }
    {
      name = "neo4j";
      port = 7474;
      dashboardSection = "Tools";
      toPersist = [
        {
          directory = "/var/lib/neo4j";
          user = "neo4j";
          group = "neo4j";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/neo4j"
      ];
    }
    {
      name = "paperless-ai";
      port = 28983;
      dashboardSection = "Tools";
      authType = "proxy";
      toPersist = [
        {
          directory = "/var/lib/paperless-ai";
          user = "root";
          group = "root";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/paperless-ai"
      ];
    }
    {
      name = "search";
      port = 25684;
      dashboardIcon = "searxng";
      dashboardSection = "Tools";
      toPersist = [ ];
      toBackup = [ ];
    }

    # Observability
    {
      name = "grafana";
      port = config.services.grafana.settings.server.http_port;
      dashboardSection = "Observability";
      toBackup = [
        "/persist/var/volumes/grafana/plugins"
      ];
    }
    {
      name = "gatus";
      port = config.services.gatus.settings.web.port;
      dashboardSection = "Observability";
      toPersist = [
        {
          directory = "/var/lib/gatus";
          user = "gatus";
          group = "gatus";
          mode = "u=rwx,g=rx,o=rx";
        }
      ];
      toBackup = [
        "/persist/var/lib/gatus"
      ];
    }
    { name = "whoami"; port = config.services.whoami.port; dashboardSection = "Observability"; authType = "proxy"; }

    # Management
    { name = "dashboard"; subdomain = "homelab"; inherit (config.services.glance.settings.server) port; dashboardSection = "Management"; dashboardIcon = "glance"; authType = "proxy"; }
    {
      name = "homeassistant";
      subdomain = "hass";
      port = config.services.home-assistant.config.http.server_port;
      dashboardSection = "Management";
      dashboardIcon = "home-assistant";
      toPersist = [
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
      ];
      toBackup = [
        "/persist${config.services.home-assistant.configDir}"
        "/persist${config.services.mosquitto.dataDir}"
        "/persist${config.services.zigbee2mqtt.dataDir}"
      ];
    }
    {
      name = "authentik";
      subdomain = "auth";
      isExposed = true;
      port = 9000;
      path = "/outpost.goauthentik.io";
      dashboardSection = "Management";
      toPersist = [
        {
          directory = "/var/lib/authentik";
          user = "authentik";
          group = "authentik";
          mode = "u=rwx,g=,o=";
        }
      ];
      toBackup = [
        "/persist/var/lib/authentik/media"
      ];
    }
  ];
}
