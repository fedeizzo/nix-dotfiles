{ lib, config, pkgs, ... }:

let
  docker = config.virtualisation.oci-containers.backend;
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./affine
    ./authentik
    ./blocky
    ./calibre
    ./climbing-lab
    ./fedeizzo.dev
    ./fusion
    ./garmindb
    ./glance
    ./grafana
    ./hass
    ./immich
    ./influxdb
    ./logrotate
    ./loki
    ./net-worth
    ./nextcloud
    ./open-webui
    # ./opencloud
    ./paperless
    ./postgres
    ./prometheus
    ./subtrackr
    # ./sunshine
    ./streaming
    ./traefik
    ./uptime-kuma
  ];

  config = {
    system.activationScripts.networth = ''
      ${dockerBin} network inspect networth >/dev/null 2>&1 || ${dockerBin} network create networth --subnet 172.20.0.0/16
    '';

    fi.services = [
      # Exposed
      { name = "fedeizzodev"; subdomain = null; port = 50001; isExposed = true; dashboardSection = "Exposed"; dashboardIcon = "hugo"; }

      # Exposed media
      { name = "jellyfin"; port = 8096; isExposed = true; dashboardSection = "Exposed media"; }
      { name = "jellyseerr"; port = 5055; isExposed = true; dashboardSection = "Exposed media"; }
      { name = "nextcloud"; port = 8180; isExposed = true; dashboardSection = "Exposed media"; }
      { name = "collabora"; inherit (config.services.collabora-online) port; isExposed = true; dashboardSection = "Exposed media"; dashboardIcon = "collabora-online"; }
      { name = "affine"; port = 3010; isExposed = true; dashboardSection = "Exposed media"; }

      # Media
      { name = "immich"; subdomain = "photo"; inherit (config.services.immich) port; dashboardSection = "Media"; }
      { name = "paperless"; inherit (config.services.paperless) port; dashboardSection = "Media"; }
      { name = "fusion"; port = 51000; dashboardSection = "Media"; dashboardIcon = "tinytinyrss"; }
      { name = "sonarr"; inherit (config.services.sonarr.settings.server) port; dashboardSection = "Media"; }
      { name = "radarr"; inherit (config.services.radarr.settings.server) port; dashboardSection = "Media"; }
      { name = "prowlarr"; inherit (config.services.prowlarr.settings.server) port; dashboardSection = "Media"; }
      { name = "deluge"; inherit (config.services.deluge.web) port; dashboardSection = "Media"; }
      { name = "bazarr"; port = config.services.bazarr.listenPort; dashboardSection = "Media"; }

      # Tools
      { name = "open-webui"; port = config.services.open-webui.port; dashboardSection = "Tools"; }
      { name = "backrest"; port = 9898; dashboardSection = "Tools"; authType = "proxy"; }
      { name = "paperless-gpt"; port = 28982; dashboardSection = "Tools"; authType = "proxy"; }
      { name = "paperless-ai"; port = 28983; dashboardSection = "Tools"; authType = "proxy"; }
      { name = "subtrackr"; port = config.services.subtrackr.port; dashboardSection = "Tools"; dashboardIcon = "nextcloud-cospend"; authType = "proxy"; }

      # Observability
      { name = "grafana"; port = config.services.grafana.settings.server.http_port; dashboardSection = "Observability"; }
      { name = "uptime-kuma"; port = (lib.strings.toInt config.services.uptime-kuma.settings.UPTIME_KUMA_PORT); dashboardSection = "Observability"; }
      { name = "whoami"; port = config.services.whoami.port; dashboardSection = "Observability"; authType = "proxy"; }

      # Management
      { name = "dashboard"; subdomain = "homelab"; inherit (config.services.glance.settings.server) port; dashboardSection = "Management"; dashboardIcon = "glance"; authType = "proxy"; }
      { name = "homeassistant"; subdomain = "hass"; port = config.services.home-assistant.config.http.server_port; dashboardSection = "Management"; dashboardIcon = "home-assistant"; }
      { name = "authentik"; subdomain = "auth"; isExposed = true; port = 9000; path = "/outpost.goauthentik.io"; dashboardSection = "Management"; }
    ];
  };
}
