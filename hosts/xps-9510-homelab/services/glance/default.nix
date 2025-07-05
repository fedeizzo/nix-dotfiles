{ pkgs-unstable, ... }:

{
  services.glance = {
    enable = true;
    package = pkgs-unstable.glance;
    settings = {
      server = {
        host = "0.0.0.0";
        port = 9854;
      };

      pages = [
        {
          name = "Startpage";
          width = "slim";
          hide-desktop-navigation = true;
          center-vertically = true;
          columns = [
            {
              size = "small";
              # rss hwacker news group weather
              widgets = [
                { type = "weather"; units = "metric"; hour-format = "24h"; location = "Paris, France"; }
                { type = "server-stats"; servers = [{ type = "local"; name = "homelab"; }]; }
                {
                  type = "markets";
                  markets = [
                    { symbol = "DDOG"; name = "Datadog"; }
                    { symbol = "DCAM"; name = "PEA"; }
                  ];
                }
                { type = "hacker-news"; limit = 15; collapse-after = 5; }
              ];
            }
            {
              size = "full";
              widgets = [
                {
                  type = "search";
                  autofocus = true;
                  new-tab = true;
                  bangs = [
                    {
                      title = "Nixpkgs github issues";
                      shortcut = "!nh";
                      url = "https://github.com/NixOS/nixpkgs/issues?q=sort%3Aupdated-desc%20is%3Aissue%20is%3Aopen%20{QUERY}";
                    }
                  ];
                }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Streaming";
                  sites = [
                    { title = "Jellyfin"; url = "https://jellyfin.fedeizzo.dev"; icon = "di:jellyfin"; }
                    { title = "Jellyseerr"; url = "https://jellyseerr.fedeizzo.dev"; icon = "di:jellyseerr"; }
                    { title = "Sonarr"; url = "http://homelab:8989"; icon = "di:sonarr"; }
                    { title = "Radarr"; url = "http://homelab:7878"; icon = "di:radarr"; }
                    { title = "Prowlar"; url = "http://homelab:9696"; icon = "di:prowlarr"; }
                    { title = "Deluge"; url = "http://homelab:8112"; icon = "di:deluge"; }
                    { title = "Bazarr"; url = "http://homelab:6767"; icon = "di:bazarr"; }
                  ];
                }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Utils";
                  sites = [
                    { title = "Immich"; url = "https://photo.fedeizzo.dev"; icon = "di:immich"; }
                    { title = "Nextctloud"; url = "https://nextcloud.fedeizzo.dev"; icon = "di:nextcloud"; }
                    { title = "Paperless"; url = "https://paperless.fedeizzo.dev"; icon = "di:paperless"; }
                  ];
                }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Observability";
                  sites = [
                    { title = "Grafana"; url = "https://grafana.fedeizzo.dev"; icon = "di:grafana"; }
                    { title = "Traefik"; url = "http://homelab:8080"; icon = "di:traefik-proxy"; }
                    { title = "Home Assistant"; url = "https://hass.fedeizzo.dev"; icon = "di:home-assistant"; }
                  ];
                }
              ];
            }
          ];
        }
      ];
    };
  };
}
