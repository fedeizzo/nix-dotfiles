_:

{
  services.glance = {
    enable = true;
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
              size = "full";
              widgets = [
                { type = "search"; autofocus = true; }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Streaming";
                  sites = [
                    { title = "Jellyfin"; url = "http://homelab:8096"; icon = "si:jellyfin"; }
                    { title = "Jellyseerr"; url = "http://homelab:5055"; icon = "si:jellyfin"; }
                    { title = "Sonarr"; url = "http://homelab:8989"; icon = "si:sonarr"; }
                    { title = "Radarr"; url = "http://homelab:7878"; icon = "si:radarr"; }
                    { title = "Prowlar"; url = "http://homelab:9696"; icon = "si:searxng"; }
                    { title = "Deluge"; url = "http://homelab:8112"; icon = "si:deluge"; }
                    { title = "Bazarr"; url = "http://homelab:6767"; icon = "si:bookstack"; }
                  ];
                }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Multimedia";
                  sites = [
                    { title = "Immich"; url = "https://photo.fedeizzo.dev"; icon = "si:immich"; }
                    { title = "Drive"; url = "https://drive.fedeizzo.dev"; icon = "si:protondrive"; }
                    { title = "Sunshine"; url = "https://homelab:47990"; icon = "si:stadia"; }
                  ];
                }
                {
                  type = "monitor";
                  cache = "1m";
                  title = "Observability";
                  sites = [
                    { title = "Grafana"; url = "https://grafana.fedeizzo.dev"; icon = "si:grafana"; }
                    { title = "Traefik"; url = "http://homelab:8080"; icon = "si:traefikproxy"; }
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
