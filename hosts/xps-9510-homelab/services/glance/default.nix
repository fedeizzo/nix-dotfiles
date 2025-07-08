{ lib, config, pkgs-unstable, ... }:

let
  groupedServices = lib.lists.groupBy (service: service.dashboardSection) config.fi.services;

  getHost = subdomain: (lib.strings.concatStringsSep "." ((lib.lists.optional (! (isNull subdomain)) subdomain) ++ [ "fedeizzo.dev" ]));

  monitorEntryMapper = service: {
    title = service.name;
    url = "https://${(getHost service.subdomain)}";
    icon = "di:${service.dashboardIcon}";
  };
  # We have to manually create an entry for traefik because we cannot list in fi.services
  traefikEntry = { title = "Traefik"; url = "http://homelab:8080"; icon = "di:traefik-proxy"; };
  dashboardSectionMapper = section: services: {
    type = "monitor";
    cache = "1m";
    title = section;
    sites = (lib.lists.optional (section == "Observability") traefikEntry) ++ (map monitorEntryMapper services);
  };

  sections = lib.attrsets.mapAttrsToList dashboardSectionMapper groupedServices;
in
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
              ] ++ sections;
            }
          ];
        }
      ];
    };
  };
}
