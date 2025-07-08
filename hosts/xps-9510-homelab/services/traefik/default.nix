{ config, ... }:

{
  users.users.traefik.uid = 990; # make backup consistent across machines
  services.traefik = {
    enable = true;
    dataDir = "/var/volumes/traefik";
    group = "traefik";
    environmentFiles = [ "/var/container_envs/traefik" ];
    staticConfigOptions = {
      global = { checkNewVersion = false; sendAnonymousUsage = false; };
      metrics = {
        prometheus = { addRoutersLabels = true; addServicesLabels = true; entryPoint = "metrics"; };
      };
      serversTransport = { insecureSkipVerify = true; };
      api = { insecure = true; dashboard = true; debug = false; };
      log = { level = "INFO"; filePath = "/var/volumes/traefik/log/traefik.json"; format = "json"; };
      accessLog = {
        filePath = "/var/volumes/traefik/log/access.json";
        format = "json";
        bufferingSize = 0; # collect logs as in-memory buffer before writing into log file
        fields = {
          headers = {
            defaultMode = "drop"; # drop all headers per default
            names = {
              User-Agent = "keep"; # log user agent strings
            };
          };
        };
      };
      entryPoints = {
        metrics = {
          address = ":8082";
        };
        web = {
          address = ":80";
          http = {
            redirections = {
              entrypoint = { to = "websecure"; };
            };
          };
        };
        websecure = {
          address = ":443";
          http = {
            tls = {
              certResolver = "leresolver";
              domains = [
                { main = "fedeizzo.dev"; sans = [ "*.fedeizzo.dev" ]; }
              ];
            };
            middlewares = [ "secHeaders@file" ];
          };
        };
      };
      certificatesResolvers =
        {
          leresolver = {
            acme = {
              email = "letsencrypt.alert@fedeizzo.dev";
              storage = "/var/volumes/traefik/acme.json";
              dnsChallenge = {
                provider = "cloudflare";
                delaybeforecheck = "0s";
                resolvers = [ "1.1.1.1:53" "8.8.8.8:53" ];
              };
            };
          };
        };
    };

    dynamicConfigOptions = {
      http = {
        middlewares = {
          secHeaders = {
            headers = {
              browserXssFilter = true; # enables basic protection against reflected XSS
              contentTypeNosniff = true; # tells browsers not to try to guess the content type
              frameDeny = true; # prevent clickjacking
              customFrameOptionsValue = "SAMEORIGIN"; # prevent clickjacking
              stsIncludeSubdomains = true; # enforce strict HTTPS
              stsPreload = true; # enforce strict HTTPS
              stsSeconds = 31536000; # enforce strict HTTPS
              customResponseHeaders = { server = ""; x-powered-by = ""; }; # remove some unnecessary info from the header
            };
          };
        };
        routers = {
          fedeizzodev = { entryPoints = [ "websecure" ]; rule = "Host(`fedeizzo.dev`)"; service = "fedeizzodev"; };
          grafana = { entryPoints = [ "websecure" ]; rule = "Host(`grafana.fedeizzo.dev`)"; service = "grafana"; };
          nocodb = { entryPoints = [ "websecure" ]; rule = "Host(`nocodb.fedeizzo.dev`)"; service = "nocodb"; };
          drive = { entryPoints = [ "websecure" ]; rule = "Host(`drive.fedeizzo.dev`)"; service = "drive"; };
          immich = { entryPoints = [ "websecure" ]; rule = "Host(`photo.fedeizzo.dev`)"; service = "immich"; };
          dashboard = { entryPoints = [ "websecure" ]; rule = "Host(`homelab.fedeizzo.dev`)"; service = "dashboard"; };
          jellyfin = { entryPoints = [ "websecure" ]; rule = "Host(`jellyfin.fedeizzo.dev`)"; service = "jellyfin"; };
          jellyseerr = { entryPoints = [ "websecure" ]; rule = "Host(`jellyseerr.fedeizzo.dev`)"; service = "jellyseerr"; };
          hass = { entryPoints = [ "websecure" ]; rule = "Host(`hass.fedeizzo.dev`)"; service = "hass"; };
          paperless = { entryPoints = [ "websecure" ]; rule = "Host(`paperless.fedeizzo.dev`)"; service = "paperless"; };
          fusion = { entryPoints = [ "websecure" ]; rule = "Host(`fusion.fedeizzo.dev`)"; service = "fusion"; };
          nextcloud = { entryPoints = [ "websecure" ]; rule = "Host(`nextcloud.fedeizzo.dev`)"; service = "nextcloud"; };
          collabora = { entryPoints = [ "websecure" ]; rule = "Host(`collabora.fedeizzo.dev`)"; service = "collabora"; };
        };
        services = {
          fedeizzodev = { loadBalancer = { servers = [{ url = "http://localhost:50001"; }]; }; };
          grafana = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.grafana.settings.server.http_port}"; }]; }; };
          nocodb = { loadBalancer = { servers = [{ url = "http://localhost:50003"; }]; }; };
          drive = { loadBalancer = { servers = [{ url = "http://localhost:50006"; }]; }; };
          immich = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.immich.port}"; }]; }; };
          dashboard = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.glance.settings.server.port}"; }]; }; };
          jellyfin = { loadBalancer = { servers = [{ url = "http://localhost:8096"; }]; }; };
          jellyseerr = { loadBalancer = { servers = [{ url = "http://localhost:5055"; }]; }; };
          hass = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.home-assistant.config.http.server_port}"; }]; }; };
          paperless = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.paperless.port}"; }]; }; };
          fusion = { loadBalancer = { servers = [{ url = "http://localhost:51000"; }]; }; };
          nextcloud = { loadBalancer = { servers = [{ url = "http://localhost:8180"; }]; }; };
          collabora = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.collabora-online.port}"; }]; }; };
        };
      };
    };
  };
  systemd.services.traefik.serviceConfig.SupplementaryGroups = [ "nextcloud" ];
}
