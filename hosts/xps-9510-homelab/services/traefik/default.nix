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
        prometheus = {
          addRoutersLabels = true;
          addServicesLabels = true;
          entryPoint = "metrics";
        };
      };
      serversTransport = { insecureSkipVerify = true; };
      api = { insecure = true; dashboard = true; debug = false; };
      log = { level = "INFO"; filePath = "/var/volumes/traefik/log/traefik.json"; format = "json"; };
      accessLog = { filePath = "/var/volumes/traefik/log/access.json"; format = "json"; };
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
        routers = {
          fedeizzodev = { entryPoints = [ "websecure" ]; rule = "Host(`fedeizzo.dev`)"; service = "fedeizzodev"; };
          grafana = { entryPoints = [ "websecure" ]; rule = "Host(`grafana.fedeizzo.dev`)"; service = "grafana"; };
          nocodb = { entryPoints = [ "websecure" ]; rule = "Host(`nocodb.fedeizzo.dev`)"; service = "nocodb"; };
          drive = { entryPoints = [ "websecure" ]; rule = "Host(`drive.fedeizzo.dev`)"; service = "drive"; };
          immich = { entryPoints = [ "websecure" ]; rule = "Host(`photo.fedeizzo.dev`)"; service = "immich"; };
        };
        services = {
          fedeizzodev = { loadBalancer = { servers = [{ url = "http://localhost:50001"; }]; }; };
          grafana = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.grafana.settings.server.http_port}"; }]; }; };
          nocodb = { loadBalancer = { servers = [{ url = "http://localhost:50003"; }]; }; };
          drive = { loadBalancer = { servers = [{ url = "http://localhost:50006"; }]; }; };
          immich = { loadBalancer = { servers = [{ url = "http://localhost:${toString config.services.immich.port}"; }]; }; };
        };
      };
    };
  };
}
