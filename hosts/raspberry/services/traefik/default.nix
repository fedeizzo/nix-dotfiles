{ ... }:
{
  services.traefik = {
    enable = true;
    dataDir = "/var/volumes/traefik";
    group = "traefik";
    environmentFiles = [ "/var/container_envs/traefik" ];
    staticConfigOptions = {
      global = { checkNewVersion = false; sendAnonymousUsage = false; };
      serversTransport = { insecureSkipVerify = true; };
      api = { insecure = true; dashboard = true; debug = false; };
      log = { level = "DEBUG"; };
      entryPoints = {
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
        };
        services = {
          fedeizzodev = { loadBalancer = { servers = [{ url = "http://localhost:50001"; }]; }; };
          grafana = { loadBalancer = { servers = [{ url = "http://localhost:50002"; }]; }; };
          nocodb = { loadBalancer = { servers = [{ url = "http://localhost:50003"; }]; }; };
        };
      };
    };
  };
}
