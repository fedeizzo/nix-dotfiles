{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.fi;

  serviceModule = types.submodule ({ config, ... }: {
    options = {
      name = mkOption { type = types.str; };
      subdomain = mkOption { type = types.nullOr types.str; default = config.name; };
      port = mkOption { type = types.int; };
      isExposed = mkOption { type = types.bool; default = false; };
      dashboardSection = mkOption { type = types.str; };
      dashboardIcon = mkOption { type = types.str; default = "${config.name}"; };
    };
  });

  getHost = subdomain: (lib.strings.concatStringsSep "." ((lib.lists.optional (! (isNull subdomain)) subdomain) ++ [ "fedeizzo.dev" ]));

  routersGenerator = (services: builtins.listToAttrs
    (map
      (service:
        {
          name = service.name;
          value = {
            entryPoints = [ "websecure" ];
            rule = "Host(`${(getHost service.subdomain)}`)";
            service = service.name;
          };
        }
      )
      services));

  servicesGenerator = (services: builtins.listToAttrs
    (map
      (service:
        {
          name = service.name;
          value = {
            loadBalancer = {
              servers = [
                { url = "http://localhost:${toString service.port}"; }
              ];
            };
          };
        }
      )
      services));
in
{
  imports = [
    ./dns-updater.nix
  ];

  options.fi = {
    services = mkOption {
      type = types.listOf serviceModule;
    };
  };

  config = {
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
          routers = routersGenerator cfg.services;
          services = servicesGenerator cfg.services;
        };
      };
    };

    systemd.services.traefik.serviceConfig.SupplementaryGroups = [ "nextcloud" ];
  };
}
