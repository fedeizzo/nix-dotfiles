{ config, lib, ... }:

with lib;
let
  cfg = config.fi;

  serviceModule = types.submodule ({ config, ... }: {
    options = {
      name = mkOption { type = types.str; };
      subdomain = mkOption { type = types.nullOr types.str; default = config.name; };
      port = mkOption { type = types.int; };
      path = mkOption { type = types.str; default = ""; };
      isExposed = mkOption { type = types.bool; default = false; };
      authType = mkOption { type = (types.enum [ "proxy" "none" ]); default = "none"; };
      dashboardSection = mkOption { type = types.str; };
      dashboardIcon = mkOption { type = types.str; default = "${config.name}"; };
    };
  });

  getHost = subdomain: (lib.strings.concatStringsSep "." ((lib.lists.optional (subdomain != null) subdomain) ++ [ "fedeizzo.dev" ]));

  authHandlers = {
    proxy = { middlewares = [ "authentik" ]; };
    none = { };
  };

  routersGenerator = services: builtins.listToAttrs
    (map
      (service:
        {
          inherit (service) name;
          value = {
            entryPoints = [ (if service.isExposed then "websecure" else "wgsecure") ];
            rule = "Host(`${(getHost service.subdomain)}`)";
            service = service.name;
            priority = 10;
            # middlewares = lib.lists.optional (service.subdomain != "auth") "authentik";
          } // authHandlers."${service.authType}";
        }
      )
      services);


  servicesGenerator = services: builtins.listToAttrs
    (map
      (service:
        {
          inherit (service) name;
          value = {
            loadBalancer = {
              servers = [
                { url = "http://localhost:${toString service.port}${service.path}"; }
              ];
            };
          };
        }
      )
      services);
in
{
  imports = [
    ./dns-updater
    ./wake-on-lan-forwarder
  ];

  options.fi = {
    services = mkOption {
      type = types.listOf serviceModule;
    };
  };

  config = {
    users.users.traefik.uid = 990; # make backup consistent across machines
    services.whoami = {
      enable = true;
      port = 15558;
    };
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
            address = "192.168.1.65:80"; # allow only ethernet interface
            http = {
              redirections = {
                entrypoint = { to = "websecure"; };
              };
            };
          };
          websecure = {
            address = "192.168.1.65:443"; # allow only ethernet interface
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
          wgsecure = {
            address = "192.168.7.1:443"; # allow only wireguard interface
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
          localhostonly = {
            address = "127.0.0.1:34486"; # allow only localhost to reach this entrypoint
            http = {
              tls = {
                certResolver = "leresolver";
                domains = [
                  { main = "fedeizzo.dev"; sans = [ "*.fedeizzo.dev" ]; }
                ];
              };
              middlewares = [ "secHeaders@file" "wake-on-lan@file" ];
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
            wake-on-lan = {
              forwardAuth = {
                address = "http://127.0.0.1:35867/wake";
                trustForwardHeader = true;
                authResponseHeaders = [
                  "X-Wake-On-Lan"
                ];
              };
            };
            authentik = {
              forwardAuth = {
                address = "http://localhost:9000/outpost.goauthentik.io/auth/traefik";
                trustForwardHeader = true;
                authResponseHeaders = [
                  "X-authentik-username"
                  "X-authentik-groups"
                  "X-authentik-entitlements"
                  "X-authentik-email"
                  "X-authentik-name"
                  "X-authentik-uid"
                  "X-authentik-jwt"
                  "X-authentik-meta-jwks"
                  "X-authentik-meta-outpost"
                  "X-authentik-meta-provider"
                  "X-authentik-meta-app"
                  "X-authentik-meta-version"
                ];
              };
            };
          };
          routers = {
            authForwarder = {
              entryPoints = [ "websecure" ];
              rule = "HostRegexp(`{subdomain:[a-z0-9]+}.fedeizzo.dev`) && PathPrefix(`/outpost.goauthentik.io/`)";
              service = "authentik";
              priority = 150;
            };
            llm-provider = {
              rule = "Host(`llm.fedeizzo.dev`)";
              entryPoints = [ "wgsecure" ];
              service = "llm-provider";
              middlewares = [ "wake-on-lan@file" ];
            };
          } // (routersGenerator cfg.services);
          services = {
            llm-provider = {
              loadBalancer = { servers = [{ url = "http://192.168.1.67:9999"; }]; };
            };
          } // servicesGenerator cfg.services;
        };
      };
    };

    systemd.services.traefik.serviceConfig.SupplementaryGroups = [ "nextcloud" ];
  };
}
