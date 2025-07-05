{ lib, pkgs, config, ... }:

let
  sopsFile = ./nextcloud-homelab-secrets.yaml;
  format = "yaml";
  mode = "0400";
  owner = config.users.users.nextcloud.name;
  group = config.users.groups.nextcloud.name;
  restartUnits = [ ];
in
{
  users.users.nextcloud.extraGroups = [ "render" "users" ];
  services.nginx.virtualHosts."nextcloud.fedeizzo.dev".listen = [{ addr = "127.0.0.1"; port = 8180; }];

  services.nextcloud = {
    enable = true;
    package = pkgs.nextcloud31;

    hostName = "nextcloud.fedeizzo.dev";
    config = {
      adminuser = "fedeizzo";
      adminpassFile = config.sops.secrets.nextcloud-admin-password.path;

      dbtype = "sqlite";
    };
    settings = {
      overwriteprotocol = "https";
      trusted_proxies = [ "127.0.0.1" "::1" ]; # Trust Traefik (localhost)
      maintenance_window_start = "1"; # run intensive task between 1AM and 5AM
    };
    phpOptions = {
      "opcache.interned_strings_buffer" = "16";
      "opcache.memory_consumption" = "256";
    };

    caching.apcu = true;

    appstoreEnable = true;
    configureRedis = true;
    home = "/var/lib/nextcloud";
    enableImagemagick = true;
    extraAppsEnable = true;
    extraApps = {
      inherit (pkgs.nextcloud31Packages.apps) calendar richdocuments;
    };
    https = true;
  };

  services.collabora-online = {
    enable = true;
    port = 9980; # default
    settings = {
      # Rely on reverse proxy for SSL
      ssl = {
        enable = false;
        termination = true;
      };

      # Listen on loopback interface only, and accept requests from ::1
      net = {
        listen = "loopback";
        post_allow.host = [ "::1" ];
      };

      # Restrict loading documents from WOPI Host nextcloud.fedeizzo.dev
      storage.wopi = {
        "@allow" = true;
        host = [ "nextcloud.fedeizzo.dev" ];
      };

      # Set FQDN of server
      server_name = "collabora.fedeizzo.dev";
    };
  };

  sops.secrets = {
    nextcloud-admin-password = lib.mkIf config.services.nextcloud.enable {
      inherit sopsFile mode owner group format restartUnits;
    };
  };

}
