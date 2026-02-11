{ lib, pkgs, pkgs-unstable, config, ... }:

let
  sopsFile = ./nextcloud-homelab-secrets.yaml;
  format = "yaml";
  mode = "0400";
  restartUnits = [ ];
in
{
  # users.users.nextcloud.extraGroups = lib.mkIf config.services.nextcloud.enable [ "render" "users" "keys" ];
  services = {
    nginx.virtualHosts."nextcloud.fedeizzo.dev".listen = [{ addr = "127.0.0.1"; port = 8180; }];
    nginx.recommendedProxySettings = true;

    nextcloud = {
      enable = true;
      package = pkgs.nextcloud32;
      home = "/var/lib/nextcloud";

      hostName = "nextcloud.fedeizzo.dev";
      https = true;
      config = {
        adminuser = "fedeizzo";
        adminpassFile = config.sops.secrets.nextcloud-admin-password.path;

        dbtype = "pgsql";
        dbhost = "localhost:5432";
        dbname = "nextcloud";
        dbuser = "nextcloud";
        dbpassFile = config.sops.secrets.nextcloud-pg-password.path;
      };
      settings = {
        overwriteprotocol = "https";
        trusted_proxies = [ "127.0.0.1" "::1" "192.168.7.1" ]; # Trust Traefik (localhost) and wireguard
        maintenance_window_start = "1"; # run intensive task between 1AM and 5AM
        default_phone_region = "FR";
        log_type = "file";
      };
      phpOptions = {
        "opcache.interned_strings_buffer" = "16";
        "opcache.memory_consumption" = "256";
        "realpath_cache_size" = "0";
        "opcache.jit" = "1255";
        "opcache.jit_buffer_size" = "8M";
      };

      # push notification for nextcloud-client
      notify_push = {
        enable = true;
        package = pkgs-unstable.nextcloud-notify_push;
        nextcloudUrl = "http://127.0.0.1:8180";
      };

      # cache
      caching = {
        redis = true;
      };
      configureRedis = true;


      # appstore and apps
      appstoreEnable = true;
      extraAppsEnable = true;
      enableImagemagick = true;
      extraApps = {
        inherit (pkgs.nextcloud32Packages.apps) calendar contacts richdocuments tasks deck user_oidc;
      };
      autoUpdateApps.enable = true;
    };

    collabora-online = {
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
  };

  sops.secrets = {
    nextcloud-admin-password = lib.mkIf config.services.nextcloud.enable {
      inherit sopsFile mode format restartUnits;
    };
  };

}
