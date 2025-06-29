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
    settings.overwriteprotocol = "https";
    settings.trusted_proxies = [ "127.0.0.1" "::1" ]; # Trust Traefik (localhost)
    phpOptions = {
      "opcache.interned_strings_buffer" = "16";
      "opcache.memory_consumption" = "256";
    };

    caching.apcu = true;

    appstoreEnable = true;
    configureRedis = true;
    home = "/var/lib/nextcloud";
    enableImagemagick = false;
    extraAppsEnable = true;
    extraApps = {
      inherit (pkgs.nextcloud31Packages.apps) calendar onlyoffice;
    };
    https = true;
  };

  sops.secrets = {
    nextcloud-admin-password = lib.mkIf config.services.nextcloud.enable {
      inherit sopsFile mode owner group format restartUnits;
    };

  };
}
