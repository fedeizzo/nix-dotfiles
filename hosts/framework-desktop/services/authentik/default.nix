{ config, lib, ... }:

{
  services = {
    authentik = {
      enable = true;
      createDatabase = false;
      nginx.enable = false;
      environmentFile = config.sops.secrets.authentik-env.path;

      settings = {
        email = {
          host = "smtp.fastmail.com";
          port = 456;
          use_ssl = true;
          from = "authentik@fedeizzo.dev";
          # username and password are in the secrets
        };

        postgresql = {
          host = "localhost";
          name = "authentik";
          user = "authentik";
          port = 5432;
          # password is in the secrets
        };

        redis = { db = 1; };

        error_reporting = { enabled = false; };
        disable_startup_analytics = true;
        disable_update_check = true;
      };
    };
  };

  # for consistent backup
  systemd.services.authentik-migrate.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "authentik";
    Group = "authentik";
  };
  systemd.services.authentik-worker.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "authentik";
    Group = "authentik";
  };
  systemd.services.authentik.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "authentik";
    Group = "authentik";
  };

  sops.secrets.authentik-env = lib.mkIf config.services.authentik.enable {
    format = "dotenv";
    sopsFile = ./authentik-homelab-secrets.env;
    mode = "0400";
    key = ""; # to map the whole file as a secret
    # owner = config.systemd.services.authentik
  };
}
