{
  flake.modules.nixos.homelab = { inputs, config, ... }:
    let
      persistanceFromServices = builtins.concatLists (map (el: el.toPersist) config.fi.services);
    in
    {
      environment.persistence."/persist" = {
        directories = [
          # ─────────────────────────────
          # Homelab platform state (also backed up)
          # ─────────────────────────────
          "/var/container_envs"
          "/var/volumes"

          {
            directory = "/var/lib/garmindb";
            user = "garmindb";
            group = "garmindb";
            mode = "u=rwx,g=rx,o=rx";
          }

          # System / root services
          "/var/lib/fail2ban"
          "/var/lib/garmin-fetch-data"

          # DB services
          {
            directory = "/var/lib/influxdb";
            user = "influxdb";
            group = "influxdb";
            mode = "u=rwx,g=rwx,o=";
          }
          {
            directory = "/var/lib/postgresql";
            user = "postgres";
            group = "postgres";
            mode = "u=rwx,g=rx,o=";
          }

          # Observability
          {
            directory = "/var/lib/prometheus2";
            user = "prometheus";
            group = "prometheus";
            mode = "u=rwx,g=rx,o=";
          }

          {
            directory = "/var/lib/tindeq";
            user = "nextcloud";
            group = "nextcloud";
            mode = "u=rwx,g=rx,o=rx";
          }

          # Redis (persist but no backup)
          {
            directory = "/var/lib/redis-affine";
            user = "redis-affine";
            group = "redis-affine";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/redis-authentik";
            user = "nobody";
            group = "nogroup";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/redis-immich";
            user = "redis-immich";
            group = "redis-immich";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/redis-nextcloud";
            user = "nextcloud";
            group = "nextcloud";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/redis-paperless";
            user = "redis-paperless";
            group = "redis-paperless";
            mode = "u=rwx,g=,o=";
          }
        ] ++ persistanceFromServices;

        files = [
          # App single-file state
          "/var/lib/private/dns-updater/cache.json"
        ];
      };
    };
}
