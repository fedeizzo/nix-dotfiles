_:

{
  services.immich = {
    enable = true;
    host = "localhost";
    port = 50009;
    database = {
      enable = true;
      createDB = true;
      user = "immich";
      name = "immich";
      host = "localhost";
      port = 5432;
    };
    environment = {
      IMMICH_ENV = "production";
      IMMICH_LOG_LEVEL = "warn";
      IMMICH_API_METRICS_PORT = "50010";
      IMMICH_MICROSERVICES_METRICS_PORT = "50011";
      IMMICH_TELEMETRY_INCLUDE = "all";
    };
    secretsFile = "/var/container_envs/immich";

    machine-learning.enable = true;
  };
}
