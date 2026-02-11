{ lib, config, ... }:

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
    secretsFile = "${config.sops.secrets.immich.path}";

    machine-learning.enable = true;
  };


  sops.secrets.immich = lib.mkIf config.services.immich.enable {
    format = "dotenv";
    mode = "0400";
    owner = config.users.users.immich.name;
    group = config.users.groups.immich.name;
    restartUnits = [
      "immich-server.service"
      "immich-machine-learning.service"
    ];
    sopsFile = ./immich-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
