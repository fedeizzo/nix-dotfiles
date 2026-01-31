{ config, pkgs, lib, ... }:

let
  env = {
    REDIS_SERVER_HOST = "127.0.0.1";
    REDIS_SERVER_PORT = "52231";
    AFFINE_INDEXER_ENABLED = "false";
    AFFINE_REVISION = "stable";
    AFFINE_SERVER_EXTERNAL_URL = "https://affine.fedeizzo.dev";
  };

  dockerEnvFlags =
    builtins.concatStringsSep " \\\n      "
      (lib.mapAttrsToList
        (name: value: "-e ${name}=${value}")
        env);

  affineMigrationScript = pkgs.writeShellScriptBin "affine-migration.sh" ''
    set -euo pipefail

    IMAGE="ghcr.io/toeverything/affine:stable"
    NAME="affine-migration"

    echo "Running Affine migration…"

    if docker ps -a --format '{{.Names}}' | grep -q "^$NAME$"; then
      docker rm -f "$NAME"
    fi

    docker run --rm \
      --name "$NAME" \
      --network host \
      --env-file ${config.sops.secrets.affine.path} \
      ${dockerEnvFlags} \
      -v /var/lib/affine/config:/root/.affine/config \
      -v /var/lib/affine/storage:/root/.affine/storage \
      "$IMAGE" \
      sh -c "node ./scripts/self-host-predeploy.js"

    echo "Affine migration completed."
  '';
in
{
  services.redis.servers.affine = {
    enable = true;
    port = 52231;
  };

  virtualisation.oci-containers.containers = {
    "affine-server" = {
      image = "ghcr.io/toeverything/affine:stable";
      environment = env;
      environmentFiles = [ config.sops.secrets.affine.path ];
      volumes = [
        "/var/lib/affine/config:/root/.affine/config"
        "/var/lib/affine/storage:/root/.affine/storage"
      ];
      extraOptions = [ "--network=host" ];
      autoStart = false;
    };
  };
  # systemd.services.docker-affine-server = {
  #   after = [ "docker.service" ];
  #   requires = [ "docker.service" ];
  #   preStart = affineMigrationScript;
  # };

  environment.systemPackages = [ affineMigrationScript ];

  sops.secrets.affine = {
    format = "dotenv";
    mode = "0400";
    # restartUnits = [
    #   "affine-server.service"
    # ];
    sopsFile = ./affine-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
