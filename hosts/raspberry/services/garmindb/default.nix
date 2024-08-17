{ ... }:

{
  virtualisation.oci-containers.containers."garmindb-sync" = {
    image = "fedeizzo/garmindb-sync:latest";
    autoStart = true;
    volumes = [
      "/var/container_envs/garmindb.json:/root/.GarminDb/GarminConnectConfig.json"
      "/var/volumes/garmindb:/root/HealthData"
    ];
  };
}

