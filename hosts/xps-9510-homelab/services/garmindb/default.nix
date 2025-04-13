{ config, lib, ... }:

{
  virtualisation.oci-containers.containers."garmindb-sync" = {
    image = "fedeizzo/garmindb-sync:latest";
    autoStart = true;
    volumes = [
      "/var/container_envs/garmindb.json:/root/.GarminDb/GarminConnectConfig.json"
      "/var/volumes/garmindb:/root/HealthData"
    ];
  };

  virtualisation.oci-containers.containers."garmin-fetch-data" = {
    image = "thisisarpanghosh/garmin-fetch-data:latest";
    autoStart = true;
    extraOptions = [
      # "--restart=unless-stopped"
      "--network=host" # to see influx
    ];
    user = "root:root";
    environmentFiles = [
      "${config.sops.secrets.garmindb.path}"
    ];
    volumes = [
      "/var/lib/garmin-fetch-data:/root/.garminconnect"
    ];
    # for the initial setup of influx
    # [root@homelab:~]# influx -username 'user' -password 'pass' -host 'localhost' -port 8086
    # Connected to http://localhost:8086 version 1.10.7
    # InfluxDB shell version: 1.10.7
    # > CREATE DATABASE "GarminStats"
    # > CREATE RETENTION POLICY "infinite_rp" ON "GarminStats" DURATION INF REPLICATION 1 DEFAULT
    # > SHOW RETENTION POLICIES ON "GarminStats"
  };

  sops.secrets.garmindb = lib.mkIf config.virtualisation.oci-containers.containers."garmin-fetch-data".autoStart {
    format = "dotenv";
    mode = "0400";
    restartUnits = [
      "docker-garmin-fetch-data.service"
    ];
    sopsFile = ./garmin-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
