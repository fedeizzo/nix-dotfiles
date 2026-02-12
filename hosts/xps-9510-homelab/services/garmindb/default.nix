{ config, lib, pkgs-unstable, pkgs, ... }:

let
  garmindbConfig = builtins.fromJSON (builtins.readFile ./garmindb_config.json) // {
    credentials = {
      user = "federico.izzo99@gmail.com";
      secure_password = false;
      password_file = "${config.sops.secrets.garmindb_password.path}";
    };
  };
  configFile = pkgs.writeTextFile {
    name = "garmindb_config.json";
    text = builtins.toJSON garmindbConfig;
  };
in
{
  users.groups.garmindb = {
    gid = 962;
  };
  users.users.garmindb = {
    uid = 970;
    isSystemUser = true;
    group = "garmindb";
    home = "/var/lib/garmindb";
  };
  systemd.tmpfiles.rules = [
    "C! /var/lib/garmindb/GarminConnectConfig.json 0400 garmindb garmindb - ${configFile}"
  ];
  systemd.services.garmindb = {
    enable = true;
    description = "garmindb";
    wantedBy = [ ];
    after = [ "network.target" ];

    serviceConfig = {
      User = "garmindb";
      Group = "garmindb";
      DynamicUser = false;
      # RuntimeDirectoryMode = "0400";
      Type = "oneshot";
      ExecStart = "${pkgs-unstable.garmindb}/bin/garmindb_cli.py --config /var/lib/garmindb --download --import --analyze --all";
      # ExecStart = "${pkgs-unstable.garmindb}/bin/garmindb_cli.py --config /var/lib/garmindb --download --import --analyze --all --latest";
      StateDirectory = "garmindb";
      WorkingDirectory = "/var/lib/garmindb";
      ReadWritePaths = [ "/var/lib/garmindb" ];

      # Hardening
      PrivateTmp = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      NoNewPrivileges = true;
      PrivateDevices = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
    };
  };

  systemd.timers.garmindb = {
    enable = true;
    description = "Run garmindb";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      Persistent = true;
      OnCalendar = "*-*-* 2:00:00";
    };
  };

  # virtualisation.oci-containers.containers."garmindb-sync" = {
  #   image = "fedeizzo/garmindb-sync:latest";
  #   autoStart = true;
  #   volumes = [
  #     "/var/container_envs/garmindb.json:/root/.GarminDb/GarminConnectConfig.json"
  #     "/var/volumes/garmindb:/root/HealthData"
  #   ];
  # };

  virtualisation.oci-containers.containers."garmin-fetch-data" = {
    image = "thisisarpanghosh/garmin-fetch-data:latest";
    autoStart = true;
    extraOptions = [
      # "--restart=unless-stopped"
      "--network=host" # to see influx
    ];
    user = "root:root";
    environmentFiles = [
      "${config.sops.secrets.garmin-fetch-data.path}"
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

  sops.secrets.garmin-fetch-data = {
    format = "dotenv";
    mode = "0400";
    restartUnits = [
      "docker-garmin-fetch-data.service"
    ];
    sopsFile = ./garmin-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
  sops.secrets.garmindb_password = {
    format = "yaml";
    mode = "0440";
    group = config.systemd.services.garmindb.serviceConfig.Group;
    sopsFile = ./garmindb-homelab-secrets.yaml;
  };
}
