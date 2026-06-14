{
  flake.modules.nixos.garmindb = { config, lib, pkgs-unstable, pkgs, ... }:
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
    systemd.tmpfiles.rules = [
      "C! /var/lib/garmindb/GarminConnectConfig.json 0400 garmindb garmindb - ${configFile}"
    ];
    systemd.services.garmindb = {
      enable = false;
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

    virtualisation.oci-containers.containers."garmin-fetch-data" = {
      image = "thisisarpanghosh/garmin-fetch-data:latest";
      autoStart = false;
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

    users.users.garmindb = {
      uid = 970;
      isSystemUser = true;
      group = "garmindb";
      home = "/var/lib/garmindb";
    };
    users.groups.garmindb.gid = 970;
  };
}
