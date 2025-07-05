{ inputs, config, ... }:

{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];
  sops = {
    defaultSopsFile = ./homelab-secrets.yaml;
    defaultSopsFormat = "yaml";
    age = {
      keyFile = "/var/lib/sops/keys.txt";
      generateKey = false;
      sshKeyPaths = [ ];
    };

    secrets = {
      homelab-wireguard-private-key = { };

      restic-repository = { };
      restic-password = { };
      paperless-admin-password = { };

      # postgres passwords
      networth-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
      networth_ro-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
      immich-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
      paperless-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
      nextcloud-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        mode = "440";
        # restartUnits = [ "postgresql.service" ];
      };
    };
  };
}
