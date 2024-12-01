{ inputs, config, ... }:

{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];
  sops = {
    defaultSopsFile = ../../../secrets/raspberry-secrets.yaml;
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

      # postgres passwords
      networth-pg-password = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
      networth-pg-password-ro = {
        owner = config.systemd.services.postgresql.serviceConfig.User;
        group = config.systemd.services.postgresql.serviceConfig.Group;
        # restartUnits = [ "postgresql.service" ];
      };
    };
  };
}
