{ inputs, ... }:

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
      sftpgo-pg-password = {
        # owner = config.systemd.services.postgresql.serviceConfig.User;
        restartUnits = [ "postgresql.service" ];
      };
    };
  };
}
