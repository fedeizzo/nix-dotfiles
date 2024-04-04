{ inputs, ... }:

{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];
  sops = {
    defaultSopsFile = ../../../secrets/raspberry-secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];

    secrets = {
      homelab-wireguard-private-key = { };

      restic-repository = { };
      restic-password = { };
    };
  };
}
