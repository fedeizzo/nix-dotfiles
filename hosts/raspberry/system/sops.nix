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

    homelab-wireguard-private-key.sopsFile = { };

    restic-repository.sopsFile = { };
    restic-password.sopsFile = { };
  };
}
