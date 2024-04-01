{ inputs, ... }:

{
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];
  sops = {
    defaultSopsFile = ../../../secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];
  };
  sops.secrets.restic-repository = {
    sopsFile = ../../../secrets/raspberry-secrets.yaml;
    format = "yaml";
  };
  sops.secrets.restic-password = {
    sopsFile = ../../../secrets/raspberry-secrets.yaml;
    format = "yaml";
  };
  sops.secrets.laptop-ssh-public-key.sopsFile = ../../../secrets.yaml;
  sops.secrets.deploy-homelab-ssh-public-key.sopsFile = ../../../secrets/raspberry-secrets.yaml;
  sops.secrets.homelab-wireguard-private-key.sopsFile = ../../../secrets.yaml;
}
