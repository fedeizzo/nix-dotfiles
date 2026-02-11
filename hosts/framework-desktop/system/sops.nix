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
      local-restic-password = { };
      paperless-admin-password = { };
    };
  };
}
