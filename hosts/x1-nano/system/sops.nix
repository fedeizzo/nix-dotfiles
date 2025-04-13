{ pkgs, username, ... }:

{
  environment.systemPackages = with pkgs; [ sops age ];
  sops = {
    defaultSopsFile = ./oven-secrets.yaml;
    defaultSopsFormat = "yaml";
    age = {
      keyFile = "/var/lib/sops/keys.txt";
      generateKey = false;
      sshKeyPaths = [ ];
    };
  };

  sops.secrets = {
    "${username}-path" = {
      neededForUsers = true;
    };

    x1-wireguard-private-key = { };
  };
}
