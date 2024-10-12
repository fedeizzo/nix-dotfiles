{ pkgs, username, ... }:

{
  environment.systemPackages = with pkgs; [ sops age ];
  sops = {
    defaultSopsFile = ../../../secrets/x1-carbon-secrets.yaml;
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
  };
}
