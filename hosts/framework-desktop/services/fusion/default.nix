{ config, ... }:

{
  virtualisation.oci-containers.containers."fusion" = {
    image = "ghcr.io/0x2e/fusion:latest";
    autoStart = true;
    extraOptions = [ "--memory=512m" ];
    environmentFiles = [ config.sops.secrets.fusion.path ];
    volumes = [ "/var/lib/fusion:/data" ];
    ports = [ "51000:8080" ];
  };


  sops.secrets.fusion = {
    format = "dotenv";
    mode = "0400";
    restartUnits = [
      "docker-fusion.service"
    ];
    sopsFile = ./fusion-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
