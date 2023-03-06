{ lib, config, ... }:

{
  virtualisation.oci-containers.containers."cloudflare-ddns" = {
    image = "timothyjmiller/cloudflare-ddns:latest";
    environment = { PUID = "1000"; PGID = "1000"; };
    autoStart = true;
    extraOptions = [ "--security-opt" "no-new-privileges" "--memory=32Mi" ];
    volumes = [ "/var/volumes/cloudflare-ddns/config.json:/config.json" ];
  };
}
