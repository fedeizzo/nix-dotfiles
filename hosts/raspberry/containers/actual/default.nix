{ pkgs, config, ... }:

{
  virtualisation.oci-containers.containers."actual" = {
    image = "actualbudget/actual-server";
    autoStart = true;
    extraOptions = [ "--network=homelab" ];
    volumes = [ "/var/volumes/actual-data:/var/lib/data" ];
  };
}
