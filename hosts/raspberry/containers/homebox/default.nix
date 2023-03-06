{ pkgs, config, ... }:

{
  virtualisation.oci-containers.containers."homebox" = {
    # default port 7745
    image = "ghcr.io/hay-kot/homebox:latest";
    autoStart = true;
    extraOptions = [ "--network=homelab" "--memory=1024Mi" ];
    environmentFiles = [ "/var/container_envs/homebox" ];
    volumes = [ "/var/volumes/homebox:/data" ];
  };
}
