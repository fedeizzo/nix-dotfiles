{ pkgs, config, ... }:

{
  virtualisation.oci-containers.containers."diun" = {
    image = "crazymax/diun:latest";
    autoStart = true;
    extraOptions = [ "--memory=128m" ];
    environmentFiles = [ "/var/container_envs/diun" ];
    volumes = [
      "/var/volumes/diun:/data"
      "/var/run/docker.sock:/var/run/docker.sock"
    ];
  };
}
