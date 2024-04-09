{ ... }:
{
  services.traefik = {
    enable = true;
    dataDir = "/var/volumes/traefik";
    group = "traefik";
    environmentFiles = [ "/var/container_envs/traefik" ];
    staticConfigFile = "./static-config.yaml";
    dynamicConfigFile = "./dynamic-config.yaml";
  };
}
