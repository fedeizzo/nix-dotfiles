{ ... }:
{
  # environment.etc.traefik-static-config = {
  #   enable = true;
  #   source = ./static-config.yaml;
  #   target = "traefik/static-config.yaml";
  # };
  # environment.etc.traefik-dynamic-config = {
  #   enable = true;
  #   source = ./dynamic-config.yaml;
  #   target = "traefik/dynamic-config.yaml";
  # };
  services.traefik = {
    enable = true;
    dataDir = "/var/volumes/traefik";
    group = "traefik";
    environmentFiles = "/var/container_envs/traefik";
    staticConfigFile = "./static-config.yaml";
    dynamicConfigFile = "./dynamic-config.yaml";
  };
  # virtualisation.oci-containers.containers."traefik" = {
  #   image = "traefik:v3.0";
  #   ports = [ "443:443" "8080:8080" ];
  #   autoStart = true;
  #   extraOptions = [ "--network=homelab" ];
  #   environmentFiles = [
  #     "/var/container_envs/traefik"
  #   ];
  #   volumes = [
  #     "/etc/traefik/static-config.yaml:/etc/traefik/traefik.yaml"
  #     "/etc/traefik/dynamic-config.yaml:/etc/traefik/dynamic-confs/dynamic-config.yaml"
  #     "/var/volumes/traefik/acme:/letsencrypt"
  #   ];
  # };
}
