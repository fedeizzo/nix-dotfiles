{ lib, config, kubernetesOrderString, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.traefik.applicationOrder; });
in
{
  config = mkIf (config.fiCluster.services.traefik.enable) {
    environment.etc.traefik-role = {
      enable = true;
      source = ./traefik-role.yaml;
      target = "homelab-kubernetes/${order}-01-traefik-role.yaml";
    };
    environment.etc.traefik-account = {
      enable = true;
      source = ./traefik-account.yaml;
      target = "homelab-kubernetes/${order}-02-traefik-account.yaml";
    };
    environment.etc.traefik-deployment = {
      enable = true;
      source = ./traefik-deployment.yaml;
      target = "homelab-kubernetes/${order}-03-traefik-deployment.yaml";
    };
  };
}
