{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.traefik.applicationOrder; });
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.traefik.enable; });
  secretSuffix = (kubernetesSuffixFile {
    isEnable = config.fiCluster.services.traefik.enable;
    isSops = true;
  });
in
{
  config = {
    environment.etc.traefik-internal-config = {
      enable = true;
      source = ./traefik-internal-config.yaml;
      target = "homelab-kubernetes/internal-configs/traefik.yml";
    };
    environment.etc.traefik-role = {
      enable = true;
      source = ./traefik-role.yaml;
      target = "homelab-kubernetes/${order}-01-traefik-role-${suffix}.yaml";
    };
    environment.etc.traefik-account = {
      enable = true;
      source = ./traefik-account.yaml;
      target = "homelab-kubernetes/${order}-02-traefik-account-${suffix}.yaml";
    };
    environment.etc.traefik-deployment = {
      enable = true;
      source = ./traefik-deployment.yaml;
      target = "homelab-kubernetes/${order}-03-traefik-deployment-${suffix}.yaml";
    };
    environment.etc.traefik-middleware = {
      enable = true;
      source = ./traefik-middleware.yaml;
      target = "homelab-kubernetes/${order}-03-traefik-middleware-${suffix}.yaml";
    };
  };
}
