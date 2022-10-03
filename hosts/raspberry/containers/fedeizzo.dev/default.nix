{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.traefik.enable; });
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.fedeizzodev.applicationOrder; });
in
{
  config = {
    environment.etc.fedeizzodev-deployment = {
      enable = true;
      source = ./fedeizzodev-deployment.yaml;
      target = "homelab-kubernetes/${order}-fedeizzodev-deployment-${suffix}.yaml";
    };
  };
}
