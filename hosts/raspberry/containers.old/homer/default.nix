{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.homer.applicationOrder; });
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.homer.enable; });
in
{
  config = {
    environment.etc.homer-deployment = {
      enable = true;
      source = ./homer-deployment.yaml;
      target = "homelab-kubernetes/${order}-01-homer-deployment-${suffix}.yaml";
    };
    environment.etc.homer-configmap = {
      enable = true;
      source = ./homer-configmap.yaml;
      target = "homelab-kubernetes/${order}-02-homer-configmap-${suffix}.yaml";
    };
  };
}
