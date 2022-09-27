{ lib, config, kubernetesOrderString, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.homer.applicationOrder; });
in
{
  config = mkIf (config.fiCluster.services.homer.enable) {
    environment.etc.homer-deployment = {
      enable = true;
      source = ./homer-deployment.yaml;
      target = "homelab-kubernetes/${order}-01-homer-deployment.yaml";
    };
    environment.etc.homer-configmap = {
      enable = true;
      source = ./homer-configmap.yaml;
      target = "homelab-kubernetes/${order}-02-homer-configmap.yaml";
    };
  };
}
