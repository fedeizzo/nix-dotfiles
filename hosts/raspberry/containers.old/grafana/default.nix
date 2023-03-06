{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.grafana.applicationOrder; });
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.grafana.enable; });
in
{
  config = {
    environment.etc.grafana-deployment = {
      enable = true;
      source = ./grafana-deployment.yaml;
      target = "homelab-kubernetes/${order}-01-grafana-deployment-${suffix}.yaml";
    };
  };
}
