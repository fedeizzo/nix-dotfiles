{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.cloudflare-ddns.applicationOrder; });
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.cloudflare-ddns.enable; });
in
{
  config = {
    environment.etc.cloudflare-ddns-deployment = {
      enable = true;
      source = ./cloudflare-ddns-deployment.yaml;
      target = "homelab-kubernetes/${order}-cloudflare-ddns-deployment-${suffix}.yaml";
    };
  };
}
