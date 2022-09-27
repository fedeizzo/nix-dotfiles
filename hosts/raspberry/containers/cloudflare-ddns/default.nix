{ lib, config, kubernetesOrderString, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.cloudflare-ddns.applicationOrder; });
in
{
  config = mkIf (config.fiCluster.services.cloudflare-ddns.enable) {
    environment.etc.cloudflare-ddns-deployment = {
      enable = true;
      source = ./cloudflare-ddns-deployment.yaml;
      target = "homelab-kubernetes/${order}-cloudflare-ddns-deployment.yaml";
    };
  };
}
