{ lib, config, kubernetesOrderString, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.authelia.applicationOrder; });
in
{
  config = mkIf (config.fiCluster.services.authelia.enable) {
    environment.etc.authelia-deployment = {
      enable = true;
      source = ./authelia-deployment.yaml;
      target = "homelab-kubernetes/${order}-authelia-deployment.yaml";
    };
  };
}
