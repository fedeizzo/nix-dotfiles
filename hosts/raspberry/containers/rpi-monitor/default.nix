{ lib, config, ... }:

with lib;
with builtins;
{
  config = mkIf (elem "rpi-monitor" config.fiCluser.services) {
    environment.etc.homer-deployment = {
      enable = true;
      source = ./rpi-monitor-deployment.yaml;
      target = "homelab-kubernetes/rpi-monitor-deployment.yaml";
    };
  };
}
