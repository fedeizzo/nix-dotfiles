{ config, pkgs, ... }:

let
  docker = config.virtualisation.oci-containers.backend;
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./blocky
    ./traefik
    ./garmindb
    ./grafana
    ./logrotate
    ./loki
    ./fedeizzo.dev
    ./net-worth
    ./prometheus
    ./diun
  ];

  config = {
    virtualisation.oci-containers.backend = "docker";
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;
        enableNvidia = false;
      };
      podman.enable = false;
    };

    system.activationScripts.networth = ''
      ${dockerBin} network inspect networth >/dev/null 2>&1 || ${dockerBin} network create networth --subnet 172.20.0.0/16
    '';

  };
}
