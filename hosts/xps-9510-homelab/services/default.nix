{ config, pkgs, ... }:

let
  docker = config.virtualisation.oci-containers.backend;
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./blocky
    ./diun
    ./fedeizzo.dev
    ./garmindb
    ./grafana
    ./immich
    ./logrotate
    ./loki
    ./net-worth
    ./postgres
    ./prometheus
    ./sunshine
    ./sftpgo
    ./traefik
  ];

  config = {
    system.activationScripts.networth = ''
      ${dockerBin} network inspect networth >/dev/null 2>&1 || ${dockerBin} network create networth --subnet 172.20.0.0/16
    '';
  };
}
