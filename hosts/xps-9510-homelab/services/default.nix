{ config, pkgs, ... }:

let
  docker = config.virtualisation.oci-containers.backend;
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./blocky
    ./fedeizzo.dev
    ./fusion
    ./garmindb
    ./glance
    ./grafana
    ./hass
    ./immich
    ./influxdb
    ./logrotate
    ./loki
    ./net-worth
    ./nextcloud
    ./paperless
    ./postgres
    ./prometheus
    # ./sunshine
    ./sftpgo
    ./streaming
    ./traefik
  ];

  config = {
    system.activationScripts.networth = ''
      ${dockerBin} network inspect networth >/dev/null 2>&1 || ${dockerBin} network create networth --subnet 172.20.0.0/16
    '';
  };
}
