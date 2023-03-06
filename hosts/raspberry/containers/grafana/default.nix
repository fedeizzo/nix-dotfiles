{ pkgs, config, dockerNetworkScript, ... }:

let
  docker = "${config.virtualisation.oci-containers.backend}";
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  system.activationScripts.mkGrafanaNetwork = (dockerNetworkScript
    {
      dockerBin = dockerBin;
      networkName = "grafana";
    });
  virtualisation.oci-containers.containers."grafana" = {
    image = "grafana/grafana:9.3.6";
    autoStart = true;
    ports = [ "3000:3000" ];
    extraOptions = [ "--network=grafana" ];
  };
  virtualisation.oci-containers.containers."loki" = {
    image = "grafana/loki:2.7.3";
    autoStart = true;
    ports = [ "3100:3100" ];
    extraOptions = [ "--network=grafana" ];
    volumes = [ "/var/volumes/loki:/mnt/config" ];
  };
  virtualisation.oci-containers.containers."promtail" = {
    image = "grafana/promtail:2.7.3";
    autoStart = true;
    extraOptions = [ "--network=grafana" ];
    volumes = [ "/var/volumes/promtail:/mnt/config" ];
  };
}
