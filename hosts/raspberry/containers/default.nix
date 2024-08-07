{ pkgs, config, dockerNetworkScript, ... }:

let
  docker = "${config.virtualisation.oci-containers.backend}";
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./blocky
    ./traefik
    ./fedeizzo.dev
    ./net-worth
    # ./prometheus
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

    system.activationScripts.mkHomelabNetwork = (dockerNetworkScript
      {
        dockerBin = dockerBin;
        networkName = "homelab";
      });
  };
}
