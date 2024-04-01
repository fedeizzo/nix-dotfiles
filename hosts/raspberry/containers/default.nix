{ pkgs
, lib
, config
, kubernetesSuffixFile
, dockerNetworkScript
, ...
}:

with lib;
let
  docker = "${config.virtualisation.oci-containers.backend}";
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./traefik
    ./fedeizzo.dev
    # ./fireflyiii
    ./net-worth
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
