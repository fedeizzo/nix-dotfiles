{ pkgs
, lib
, config
, kubernetesSuffixFile
, dockerNetworkScript
, ...
}:

with lib;
let
  enableOption = mkOption {
    type = types.bool;
    default = false;
    description = "Enable service";
  };
  appOrderOption = mkOption {
    type = types.ints.positive;
    default = 1;
    description = "Number used for name of the systemlink, used for kubectl apply order";
  };
  docker = "${config.virtualisation.oci-containers.backend}";
  dockerBin = "${pkgs.${docker}}/bin/${docker}";
in
{
  imports = [
    ./traefik
    # ./cert-manager
    # ./homer
    # ./authelia
    ./cloudflare-ddns
    ./fedeizzo.dev
    ./fireflyiii
    ./net-worth
    # ./homebox
    ./diun
    # ./grafana
    # ./openbooks
    # ./pi-hole
  ];

  options = {
    fiCluster.services = {
      traefik.enable = enableOption;
      traefik.applicationOrder = appOrderOption;
      cert-manager.enable = enableOption;
      cert-manager.applicationOrder = appOrderOption;
      authelia.enable = enableOption;
      authelia.applicationOrder = appOrderOption;
      cloudflare-ddns.enable = enableOption;
      cloudflare-ddns.applicationOrder = appOrderOption;
      homer.enable = enableOption;
      homer.applicationOrder = appOrderOption;
      fedeizzodev.enable = enableOption;
      fedeizzodev.applicationOrder = appOrderOption;
      pihole.enable = enableOption;
      pihole.applicationOrder = appOrderOption;
    };
  };

  config = {
    virtualisation.oci-containers.backend = "docker";
    system.activationScripts.mkHomelabNetwork = (dockerNetworkScript
      {
        dockerBin = dockerBin;
        networkName = "homelab";
      });
    # environment.etc.global-configmap = {
    #   enable = true;
    #   source = ./global-configmap.yaml;
    #   target = "homelab-kubernetes/00-global-configmap-${(kubernetesSuffixFile { isEnable = true; })}.yaml";
    # };
    # environment.systemPackages = [
    #   pkgs.k3shomelab-manager
    # ];
  };
}
