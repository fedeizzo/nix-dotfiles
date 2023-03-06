{ pkgs
, lib
, config
, kubernetesOrderString
, kubernetesSuffixFile
, dockerNetworkScript
, ...
}:

with lib;
with builtins;
let
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.fedeizzodev.enable; });
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.fedeizzodev.applicationOrder; });
in
{
  config = {
    environment.etc.fedeizzodev-deployment = {
      enable = true;
      source = ./fedeizzodev-deployment.yaml;
      target = "homelab-kubernetes/${order}-fedeizzodev-deployment-${suffix}.yaml";
    };

    system.activationScripts.mkFedeizzodevNetwork = (dockerNetworkScript
      {
        pkgs = pkgs;
        docker = "${config.virtualisation.oci-containers.backend}";
        networkName = "fedeizzodev";
      });
    virtualisation.oci-containers.containers."fedeizzodev" = {
      image = "fedeizzo/website:latest";
      ports = [ "45231:80" ];
      autoStart = true;
      extraOptions = [ "--network=fedeizzodev" ];

    };
  };
}
