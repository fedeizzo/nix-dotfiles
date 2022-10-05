{ lib, config, kubernetesOrderString, kubernetesSuffixFile, ... }:

with lib;
with builtins;
let
  order = (kubernetesOrderString { intOrder = config.fiCluster.services.cert-manager.applicationOrder; });
  suffix = (kubernetesSuffixFile { isEnable = config.fiCluster.services.cert-manager.enable; });
  secretSuffix = (kubernetesSuffixFile {
    isEnable = config.fiCluster.services.cert-manager.enable;
    isSops = true;
  });
in
{
  config = {
    environment.etc.cert-manager-secrets = {
      enable = true;
      source = ./cert-manager-secrets.yaml;
      target = "homelab-kubernetes/${order}-01-cert-manager-secrets-${secretSuffix}.yaml";
    };
    environment.etc.cert-manager-issuer-lets-encrypt = {
      enable = true;
      source = ./cert-manager-issuer-lets-encrypt.yaml;
      target = "homelab-kubernetes/${order}-02-cert-manager-issuer-lets-encrypt-${suffix}.yaml";
    };
    environment.etc.cert-manager-certificate = {
      enable = true;
      source = ./cert-manager-certificate.yaml;
      target = "homelab-kubernetes/${order}-03-cert-manager-certificate-${suffix}.yaml";
    };
  };
}
