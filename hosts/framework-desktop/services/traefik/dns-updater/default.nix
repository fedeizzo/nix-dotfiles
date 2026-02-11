{ lib, config, pkgs, ... }:

let
  groupedServices = builtins.groupBy (service: toString service.isExposed) config.fi.services;

  getHost = service: (lib.strings.concatStringsSep "." ((lib.lists.optional (service.subdomain != null) service.subdomain) ++ [ "fedeizzo.dev" ]));

  exposedServices = groupedServices."1";
  notExposedServices = groupedServices."";

  exposedDomains = lib.strings.concatStringsSep " " (map getHost exposedServices);
  notExposedDomains = lib.strings.concatStringsSep " " (map getHost notExposedServices);

  dns-updater = (pkgs.callPackage ./build.nix { });
in
{
  environment.systemPackages = [ dns-updater ];

  systemd.services.dns-updater = {
    enable = true;
    description = "dns-updater";
    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];
    # restartTriggers = [
    #   # config.sops.secrets.dns-updater-zone-id.sopsFileHash
    #   # config.sops.secrets.dns-updater-password.sopsFileHash
    #   exposedDomains
    #   notExposedDomains
    # ];

    serviceConfig = {
      User = "dns-updater";
      Group = "keys";
      DynamicUser = true;
      RuntimeDirectoryMode = "0400";
      Type = "oneshot";
      ExecStart = "${lib.getExe dns-updater} -token-path '${config.sops.secrets.dns-updater-password.path}' -zone-id-path '${config.sops.secrets.dns-updater-zone-id.path}' -exposed '${exposedDomains}' -internal '${notExposedDomains}'";
      StateDirectory = "dns-updater";
    };
  };

  systemd.timers.dns-updater = {
    enable = false;
    description = "Run dns-updater";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "1h";
      OnUnitInactiveSec = "1h";
    };
  };

  sops.secrets.dns-updater-password = {
    format = "yaml";
    mode = "0440";
    group = "keys";
    sopsFile = ../traefik-homelab-secrets.yaml;
  };
  sops.secrets.dns-updater-zone-id = {
    format = "yaml";
    mode = "0440";
    group = "keys";
    sopsFile = ../traefik-homelab-secrets.yaml;
  };
}
