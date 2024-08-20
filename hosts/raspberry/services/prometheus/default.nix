{ config, ... }:

{
  services.prometheus = {
    enable = true;
    alertmanagers = [ ];
    exporters = {
      systemd.enable = true;
      node.enabled = true;
      wireguard.enable = true;
    };
    scrapeConfigs = [
      {
        job_name = "systemd";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.systemd.port}" ];
        }];
      }
      {
        job_name = "rasp-nixos";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.node.port}" ];
        }];
      }
      {
        job_name = "wireguard";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.wireguard.port}" ];
        }];
      }
    ];
    stateDir = "prometheus2";
  };
}
