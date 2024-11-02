{ config, hostname, ... }:

{
  services.prometheus = {
    enable = true;
    retentionTime = "30d";
    alertmanagers = [ ];
    exporters = {
      systemd.enable = true;
      node.enable = true;
      wireguard.enable = true;
      smartctl = {
        enable = true;
        devices = [ "/dev/sda" ];
      };
      restic = {
        enable = true;
        repository = "b2:fedeizzo-homelab-backup";
        environmentFile = "/root/.restic_backup_env";
        passwordFile = config.sops.secrets.restic-password.path;
      };
    };
    scrapeConfigs = [
      {
        job_name = "systemd";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.systemd.port}" ];
        }];
      }
      {
        job_name = "${hostname}";
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
      {
        job_name = "traefik";
        static_configs = [{
          targets = [ "127.0.0.1:8082" ];
        }];
      }
      {
        job_name = "sftpgo";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.sftpgo.settings.telemetry.bind_port}" ];
        }];
      }
      {
        job_name = "smartctl";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.smartctl.port}" ];
        }];
      }
      {
        job_name = "restic";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.prometheus.exporters.restic.port}" ];
        }];
      }
      {
        job_name = "blocky";
        static_configs = [{
          targets = [ "127.0.0.1:${toString config.services.blocky.settings.ports.http}" ];
        }];
      }
    ];
    stateDir = "prometheus2";
  };
}
