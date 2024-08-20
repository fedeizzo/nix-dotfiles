{ ... }:

{
  services.loki = {
    enable = true;
    dataDir = "/var/volumes/loki";
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_port = 3100;
        grpc_listen_port = 9096;
      };
      common = {
        instance_addr = "127.0.0.1";
        path_prefix = "/tmp/loki";
        replication_factor = 1;
        storage = {
          filesystem = {
            chunks_directory = "/tmp/loki/chunks";
            rules_directory = "/tmp/loki/rules";
          };
        };
        ring = {
          kvstore = {
            store = "inmemory";
          };
        };
      };
      query_range = {
        results_cache = {
          cache = {
            embedded_cache = {
              enabled = true;
              max_size_mb = 100;
            };
          };
        };
      };
      schema_config = {
        configs = [{
          from = "2020-10-24";
          store = "tsdb";
          object_store = "filesystem";
          schema = "v13";
          index = {
            prefix = "index_";
            period = "24h";
          };
        }];
      };
      ruler = {
        alertmanager_url = "http://localhost:9093";
      };
    };
  };

  services.promtail = {
    enable = true;
    configuration = {
      server = {
        http_listen_port = 28183;
        grpc_listen_port = 0;
      };
      positions = {
        filename = "/tmp/positions.yaml";
      };
      clients = [{
        url = "http://127.0.0.1:3100/loki/api/v1/push";
      }];
      scrape_configs = [
        {
          job_name = "journal";
          journal = {
            max_age = "12h";
            labels = {
              job = "systemd-journal";
              host = "rasp-nixos";
            };
          };
          relabel_configs = [
            {
              source_labels = [ "__journal__systemd_unit" ];
              target_label = "unit";
            }
            {
              source_labels = [ "__journal_priority" ];
              target_label = "priority";
            }
          ];
        }
        {
          job_name = "local-logs";
          static_configs = [
            {
              targets = [ "localhost" ];
              labels = {
                job = "traefik";
                __path__ = "/var/volumes/traefik/logs/traefik.json";
              };
            }
          ];
          pipeline_stages = {
            json = {
              expressions = { level = "level"; time = "time"; message = "message"; };
            };
            labels = { level = { }; };
            timestamp = { source = "time"; format = "2024-08-20T18:28:00+02:00"; };
          };
        }
      ];
    };
  };
}
