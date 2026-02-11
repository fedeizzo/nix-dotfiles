{ hostname, ... }:

{
  users.users.loki.uid = 994; # make backup consistent across machines
  services.loki = {
    enable = true;
    dataDir = "/var/volumes/loki";
    configuration = {
      auth_enabled = false;
      server = {
        http_listen_port = 3100;
        grpc_listen_port = 9096;
      };
      limits_config = {
        reject_old_samples = false;
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
      pattern_ingester = {
        enabled = true;
      };
    };
  };

  users.users.promtail.uid = 992; # make backup consistent across machines
  services.promtail = {
    enable = true;
    configuration = {
      global = {
        file_watch_config = { min_poll_frequency = "250ms"; max_poll_frequency = "250ms"; };
      };
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
              host = "${hostname}";
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
          job_name = "traefik-logs";
          static_configs = [
            {
              targets = [ "localhost" ];
              labels = {
                job = "traefik";
                __path__ = "/var/volumes/traefik/log/traefik.json";
              };
            }
          ];
          pipeline_stages = [
            {
              json = {
                expressions = { level = "level"; time = "time"; message = "message"; };
              };
            }
            { labels = { level = ""; }; }
            # { timestamp = { source = "time"; format = "2024-08-20T18:28:00+02:00"; }; }
          ];
        }
        {
          job_name = "traefik-access";
          static_configs = [
            {
              targets = [ "localhost" ];
              labels = {
                job = "traefik-access";
                __path__ = "/var/volumes/traefik/log/access.json";
              };
            }
          ];
          pipeline_stages = [
            {
              json = {
                expressions = {
                  RequestHost = "host";
                  RequestPath = "path";
                  RequestMethod = "method";
                  ClientHost = "client_ip";
                  time = "time";
                  level = "level";
                  msg = "msg";
                };
              };
            }
            {
              geoip = {
                db = "/var/volumes/promtail/GeoLite2-City.mmdb";
                db_type = "city";
                source = "client_ip";
              };
            }
            {
              timestamp = { source = "time"; format = "RFC3339Nano"; };
            }
            { labels = { level = ""; host = ""; }; }
          ];
        }
      ];
    };
  };
}
