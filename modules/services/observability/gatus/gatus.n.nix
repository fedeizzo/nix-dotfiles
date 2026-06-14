{
  flake.modules.nixos.gatus = { config, lib, ... }:
  let
    getHost = subdomain: (lib.strings.concatStringsSep "." ((lib.lists.optional (subdomain != null) subdomain) ++ [ "fedeizzo.dev" ]));

    # TODO: nix-dotfiles is currently excluded from monitoring, fix it later
    servicesToExclude = [
      "nix-dotfiles"
    ];

    servicesToMonitor = builtins.filter (service: service.shouldMonitorUptime && !(builtins.elem service.name servicesToExclude)) config.fi.services;

    # Map services from fi.services to Gatus endpoints for monitoring
    endpoints = map (service: {
      name = service.name;
      url = "https://${getHost service.subdomain}";
      interval = "1m";
      conditions = [
        "[STATUS] < 400"
      ];
      alerts = [
        {
          type = "ntfy";
        }
      ];
    }) servicesToMonitor;
  in
  {
    services.gatus = {
      enable = true;
      settings = {
        web.port = 8085;
        storage = {
          type = "sqlite";
          path = "/var/lib/gatus/data.db";
        };
        alerting = {
          ntfy = {
            url = "https://ntfy.fedeizzo.dev";
            topic = "alerts";
            "default-alert" = {
              "failure-threshold" = 3;
              "success-threshold" = 2;
              "send-on-resolved" = true;
            };
          };
        };
        ui = {
          title = "Homelab Status";
          header = "Homelab Status";
          description = "Real-time health status of fedeizzo.dev homelab services";
        };
        inherit endpoints;
      };
    };

    # Use a static user for consistent persistence and backup
    systemd.services.gatus.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "gatus";
      Group = "gatus";
    };

    fi.services = [
      {
        name = "gatus";
        port = config.services.gatus.settings.web.port;
        dashboardSection = "Observability";
        toPersist = [
          {
            directory = "/var/lib/gatus";
            user = "gatus";
            group = "gatus";
            mode = "u=rwx,g=rx,o=rx";
          }
        ];
        toBackup = [
          "/persist/var/lib/gatus"
        ];
      }
    ];

    users.users.gatus = {
      uid = 63100;
      isSystemUser = true;
      group = "gatus";
    };
    users.groups.gatus.gid = 63100;
  };
}
