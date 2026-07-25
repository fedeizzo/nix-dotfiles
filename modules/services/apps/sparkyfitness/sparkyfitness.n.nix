{
  flake-file.inputs.sparkyfitness.url = "github:CodeWithCJ/SparkyFitness";

  flake.modules.nixos.sparkyfitness = { inputs, pkgs, config, lib, pkgs-unstable ? pkgs, ... }: {
    imports = [
      inputs.sparkyfitness.nixosModules.sparkyfitness
    ];

    services.sparkyfitness = {
      enable = true;
      port = 55221;
      user = "sparkyfitness";
      group = "sparkyfitness";
      stateDir = "/var/lib/sparkyfitness";

      frontendUrl = "https://fitness.fedeizzo.dev";
      environmentFile = config.sops.secrets.sparkyfitness.path;
      nginx.enable = true;
      nginx.virtualHost = "fitness.fedeizzo.dev";

      extraEnvironment = {
        SPARKY_FITNESS_DISABLE_EMAIL_LOGIN = "false"; # TODO disable after first login
        SPARKY_FITNESS_DISABLE_SIGNUP = "false"; # TODO same
        SPARKY_FITNESS_PUBLIC_API_DOCS = "false";
        # SPARKY_FITNESS_API_KEY_RATELIMIT_WINDOW_MS = "60000"; # 1 minute
        # SPARKY_FITNESS_API_KEY_RATELIMIT_MAX_REQUESTS = "10";
        SPARKY_FITNESS_API_KEY_RATELIMIT_WINDOW_MS = "60000"; # 1 minute
        SPARKY_FITNESS_API_KEY_RATELIMIT_MAX_REQUESTS = "100000";

        GARMIN_MICROSERVICE_URL = "http://127.0.0.1:55223";

        # TODO setup oidc
      };

      database = {
        createLocally = false;
        user = "sparkyfitness";
        appUser = "sparky_app";
        name = "sparkyfitness";
        host = "localhost";
        port = 5432;
      };
    };


    systemd.services.sparkyfitness-garmin =
      let
        sparkyfitness-garmin = pkgs-unstable.callPackage ./sparkyfitness-garmin.default { };
      in
      {
        description = "SparkyFitness Garmin Backend API";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          ExecStart = "${sparkyfitness-garmin}/bin/sparkyfitness-garmin --host 127.0.0.1 --port 55223";
          ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p mock_data";
          User = config.users.users.sparkyfitness.name;
          Group = config.users.groups.sparkyfitness.name;
          WorkingDirectory = "/var/lib/sparkyfitness-garmin";
          StateDirectory = "sparkyfitness-garmin";
          Restart = "always";
          RestartSec = "5";
        };
      };

    sops.secrets.sparkyfitness = lib.mkIf config.services.sparkyfitness.enable {
      format = "dotenv";
      mode = "0400";
      owner = config.users.users.sparkyfitness.name;
      group = config.users.groups.sparkyfitness.name;
      restartUnits = [ "sparkyfitness.service" ];
      sopsFile = ./sparkyfitness-homelab-secrets.env;
      key = ""; # to map the whole file as a secret
    };

    fi.services = [
      {
        name = "sparky-fitness";
        shouldMonitorUptime = false;
        subdomain = "fitness";
        port = 55222;
        dashboardSection = "Personal";
        toPersist = [
          {
            directory = config.services.sparkyfitness.stateDir;
            user = "sparkyfitnes";
            group = "sparkyfitnes";
            mode = "u=rwx,g=rx,o=";
          }
          {
            directory = "/var/lib/sparkyfitness-garmin";
            user = "sparkyfitnes";
            group = "sparkyfitnes";
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.sparkyfitness.stateDir}"
          "/persist/var/lib/sparkyfitness-garmin"
        ];
      }
    ];

    users.users.sparkyfitness = {
      uid = 973;
      group = "sparkyfitness";
    };
    users.groups.sparkyfitness.gid = 973;

    services.nginx.virtualHosts."fitness.fedeizzo.dev" = {
      listen = [{ addr = "127.0.0.1"; port = 55222; }];
    };
  };
}
