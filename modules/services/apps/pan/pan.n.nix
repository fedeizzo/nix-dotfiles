{
  flake.modules.nixos.apps-pan-config = { config, lib, ... }: {
    services.apps-pan = {
      enable = false; # Set to true when ready to replace the old go service
      settings = {
        models = {
          name = "qwen27";
          openai_api_key = "placeholder";
          openai_base_url = "https://llama.fedeizzo.dev/v1";
        };

        fastmail = {
          session_url = "https://api.fastmail.com/jmap/session";
          api_file = config.sops.secrets.pan-fastmail.path;
        };
        lunchmoney = {
          api_file = config.sops.secrets.pan-lunchmoney.path;
        };
        interface = {
          type = "matrix";
        };

        matrix = {
          homeserver = "https://matrix.org";
          user = "@pan-agent:matrix.org";
          password_file = config.sops.secrets.pan-matrix.path;
          allowed_user = "@fedeizzo:matrix.org";
          allowed_room = "!nhvcPGpOUCObLvdqTp:matrix.org";
          data_dir = "${config.services.apps-pan.dataDir}/matrix";
          notification_room = "!nhvcPGpOUCObLvdqTp:matrix.org";
          message_retention = "168h";
        };

        log = {
          path = "log/pan.log";
          level = "info";
        };

        jobs = [
          {
            name = "transaction";
            spec = "*/5 10-20 * * *";
            condition = "lunchmoney:has_unreviewed";
            prompt = "Review the latest Lunch Money transaction.";
            runner = "lunchmoney";
          }
        ];

      };
    };

    sops.secrets = lib.genAttrs [ "pan-fastmail" "pan-matrix" "pan-lunchmoney" ] (name: {
      format = "yaml";
      mode = "0440";
      owner = config.systemd.services.apps-pan.serviceConfig.User;
      group = config.systemd.services.apps-pan.serviceConfig.Group;
      sopsFile = ./pan-homelab-secrets.yaml;
    });

    fi.services = [
      {
        name = "apps-pan";
        dashboardSection = "Tools";
        shouldBehindReverseProxy = false;
        shouldMonitorUptime = false;
        shouldBeInDashboard = false;
        toPersist = [
          {
            directory = config.services.apps-pan.dataDir;
            user = "pan-rust";
            group = "pan-rust";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.apps-pan.dataDir}"
        ];
      }
    ];
  };
}
