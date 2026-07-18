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
          api_file = config.sops.secrets.pan-fastmail.path;
        };
        lunchmoney = {
          api_file = config.sops.secrets.pan-lunchmoney.path;
        };
        fusion = {
          endpoint = "https://fusion.fedeizzo.dev/api";
          password_file = config.sops.secrets.pan-fusion.path;
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

        telemetry = {
          port = "35556";
        };

        hindsight = {
          url = "path";
          api_key = "placeholder";
          bank_id = "pan";
        };

        jobs = [
          {
            name = "transaction";
            spec = "*/5 10-20 * * *";
            condition = "lunchmoney:has_unreviewed";
            prompt = "Get latest lunchmoney transaction and review it.";
            runner = "lunchmoney";
          }
          {
            name = "fusion_triage";
            spec = "0 8 * * *";
            condition = "fusion:has_unread";
            prompt = "Please fetch my latest unread RSS feeds and present a summary.";
            runner = "fusion";
          }
        ];
      };
    };

    # We reuse the secrets defined in the old pan configuration, or we could duplicate them.
    # We will assume they are defined globally or we can define them here if the old pan is disabled.

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
