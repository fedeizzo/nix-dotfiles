{
  flake.modules.nixos.pan = { config, lib, ... }: {
    services.pan = {
      enable = true;
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
          endpoint = "https://fusion.fedeizzo.dev/api"; # Replace with actual fusion API URL
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
          data_dir = "${config.services.pan.dataDir}/matrix";
          notification_room = "!nhvcPGpOUCObLvdqTp:matrix.org";
          message_retention = "168h"; # 1 week
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
          # {
          #   name = "email";
          #   spec = "@every 3m";
          #   condition = "fastmail:has_unread";
          #   prompt = "Triage the last email in the inbox.";
          # }
          {
            name = "transaction";
            spec = "*/5 10-20 * * *"; # every 5 min between 10:00 and 18:00
            condition = "lunchmoney:has_unreviewed";
            prompt = "Get latest lunchmoney transaction and review it.";
            runner = "lunchmoney";
          }
          {
            name = "fusion_triage";
            spec = "0 8 * * *"; # every morning at 08:00
            condition = "fusion:has_unread";
            prompt = "Please fetch my latest unread RSS feeds and present a summary.";
            runner = "fusion";
          }
        ];
      };
    };

    users.users.pan = {
      uid = 951;
      group = "pan";
    };

    users.groups.pan = { gid = 951; };

    sops.secrets = lib.genAttrs [ "pan-fastmail" "pan-matrix" "pan-lunchmoney" "pan-fusion" ] (name: {
      format = "yaml";
      mode = "0440";
      owner = config.systemd.services.pan.serviceConfig.User;
      group = config.systemd.services.pan.serviceConfig.Group;
      sopsFile = ./pan-homelab-secrets.yaml;
    });

    fi.services = [
      {
        name = "pan";
        dashboardSection = "Tools";
        shouldBehindReverseProxy = false;
        shouldMonitorUptime = false;
        shouldBeInDashboard = false;
        toPersist = [
          {
            directory = config.services.pan.dataDir;
            user = "pan";
            group = "pan";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist${config.services.pan.dataDir}"
        ];
      }
    ];
  };
}
