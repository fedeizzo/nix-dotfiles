{
  flake.modules.nixos.n8n = { config, lib, ... }: {
    services.n8n = {
      enable = true;

      environment = {
        GENERIC_TIMEZONE = "Europe/Paris";

        N8N_PORT = "22553";

        N8N_DIAGNOSTICS_ENABLED = "false";
        N8N_VERSION_NOTIFICATIONS_ENABLED = "false";
        N8N_HIRING_BANNER_ENABLED = "false";
        N8N_PUBLIC_API_SWAGGERUI_DISABLED = "true";

        # N8N_AI_ASSISTANT_BASE_URL = "https://ai-assistant.n8n.io";
      };
    };
    services.redis.servers.n8n = {
      enable = true;
      port = 52232;
    };
    # for consistent backup
    systemd.services.n8n.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "n8n";
      Group = "n8n";
    };

    fi.services = [
      {
        name = "n8n";
        port = (lib.strings.toInt config.services.n8n.environment.N8N_PORT);
        dashboardSection = "Tools";
        toPersist = [
          {
            directory = "/var/lib/n8n";
            user = "n8n";
            group = "n8n";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/redis-n8n";
            user = "redis-n8n";
            group = "redis-n8n";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/n8n"
          "/persist/var/lib/redis-n8n"
        ];
      }
    ];

    users.users.n8n = {
      uid = 288;
      group = "n8n";
    };
    users.groups.n8n.gid = 288;
  };
}
