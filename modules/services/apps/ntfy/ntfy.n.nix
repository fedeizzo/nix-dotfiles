{
  flake.modules.nixos.ntfy = { config, lib, ... }: {
    services.ntfy-sh = {
      enable = true;
      settings = {
        # Server
        base-url = "https://ntfy.fedeizzo.dev";
        listen-http = "127.0.0.1:23445";
        behind-proxy = true;

        # Access control
        auth-file = "/var/lib/ntfy-sh/auth.db";
        enable-login = true;
        enable-signup = true;

        # Attachments
        attachment-cache-dir = "/var/cache/ntfy-sh/attachments";

        # Message cache
        cache-file = "/var/cache/ntfy-sh/cache.db";
      };
    };
    systemd.services.ntfy-sh.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "ntfy-sh";
      Group = "ntfy-sh";
    };

    fi.services = [
      {
        name = "ntfy";
        port = 23445;
        dashboardSection = "Tools";
        toPersist = [
          {
            directory = "/var/cache/ntfy-sh";
            user = "ntfy-sh";
            group = "ntfy-sh";
            mode = "u=rwx,g=,o=";
          }
          {
            directory = "/var/lib/ntfy-sh";
            user = "ntfy-sh";
            group = "ntfy-sh";
            mode = "u=rwx,g=,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/ntfy-sh"
        ];
      }
    ];
    users.users.ntfy-sh = {
      uid = 959;
      group = "ntfy-sh";
    };
    users.groups.ntfy-sh.gid = 959;
  };
}
