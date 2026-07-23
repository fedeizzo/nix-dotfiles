{
  flake.modules.nixos.logrotate = { ... }: {
    services.logrotate = {
      enable = true;
      settings = {
        "/var/volumes/traefik/log/access.json" = {
          size = "100M";
          rotate = 5;
          missingok = true;
          notifempty = true;
          postrotate = ''
            systemctl kill --signal=USR1 traefik
          '';
        };
        "/var/log/audit/audit.log" = {
          size = "10M";
          rotate = 5;
          missingok = true;
          notifempty = true;
          frequency = "hourly";
          # 2. Use copytruncate so auditd doesn't lose its file handle,
          # OR send the correct SIGUSR1 signal. copytruncate is much safer
          # when external tools manage audit logs.
          copytruncate = true;
          postrotate = ''
            # Send SIGUSR1 if you prefer not to use copytruncate
            # systemctl kill -s USR1 auditd
          '';
        };
        "/var/log/pan/pan.log" = {
          size = "10M";
          rotate = 5;
          missingok = true;
          notifempty = true;
        };
      };
    };
  };
}
