{
  # daily gufo metrics dashboard, built by CI in the private gufo-org/metrics repo
  flake.modules.nixos.gufo-metrics = { config, lib, pkgs, ... }:
    let
      port = 34998;
      host = "metrics.gufo.blog";
      root = "/var/lib/gufo-metrics";
    in
    {
      # CI commits dashboard.html at 03:17 UTC; pull it shortly after
      systemd.services.gufo-metrics-fetch = {
        description = "Fetch gufo metrics dashboard";
        wantedBy = [ "multi-user.target" ]; # refresh on boot too
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        path = [ pkgs.curl ];
        script = ''
          curl -fsSL --retry 5 --retry-all-errors \
            -H "Authorization: Bearer $(head -n1 "$CREDENTIALS_DIRECTORY/token")" \
            -H "Accept: application/vnd.github.raw+json" \
            -o ${root}/index.html.tmp \
            https://api.github.com/repos/gufo-org/metrics/contents/dashboard.html
          mv ${root}/index.html.tmp ${root}/index.html
        '';
        serviceConfig = {
          Type = "oneshot";
          User = "gufo-metrics";
          Group = "gufo-metrics";
          StateDirectory = "gufo-metrics";
          LoadCredential = "token:${config.sops.secrets.gufo-metrics-github-token.path}";
        };
      };

      systemd.timers.gufo-metrics-fetch = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-* 04:00:00 UTC";
          Persistent = true;
          RandomizedDelaySec = "10m";
        };
      };

      systemd.services.gufo-metrics = {
        description = "Serve gufo metrics dashboard";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${lib.getExe pkgs.static-web-server} --host 127.0.0.1 --port ${toString port} --root ${root} --directory-listing false";
          User = "gufo-metrics";
          Group = "gufo-metrics";
          StateDirectory = "gufo-metrics";
          Restart = "on-failure";
          NoNewPrivileges = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
        };
      };

      users.users.gufo-metrics = { isSystemUser = true; group = "gufo-metrics"; };
      users.groups.gufo-metrics = { };

      # fine-grained PAT, gufo-org/metrics only, Contents: read
      sops.secrets.gufo-metrics-github-token = {
        format = "yaml";
        mode = "0400";
        restartUnits = [ "gufo-metrics-fetch.service" ];
        sopsFile = ./gufo-metrics-homelab-secrets.yaml;
      };

      fi.services = [
        {
          name = "gufo-metrics";
          isExposed = true;
          useDefaultDomain = false;
          extraHosts = [ host ];
          authType = "proxy";
          host = "127.0.0.1"; # server binds ipv4 loopback only
          inherit port;
          dashboardSection = "Exposed";
          dashboardIcon = "grafana";
          toPersist = [
            {
              directory = root;
              user = "gufo-metrics";
              group = "gufo-metrics";
              mode = "u=rwx,g=rx,o=";
            }
          ];
        }
      ];
    };
}
