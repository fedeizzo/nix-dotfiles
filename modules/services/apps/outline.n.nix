{
  flake.modules.nixos.outline = { config, lib, ... }:
    let
      # env vars the nixos module can't take from a file, exported from sops before start
      secretEnv = {
        OIDC_CLIENT_ID = "outline-oidc-client-id";
        # github link previews, see https://docs.getoutline.com/s/hosting/doc/github-GchT3NNxI9
        GITHUB_CLIENT_ID = "outline-github-client-id";
        GITHUB_CLIENT_SECRET = "outline-github-client-secret";
        GITHUB_APP_NAME = "outline-github-app-name";
        GITHUB_APP_ID = "outline-github-app-id";
        GITHUB_APP_PRIVATE_KEY = "outline-github-app-private-key"; # base64-encoded pem
      };
    in
    {
    services.outline = {
      enable = true;
      port = 38461;
      publicUrl = "https://notes.gufo.blog";
      forceHttps = false; # TLS terminated by traefik

      # postgres and redis are created locally by the nixos module
      storage.storageType = "local";

      oidcAuthentication = {
        clientId = "overridden-at-runtime"; # real id loaded from sops, see script below
        clientSecretFile = config.sops.secrets.outline-oidc-client-secret.path;
        authUrl = "https://auth.gufo.blog/application/o/authorize/";
        tokenUrl = "https://auth.fedeizzo.dev/application/o/token/";
        userinfoUrl = "https://auth.fedeizzo.dev/application/o/userinfo/";
        scopes = [ "openid" "email" "profile" ];
        usernameClaim = "preferred_username";
        displayName = "Authentik";
      };
    };

    systemd.services.outline.script = lib.mkBefore (lib.concatStrings (lib.mapAttrsToList (env: secret: ''
      ${env}="$(head -n1 ${lib.escapeShellArg config.sops.secrets.${secret}.path})"
      export ${env}
    '') secretEnv));

    # /var/lib/outline is bind-mounted from /persist after tmpfiles run, hiding the data dir;
    # let systemd create it so ReadWritePaths exists when the namespace is set up
    systemd.services.outline.serviceConfig.StateDirectory = lib.mkForce [ "outline" "outline/data" ];

    # hardening, see https://github.com/outline/outline/blob/main/.env.sample
    systemd.services.outline.environment = {
      # nixos module sets this to "" which disables the limiter; upstream default is on
      RATE_LIMITER_ENABLED = lib.mkForce "true";
      RATE_LIMITER_REQUESTS = lib.mkForce "1000";
    };

    # the nixos module only sets ProtectSystem and UMask
    systemd.services.outline.serviceConfig = {
      NoNewPrivileges = true;
      CapabilityBoundingSet = "";
      PrivateDevices = true;
      ProtectHome = true;
      ProtectProc = "invisible";
      ProcSubset = "pid";
      ProtectClock = true;
      ProtectHostname = true;
      ProtectKernelLogs = true;
      ProtectKernelModules = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
      RestrictNamespaces = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      LockPersonality = true;
      SystemCallArchitectures = "native";
      SystemCallFilter = [ "@system-service" "~@privileged" "~@resources" ];
      # no MemoryDenyWriteExecute: node's JIT needs W+X pages
    };

    sops.secrets = lib.mkIf config.services.outline.enable (lib.genAttrs ([
      "outline-oidc-client-secret"
    ] ++ lib.attrValues secretEnv) (_: {
      format = "yaml";
      mode = "0400";
      owner = config.services.outline.user;
      group = config.services.outline.group;
      restartUnits = [ "outline.service" ];
      sopsFile = ./outline-homelab-secrets.yaml;
    }));

    fi.services = [
      {
        name = "outline";
        subdomain = "gufonotes";
        isExposed = true;
        extraHosts = [ "notes.gufo.blog" ];
        inherit (config.services.outline) port;
        dashboardSection = "Exposed";
        toPersist = [
          {
            directory = "/var/lib/outline";
            user = config.services.outline.user;
            group = config.services.outline.group;
            mode = "u=rwx,g=rx,o=";
          }
        ];
        toBackup = [
          "/persist/var/lib/outline"
        ];
      }
    ];
  };
}
