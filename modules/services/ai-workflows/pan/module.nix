{
  flake.modules.nixos.pan = { pkgs, config, lib, ... }:
    let
      cfg = config.services.pan;
      format = pkgs.formats.yaml { };

      package = pkgs.buildGoModule {
        pname = "pan";
        version = "0.1.0";
        src = lib.cleanSource ./.;

        tags = [ "goolm" ];
        vendorHash = "sha256-6EJ8XREKsrBz+LszzYPJ0y6v6OlFOYR0A31EmX91cZI=";
      };
    in
    {
      options.services.pan = {
        enable = lib.mkEnableOption "Pan service";

        settings = lib.mkOption {
          type = format.type;
          default = { };
          description = ''
            Configuration for Pan, written to config.yaml.
          '';
        };

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/pan";
          description = "Data directory for Pan, used as the working directory.";
        };
      };

      config = lib.mkIf cfg.enable {
        systemd.services.pan = {
          description = "Pan Service";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          serviceConfig = {
            ExecStart = "${package}/bin/pan --config ${format.generate "pan-config.yaml" cfg.settings}";
            WorkingDirectory = cfg.dataDir;
            StateDirectory = "pan";
            User = "pan";
            Group = "pan";
            Restart = "on-failure";

            # Hardening
            CapabilityBoundingSet = "";
            LockPersonality = true;
            MemoryDenyWriteExecute = true;
            NoNewPrivileges = true;
            PrivateDevices = true;
            PrivateMounts = true;
            PrivateTmp = true;
            PrivateUsers = true;
            ProtectClock = true;
            ProtectControlGroups = true;
            ProtectHome = true;
            ProtectHostname = true;
            ProtectKernelLogs = true;
            ProtectKernelModules = true;
            ProtectKernelTunables = true;
            ProtectSystem = "strict";
            RemoveIPC = true;
            RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
            RestrictNamespaces = true;
            RestrictRealtime = true;
            RestrictSUIDSGID = true;
            SystemCallArchitectures = "native";
            SystemCallFilter = [ "@system-service" "~@privileged" ];
            UMask = "0077";
          };
        };
      };
    };
}
