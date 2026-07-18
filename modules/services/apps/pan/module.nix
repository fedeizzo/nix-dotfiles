{
  flake.modules.nixos.apps-pan = { pkgs, config, lib, ... }:
    let
      cfg = config.services.apps-pan;
      format = pkgs.formats.yaml { };

      package = pkgs.rustPlatform.buildRustPackage {
        pname = "pan";
        version = "0.1.0";
        src = lib.cleanSource ./.;

        cargoLock = {
          lockFile = ./Cargo.lock;
          allowBuiltinFetchGit = true;
        };
      };
    in
    {
      options.services.apps-pan = {
        enable = lib.mkEnableOption "Pan service (Rust)";

        settings = lib.mkOption {
          type = format.type;
          default = { };
          description = ''
            Configuration for Pan, written to config.yaml.
          '';
        };

        dataDir = lib.mkOption {
          type = lib.types.str;
          default = "/var/lib/pan-rust";
          description = "Data directory for Pan, used as the working directory.";
        };
      };

      config = lib.mkIf cfg.enable {
        systemd.services.apps-pan = {
          description = "Pan Service (Rust)";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          serviceConfig = {
            ExecStart = "${package}/bin/pan --config ${format.generate "pan-config.yaml" cfg.settings}";
            WorkingDirectory = cfg.dataDir;
            StateDirectory = "pan-rust";
            User = "pan-rust";
            Group = "pan-rust";
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
        
        users.users.pan-rust = {
          uid = 952;
          group = "pan-rust";
          isSystemUser = true;
        };

        users.groups.pan-rust = { gid = 952; };
      };
    };
}
