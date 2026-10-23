{
  flake.modules.nixos.apps-pan = { pkgs, config, lib, inputs, ... }:
    let
      cfg = config.services.apps-pan;
      format = pkgs.formats.yaml { };

      package = pkgs.rustPlatform.buildRustPackage {
        pname = "pan";
        version = "0.1.0";
        src = lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let
              name = baseNameOf path;
            in
              !builtins.elem name [
                ".agents"
                "config.yaml"
                "frontend"
                "pan-homelab-secrets.yaml"
                "pan.log"
                "target"
              ];
        };

        cargoLock = {
          lockFile = ./Cargo.lock;
          allowBuiltinFetchGit = true;
        };

        # HTTP clients load a CA bundle even when tests use local HTTP mocks.
        preCheck = ''
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
        '';
      };
    in
    {
      imports = [ inputs.self.modules.nixos.apps-pan-worker ];
      options.services.apps-pan = {
        enable = lib.mkEnableOption "Pan service (Rust)";

        package = lib.mkOption {
          type = lib.types.package;
          default = package;
          defaultText = lib.literalExpression "the bundled Pan Rust package";
          description = "Pan package to execute.";
        };

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
        systemd.services.apps-pan = {
          description = "Pan Service (Rust)";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          serviceConfig = {
            ExecStart = "${cfg.package}/bin/pan --config ${format.generate "pan-config.yaml" cfg.settings} serve";
            WorkingDirectory = cfg.dataDir;
            StateDirectory = lib.removePrefix "/var/lib/" cfg.dataDir;
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
