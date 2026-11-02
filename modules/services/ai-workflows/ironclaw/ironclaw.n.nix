{
  flake.modules.nixos.ironclaw = { config, pkgs, lib, ... }:
    let
      port = 3020;
      stateDir = "/var/lib/ironclaw";

      # Upstream ships no flake and nixpkgs has no package: use the static musl release binary
      ironclaw = pkgs.stdenvNoCC.mkDerivation rec {
        pname = "ironclaw";
        version = "1.4.1";
        src = pkgs.fetchurl {
          url = "https://github.com/nearai/ironclaw/releases/download/ironclaw-v${version}/ironclaw-x86_64-unknown-linux-musl.tar.gz";
          hash = "sha256-wnRUuIanqPB10//bmb1vTXyvSI4DSin94Jg+JuWW/9s=";
        };
        installPhase = ''
          install -Dm755 ironclaw $out/bin/ironclaw
        '';
        meta.mainProgram = "ironclaw";
      };
    in
    {
      environment.systemPackages = [ ironclaw ];

      systemd.services.ironclaw = {
        description = "IronClaw AI agent";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];

        # The config is NOT managed by Nix: IronClaw creates and owns /var/lib/ironclaw/config.toml
        # (sparse first-run file), plus providers.json and its encrypted secret store in the same dir.
        # Iterate on it with the CLI as the service user, e.g.:
        #   sudo -u ironclaw env IRONCLAW_REBORN_HOME=/var/lib/ironclaw ironclaw models set-provider openai_compatible --model forge/qwen-flash
        #   sudo -u ironclaw env IRONCLAW_REBORN_HOME=/var/lib/ironclaw ironclaw config list
        # The CLI cannot set the endpoint: add `base_url = "https://llama.fedeizzo.dev/v1"` under [llm.default] in config.toml.
        # Then `systemctl restart ironclaw` (config writes never restart the service).
        # Precedence: compiled defaults < config.toml < env vars < CLI flags, so keep the env below minimal.
        # TODO: once the config is stable, port /var/lib/ironclaw/config.toml to a declarative Nix definition
        # (pkgs.formats.toml) and drop the provider/model setup from the CLI.
        environment = {
          HOME = stateDir;
          IRONCLAW_REBORN_HOME = stateDir;
          IRONCLAW_REBORN_WORKSPACE_ROOT = "${stateDir}/workspace";
          IRONCLAW_REBORN_PROFILE = "hosted-single-tenant-volume";
          # Must match [identity].default_owner if that gets set in config.toml
          IRONCLAW_REBORN_WEBUI_USER_ID = "ironclaw";
          IRONCLAW_REBORN_LOG = "info";

          # Referenced by [llm.default].api_key_env in config.toml; llama-swap ignores the value
          LLM_API_KEY = "placeholder";
          LLM_REQUEST_TIMEOUT_SECS = "600";
        };

        serviceConfig = {
          User = "ironclaw";
          Group = "ironclaw";
          StateDirectory = "ironclaw";
          StateDirectoryMode = "0750";
          ExecStart = "${lib.getExe ironclaw} serve --host 127.0.0.1 --port ${toString port}";
          # Provides IRONCLAW_REBORN_WEBUI_TOKEN (>= 32 bytes, also the session-signing key)
          EnvironmentFile = [ config.sops.secrets.ironclaw-env.path ];
          Restart = "always";
          RestartSec = 10;

          ProtectHome = true;
          ProtectSystem = "strict";
          PrivateTmp = true;
          NoNewPrivileges = true;
        };
      };

      sops.secrets.ironclaw-env = {
        format = "dotenv";
        mode = "0400";
        owner = "ironclaw";
        restartUnits = [ "ironclaw.service" ];
        sopsFile = ./ironclaw-homelab-secrets.env;
        key = ""; # Maps the entire env file as a secret
      };

      fi.services = [
        {
          name = "ironclaw";
          subdomain = "pan";
          inherit port;
          dashboardSection = "Tools";
          toPersist = [
            {
              directory = stateDir;
              user = "ironclaw";
              group = "ironclaw";
              mode = "u=rwx,g=rx,o=";
            }
          ];
          toBackup = [
            "/persist${stateDir}"
          ];
        }
      ];

      users.users.ironclaw = {
        isSystemUser = true;
        uid = 291;
        group = "ironclaw";
        home = stateDir;
      };
      users.groups.ironclaw.gid = 291;
    };
}
