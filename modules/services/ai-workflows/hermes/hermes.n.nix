{
  flake.modules.nixos.hermes = { config, pkgs, lib, ... }:
    let
      # Dedicated, isolated Python environment specifically for skills
      skillsPythonEnv = pkgs.python3.withPackages (ps: with ps; [
        jsonschema
        requests
      ]);

      # Wrapper script that skills can call directly (e.g. `python-for-skills script.py`)
      pythonForSkills = pkgs.writeShellScriptBin "python-for-skills" ''
        exec ${skillsPythonEnv}/bin/python3 "$@"
      '';
      # model = "ds4";
      model = "qwen";

      # TODO remove after version is bumped to 0.14.5 in nixpkgs
      signal-cli-patched = pkgs.signal-cli.overrideAttrs (old: rec {
        version = "0.14.5";
        src = pkgs.fetchurl {
          url = "https://github.com/AsamK/signal-cli/releases/download/v${version}/signal-cli-${version}.tar.gz";
          hash = "sha256-YtOOv+85iNePQ35zKBg7de5UnRETguZsGvcNPr0816c=";
        };
      });
    in
    {
      services.hermes-agent = {
        enable = true;
        container.enable = lib.mkForce false;

        user = "hermes";
        group = "hermes";
        createUser = false;

        settings = {
          model = {
            default = "${model}";
            provider = "custom";
            base_url = "https://llama.fedeizzo.dev/v1";
          };

          stt = {
            enabled = true;
            provider = "openai";
            openai = {
              model = "Whisper-Large-v3-Turbo";
            };
          };

          memory = {
            memory_enable = true;
            user_profile_enable = true;
            provider = "hindsight";
          };

          plugins = {
            enabled = [ "hindsight" ];
          };

          skills = {
            guard_agent_created = true;
          };

          agent.disabled_toolsets = [
            "browser_automation"
            "video"
            "media_generation"
            "social"
            "multimodal_generation"
            "audio_output"
            "home_assistant"
            "computer_use"
            "delegation"
          ];
        };

        documents = {
          "SOUL.md" = ./SOUL.md;
        };

        extraPackages = [ pythonForSkills ];
        environmentFiles = [ config.sops.secrets.hermes-env.path ];
        addToSystemPackages = true;
        extraDependencyGroups = [ "hindsight" ];

        mcpServers = {
          lunchmoney = {
            command = "npx";
            args = [ "/var/lib/hermes/mcp-tools/node_modules/.bin/lunchmoney-mcp" ];
            env.LUNCHMONEY_API_TOKEN = "\${LUNCHMONEY_API_KEY}"; # resolved from .env
            env.LUNCHMONEY_DEBUG = "true";
            # Standard proxy routing
            env.http_proxy = "http://127.0.0.1:3128";
            env.https_proxy = "http://127.0.0.1:3128";
            env.HTTP_PROXY = "http://127.0.0.1:3128";
            env.HTTPS_PROXY = "http://127.0.0.1:3128";
            env.NODE_USE_ENV_PROXY = "1";
          };
        };

        environment = {
          NO_PROXY = "127.0.0.1,localhost,::1";
          HTTP_PROXY = "http://127.0.0.1:3128";
          HTTPS_PROXY = "http://127.0.0.1:3128";
          OPENAI_API_KEY = "placeholder";
          STT_OPENAI_API_KEY = "placeholder";
          STT_OPENAI_MODEL = "Whisper-Large-v3-Turbo";
          STT_OPENAI_BASE_URL = "https://lemonade.fedeizzo.dev/v1";
          HINDSIGHT_API_KEY = "";
          HINDSIGHT_API_URL = "https://hindsight-api.fedeizzo.dev";
          HINDSIGHT_AUTO_RETAIN = "false";

          # OPTIMIZATION: Turn off telemetry and auto-indexing loops
          # which bloat context drift across ongoing sessions.
          HERMES_DISABLE_TELEMETRY = "true";
          HERMES_MINIMAL_PROMPT = "true";
          # OPTIMIZATION: Only loads tool schemas dynamically on-demand
          # Can drop tool schema overhead from 40% of context down to 3%
          HERMES_TOOL_MODE = "search";
        };
      };

      # Systemd Hardening (Least Privilege Sandboxing)
      systemd.services.hermes-agent.serviceConfig = {
        # Mount the entire OS filesystem as read-only
        ProtectHome = lib.mkForce true; # Deny access to /home, protecting user data from local leaks
        PrivateTmp = lib.mkForce true; # Mount a separate, isolated /tmp directory
        NoNewPrivileges = lib.mkForce true; # Prevent the process and its children from gaining new privileges
        # Gives the service enough time to drain before systemd sends SIGKILL
        TimeoutStopSec = 210;
        IPAddressDeny = "any"; # Block all direct outbound network access at the systemd/kernel level
        IPAddressAllow = [
          "127.0.0.1/32"
          "::1/128"
          "127.0.0.53/32" # 1. Required: Allows systemd-resolved to handle DNS lookups
          "103.168.172.0/24" # 2. Required: Fastmail IMAP network cluster
          "202.12.124.0/24" #    Fastmail network cluster (Failover/Secondary)
          "204.75.18.0/23" #    Fastmail network cluster (Failover/Secondary)
        ];
      };

      # Egress Filtering via Squid Proxy
      services.squid = {
        enable = true;

        proxyAddress = "127.0.0.1";
        proxyPort = 3128;

        extraConfig = ''
          # 1. Allow CONNECT tunnels to the standard secure IMAP port
          acl SSL_ports port 993

          # 2. Define ACL for whitelisted domains the agent is allowed to access
          acl whitelist dstdomain .github.com
          acl whitelist dstdomain .fedeizzo.dev
          acl whitelist dstdomain .openrouter.ai
          acl whitelist dstdomain .osv.dev
          acl whitelist dstdomain .fastmail.com
          acl whitelist dstdomain .lunchmoney.dev

          ## npm and npx
          acl whitelist dstdomain .npmjs.org
          acl whitelist dstdomain .npmjs.com

          # 3. Deny any request that is NOT in the whitelist.
          http_access deny !whitelist
        '';
      };

      # Hindsight AI Memory system (deployed via Docker)
      virtualisation.oci-containers.containers.hindsight = {
        image = "ghcr.io/vectorize-io/hindsight:latest";
        extraOptions = [
          "--network=host"
        ];
        # The container is mostly stateless since memory lives in your Postgres!
        # We only mount the HuggingFace cache so it doesn't re-download the Reranker model on every restart.
        volumes = [
          "hindsight-cache:/home/hindsight/.cache/huggingface"
        ];
        environment = {
          HINDSIGHT_API_HOST = "127.0.0.1";
          HINDSIGHT_API_PORT = "18888";
          HINDSIGHT_CP_DATAPLANE_API_URL = "http://127.0.0.1:18888";
          HINDSIGHT_CP_PORT = "19999";
          HINDSIGHT_API_VECTOR_EXTENSION = "pgvector";
          # Configure OpenAI-compatible provider pointing to your local llama-swap models
          HINDSIGHT_API_LLM_API_KEY = "placeholder";
          HINDSIGHT_API_LLM_PROVIDER = "openai";
          HINDSIGHT_API_LLM_BASE_URL = "https://llama.fedeizzo.dev/v1";
          HINDSIGHT_API_LLM_MODEL = "${model}-nothink";
          HINDSIGHT_API_LLM_TIMEOUT = "600";
          HINDSIGHT_API_EMBEDDINGS_PROVIDER = "openai";
          HINDSIGHT_API_EMBEDDINGS_OPENAI_BASE_URL = "https://llama.fedeizzo.dev/v1";
          HINDSIGHT_API_EMBEDDINGS_OPENAI_MODEL = "bge-m3";
          HINDSIGHT_API_EMBEDDINGS_OPENAI_API_KEY = "placeholder";

          # Reranker Configuration (Runs on CPU inside the Hindsight container to save GPU VRAM)
          HINDSIGHT_API_RERANKER_PROVIDER = "local";
          HINDSIGHT_API_RERANKER_LOCAL_MODEL = "BAAI/bge-reranker-v2-m3";
        };
        environmentFiles = [
          config.sops.secrets.hermes-env.path
        ];
      };

      # Local Signal CLI Daemon for E2EE Communication
      systemd.services.signal-cli = {
        description = "Signal CLI Daemon";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          User = "hermes";
          Group = "hermes";
          ExecStart = "${signal-cli-patched}/bin/signal-cli --config /var/lib/signal-cli daemon --http 127.0.0.1:7583";
          Restart = "always";
          RestartSec = 5;
        };
      };

      # Ensure the state directory for the Signal CLI exists on the host
      systemd.tmpfiles.rules = [
        "d /var/lib/signal-cli 0770 hermes hermes - -"
      ];

      sops.secrets.hermes-env = {
        format = "dotenv";
        mode = "0400";
        # Automatically restart the service when the secret changes
        restartUnits = [ "hermes-agent.service" ];
        sopsFile = ./hermes-homelab-secrets.env;
        key = ""; # Maps the entire env file as a secret
      };

      fi.services = [
        {
          name = "hermes";
          port = 8642; # placeholder not used
          shouldMonitorUptime = false;
          shouldBehindReverseProxy = false;
          shouldBeInDashboard = false;
          toPersist = [
            {
              directory = "/var/lib/hermes";
              user = "hermes";
              group = "hermes";
              mode = "u=rwx,g=rwx,o=";
            }
          ];
          toBackup = [
            "/persist/var/lib/hermes"
          ];
        }
        {
          name = "signal";
          port = 7583;
          shouldMonitorUptime = false;
          shouldBeInDashboard = false;
          toPersist = [
            {
              directory = "/var/lib/signal-cli";
              user = "hermes";
              group = "hermes";
              mode = "u=rwx,g=rwx,o=";
            }
          ];
          toBackup = [
            "/persist/var/lib/signal-cli"
          ];
        }
        {
          name = "hindsight";
          port = 19999;
          dashboardSection = "Tools";
          toPersist = [ ];
          toBackup = [ ];
        }
        {
          name = "hindsight-api";
          port = 18888;
          shouldMonitorUptime = false;
          shouldBeInDashboard = false;
          dashboardSection = "Tools";
          toPersist = [ ];
          toBackup = [ ];
        }
      ];

      users.users.hermes = {
        uid = 290;
        group = "hermes";
      };
      users.groups.hermes.gid = 290;
    };
}
