{
  flake-file = {
    inputs.gufo = {
      url = "github:gufo-org/gufo";
    };
    inputs.lfm3-gufo-extension = {
      url = "github:gufo-org/lfm3-gufo-extension";
      flake = false;
    };
  };

  # llama-swap service; runs on every Strix Halo host.
  # Per-model definitions live in ./models/.
  flake.modules.nixos.llama-swap = { lib, inputs, pkgs, config, ... }:
    {
      imports = [
        (inputs.nixpkgs-unstable + "/nixos/modules/services/networking/llama-swap.nix")
        inputs.self.modules.nixos.llama-swap-gufo-lfm3
        inputs.self.modules.nixos.llama-swap-gufo-model-sync
        inputs.self.modules.nixos.llama-swap-lfm3
        inputs.self.modules.nixos.llama-swap-lfm3-vl
        inputs.self.modules.nixos.llama-swap-lfm3-audio-asr
        inputs.self.modules.nixos.llama-swap-lfm3-audio-tts
        inputs.self.modules.nixos.llama-swap-d1
        inputs.self.modules.nixos.llama-swap-qwen27b
        inputs.self.modules.nixos.llama-swap-embeddinggemma-2
        inputs.self.modules.nixos.llama-swap-flash-next
      ];
      nix.settings = {
        extra-substituters = [ "https://gufo.cachix.org" ];
        extra-trusted-public-keys = [ "gufo.cachix.org-1:rfZe3n4lHINgyoACKzs5EthIZmBNvEyCCtufUpth0PE=" ];
      };
      nixpkgs.overlays = [
        (_: _: {
          # v270+ proxies the OpenAI Realtime WebSocket (/v1/realtime), needed by
          # Open WebUI voice calls; nixpkgs-unstable still ships 268. src/vendor
          # hashes from NixOS/nixpkgs#584989 (268 -> 275), the npm deps hash
          # differs with our fetchNpmDeps: drop this once unstable has it.
          llama-swap = inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.llama-swap.overrideAttrs (finalAttrs: prev: {
            version = "275";
            src = prev.src.override {
              tag = "v${finalAttrs.version}";
              hash = "sha256-DsoBENlTUdXwDweQJzgtKNS6yawSq173xwfgAsBmsJ0=";
            };
            vendorHash = "sha256-ESaG4DYGGlK+tPM9qHMcDSI0TLkg+DdMC6E8ik0jxIw=";
            excludedPackages = prev.excludedPackages ++ [ "docbot/gensource" "docs/gen" ];
            passthru = prev.passthru // {
              ui = prev.passthru.ui.overrideAttrs (uiPrev: {
                npmDepsHash = "sha256-GQiBiP5UvrCJeiiHf6JA+PcKhg/SSCQIyECvIsr2/KM=";
                npmDeps = uiPrev.npmDeps.overrideAttrs {
                  outputHash = "sha256-GQiBiP5UvrCJeiiHf6JA+PcKhg/SSCQIyECvIsr2/KM=";
                };
              });
            };
          });
        })
      ];
      disabledModules = [
        "services/networking/llama-swap.nix"
      ];
      services.llama-swap = {
        enable = true;
        port = 11435;
        listenAddress = "0.0.0.0";
        settings = {
          healthCheckTimeout = 600;
          includeAliasesInList = true;
          # Every local model may run at the same time: nothing is ever
          # swapped out to make room for another model. Memory is not
          # checked, loading too much at once can run the host out of memory.
          groups.all = {
            swap = false;
            exclusive = false;
            members = builtins.attrNames config.services.llama-swap.settings.models;
          };
          # Small models that fit concurrently (~25 GiB of the 125 GiB unified
          # memory on Strix Halo); each unloads after 120s idle (ttl).
          hooks.on_startup.preload = [
            "lfm3-1.7b"
            "lfm3-vl"
            "lfm3-audio-asr"
            "lfm3-audio-tts"
          ];
        };
      };

      systemd.services.llama-swap = {
        environment = {
          LLAMA_CACHE = lib.mkForce "/var/cache/llama-swap";
          XDG_CACHE_HOME = lib.mkForce "/var/cache/llama-swap/.cache"; # Fix Vulkan shader cache
          HOME = "/var/cache/llama-swap"; # Fallback for any engine looking for ~
          GUFO_MODELS_DIR = config.services.gufo-model-sync.directory; # gufo model store, filled by gufo-model-sync
          GPU_MAX_HW_QUEUES = "1";
          # fastflow npu
          FLM_MODEL_PATH = "/var/cache/llama-swap/flm";
          XILINX_XRT = config.environment.sessionVariables.XILINX_XRT or "";
          XRT_PATH = config.environment.sessionVariables.XRT_PATH or "";
          FLM_DISABLE_UPDATE_CHECK = "1";
          LD_LIBRARY_PATH = "${config.environment.sessionVariables.XILINX_XRT or ""}/lib";
        };
        serviceConfig = {
          LimitMEMLOCK = "infinity"; # fastflowlm with npu support
          SupplementaryGroups = [ "video" "render" ];
          CacheDirectory = "llama-swap";
        };
      };

      environment.systemPackages = [ ];

      fi.services = [
        {
          name = "llama";
          dashboardIcon = "codellm";
          port = config.services.llama-swap.port;
          dashboardSection = "Tools";
          toPersist = [ ];
          toBackup = [ ];
        }
      ];
    };

  # The rest of the local models, OpenRouter and model sets: homelab only.
  flake.modules.nixos.llama-swap-models = { lib, inputs, config, ... }:
    {
      imports = [
        inputs.self.modules.nixos.llama-swap-ds4
        inputs.self.modules.nixos.llama-swap-qwen-tts
        inputs.self.modules.nixos.llama-swap-qwen-asr
        inputs.self.modules.nixos.llama-swap-qwen-image
      ];

      # `llama-swap -validate` runs at build time and fails on the
      # ${env.OPENROUTER_API_KEY} macro below: the key only exists at runtime (sops)
      services.llama-swap.validateConfigFile = false;

      services.llama-swap.settings = {
        hooks.on_startup.preload = [ "embeddinggemma-2" ];

        peers = {
          openrouter = {
            proxy = "https://openrouter.ai/api";
            apiKey = ''''${env.OPENROUTER_API_KEY}'';
            models = [
              "deepseek/deepseek-v4-flash-vision-exp"
              "z-ai/glm-5.2"
              "stealth/ox-alpha"
            ];
            filters = {
              setParams = {
                provider = {
                  order = [ "deepseek" "baidu" ];
                  allow_fallbacks = false;
                };
              };
            };
          };
        };
      };

      systemd.services.llama-swap.serviceConfig.EnvironmentFile = config.sops.secrets.openrouter-api-key.path;

      sops.secrets.openrouter-api-key = lib.mkIf config.services.llama-swap.enable {
        format = "yaml";
        mode = "0400";
        restartUnits = [ "llama-swap.service" ];
        sopsFile = ./llama-swap-homelab-secrets.yaml;
      };
    };
}
