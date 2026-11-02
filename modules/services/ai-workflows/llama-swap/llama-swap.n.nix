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
        inputs.self.modules.nixos.llama-swap-lfm3
        inputs.self.modules.nixos.llama-swap-lfm3-vl
        inputs.self.modules.nixos.llama-swap-lfm3-audio-asr
        inputs.self.modules.nixos.llama-swap-lfm3-audio-tts
        inputs.self.modules.nixos.llama-swap-d1
        inputs.self.modules.nixos.llama-swap-qwen27b
        inputs.self.modules.nixos.llama-swap-flash-next
      ];
      nix.settings = {
        extra-substituters = [ "https://gufo.cachix.org" ];
        extra-trusted-public-keys = [ "gufo.cachix.org-1:rfZe3n4lHINgyoACKzs5EthIZmBNvEyCCtufUpth0PE=" ];
      };
      nixpkgs.overlays = [
        (_: _: {
          inherit (inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}) llama-swap;
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
          # Small models that fit concurrently (~25 GiB of the 125 GiB unified
          # memory on Strix Halo); each unloads after 120s idle (ttl).
          hooks.on_startup.preload = [
            "lfm3-1.7b"
            "lfm3-vl"
            "lfm3-audio-asr"
            "lfm3-audio-tts"
          ];
          groups.resident = {
            swap = false; # all members run concurrently, no internal swapping
            exclusive = false; # running one of these never unloads other groups
            persistent = true; # other groups cannot unload these
            members = [
              "lfm3-1.7b"
              "lfm3-vl"
              "lfm3-audio-asr"
              "lfm3-audio-tts"
            ];
          };
        };
      };

      systemd.services.llama-swap = {
        environment = {
          LLAMA_CACHE = lib.mkForce "/persist/models";
          XDG_CACHE_HOME = lib.mkForce "/persist/models/.cache"; # Fix Vulkan shader cache
          HOME = "/persist/models"; # Fallback for any engine looking for ~
          GPU_MAX_HW_QUEUES = "1";
          # fastflow npu
          FLM_MODEL_PATH = "/persist/models/flm";
          XILINX_XRT = config.environment.sessionVariables.XILINX_XRT or "";
          XRT_PATH = config.environment.sessionVariables.XRT_PATH or "";
          FLM_DISABLE_UPDATE_CHECK = "1";
          LD_LIBRARY_PATH = "${config.environment.sessionVariables.XILINX_XRT or ""}/lib";
        };
        serviceConfig = {
          ReadWritePaths = "/persist/models";
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
        inputs.self.modules.nixos.llama-swap-embeddinggemma-2
      ];

      services.llama-swap.settings = {
        hooks.on_startup.preload = [ "embeddinggemma-2" ];
        groups.resident.members = [ "embeddinggemma-2" ];

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

        matrix = {
          vars = {
            "q27" = "qwen3.8-27b";
            "tts" = "qwen3-tts";
            "asr" = "qwen3-asr";
            "god" = "qwen-flash";
          };

          sets = {
            standard = "q27 & tts & asr & god";
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
