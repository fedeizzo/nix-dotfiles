{
  flake-file.inputs.gufo = {
    type = "git";
    url = "ssh://git@github.com/gufo-org/gufo.git";
    flake = true; # Set to false if gufo doesn't have its own flake.nix
  };

  flake.modules.nixos.llama-swap = { pkgs-unstable, lib, inputs, pkgs, config, ... }:
    let
      llama-cpp =
        (pkgs.llama-cpp.override {
          rocmSupport = true;
          rocmGpuTargets = [ "gfx1151" ];
        }).overrideAttrs
          (oldAttrs: rec {
            cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [
              "-DLLAMA_HIP_UMA=ON" # unified memory
            ];
            # Mirror the Strix Halo toolbox HIP tuning: pin the ROCm path explicitly and
            # raise the local unroll threshold for gfx1151 kernels.
            cmakeFlagsArray = (oldAttrs.cmakeFlagsArray or [ ]) ++ [
              "-DCMAKE_HIP_FLAGS=--rocm-path=${pkgs.rocmPackages.clr} -mllvm --amdgpu-unroll-threshold-local=600"
            ];
          });
      llama-server = lib.getExe' llama-cpp "llama-server";
      gufoQwen27 = inputs.gufo.lib.${pkgs.system}.mkGufoServe {
        modality = "llm";

        model = "/persist/models/models--unsloth--Qwen3.8-27B-GGUF/snapshots/4ca720788d1e01f1bff70c033e0d0028fd02e502/Qwen3.8-27B-UD-Q8_K_L.gguf";
        draftModel = "/persist/models/Qwen3.8-27B-DFlash2-Q8_0.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # sessions = 5;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        servedModelName = "qwen3.8-27b";
        context = 250000;

        # Speculative Decoding (DFlash-2)
        speculative = "dflash2";
        draftTokens = 7;
        # draftPolicy = "auto";
        minDraftTokens = 1;
        specDraftPMin = 0.0;

        # Scheduling
        prefillChunk = 512;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        # Sampling — Qwen3.8 official thinking-mode preset
        temp = 0.8;
        maxTokens = 8192;
        topP = 0.95;
        topK = 20;
        minP = 0.0;
        minKeep = 0;
        seed = -1;
        repeatPenalty = 1.0;
        repeatLastN = 64;
        frequencyPenalty = 0.0;
        presencePenalty = 0.0;

        # Reasoning
        think = "on";
        reasoningEffort = "medium";
        preserveThinking = "auto";

        # Cache
        # cacheDisk = "/var/cache/gufo";
        # cacheDiskBytes = 4294967296;
        # cacheDiskStagingBytes = 536870912;

        # Hardware
        cpu = false;
      };

      gufoDs4 = inputs.gufo.lib.${pkgs.system}.mkGufoServe {
        modality = "llm";

        model = "/persist/models/DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix-0731.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # sessions = 2;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        servedModelName = "ds4";
        context = 131072;

        # Speculative decoding: see note below — DSpark is not available in serve
        speculative = "off";

        # Scheduling
        prefillChunk = 512;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        # Sampling
        temp = 0.6;
        maxTokens = 8192;
        topP = 0.95;
        topK = 0;
        minP = 0.0;
        minKeep = 0;
        seed = -1;
        repeatPenalty = 1.0;
        repeatLastN = 64;
        frequencyPenalty = 0.0;
        presencePenalty = 0.0;

        # Cache
        # cacheDisk = "/var/cache/gufo-ds4";
        # cacheDiskBytes = 4294967296;
        # cacheDiskStagingBytes = 536870912;

        # Hardware
        cpu = false;
      };

      gufoQwenTTSCmd = inputs.gufo.lib.${pkgs.system}.mkGufoServe {
        modality = "audio";

        ttsModel = "/persist/models/audio/Qwen3-TTS-12Hz-1.7B-Base";
        ttsContext = 4096;

        asrModel = "/persist/models/audio/Qwen3-ASR-1.7B";
        asrContext = 1024;

        voices = {
          narrator_eng = {
            wav = "/persist/models/audio/clear-english-voice.wav";
            text = "It is said with truth that every building is constructed stone by stone, and the same may be said of knowledge. Extract.";
            language = "english";
          };

          narrator_ita = {
            wav = "/persist/models/audio/clear-italian-voice.wav";
            text = "Questo racconto è cresciuto nel corso della narrazione fino a diventare una storia della Grande Guerra dell'Anello, e ha in.";
            language = "italian";
          };

          me = {
            wav = "/persist/models/audio/me.wav";
            text = "ciao il mio nome è Federico sono un ingegnere informatico vivo a Parigi e nel tempo libero mi piace arrampicare";
            language = "italian";
          };
        };

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
      };

      commonFlags = ''
        -ngl 999 \
        --no-mmap -fa 1 \
        --no-ui \
        --kv-unified \
        -c 262144 \
        -t 2
      '';
    in
    {
      imports = [
        (inputs.nixpkgs-unstable + "/nixos/modules/services/networking/llama-swap.nix")
      ];
      nixpkgs.overlays = [
        (_: _: {
          inherit (inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}) llama-swap llama-rocm;
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

          models = {
            "qwen36-35b-a3b" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-35B-A3B-MTP-GGUF:UD-Q4_K_XL ${commonFlags} --spec-type draft-mtp --spec-draft-n-max 3 --spec-draft-p-min 0.75 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.00 --presence-penalty 0.0 --repeat-penalty 1.0 --ubatch-size 2048 --batch-size 4096 --chat-template-kwargs '{"preserve_thinking": true}' '';
              aliases = [ "coding" "q3-m" "qwen" ];
              filters.setParamsByID."qwen-nothink".chat_template_kwargs.enable_thinking = false;
            };

            "qwen3-embedding" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${llama-server} --port ''${PORT} -hf Qwen/Qwen3-Embedding-8B-GGUF --embedding --pooling last -ub 8192'';
            };

            "bge-m3" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${llama-server} --port ''${PORT} -hf ggml-org/bge-m3-Q8_0-GGUF --embedding -ub 8192'';
              aliases = [ "embedding" ];
            };

            "qwen3.8-27b" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = "${gufoQwen27}";
              timeouts.responseHeader = 600;
              aliases = [ "Qwen3.8-27B" ];
            };

            "ds4" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = "${gufoDs4}";
              timeouts.responseHeader = 600;
              aliases = [ ];
            };

            "qwen3-tts" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = "${gufoQwenTTSCmd}";
              timeouts.responseHeader = 600;
              aliases = [ "qwen3-asr" ];
            };
          };

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
              "q35" = "qwen36-35b-a3b";
              "e" = "bge-m3";
              "q27" = "qwen3.8-27b";
              "audio" = "qwen3-tts";
            };

            sets = {
              standard = "q27 & q35 & e & audio";
            };
          };

          includeAliasesInList = true;
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
          EnvironmentFile = config.sops.secrets.openrouter-api-key.path;
          ReadWritePaths = "/persist/models";
          LimitMEMLOCK = "infinity"; # fastflowlm with npu support
          SupplementaryGroups = [ "video" "render" ];
        };
      };

      environment.systemPackages = [ ];

      sops.secrets.openrouter-api-key = lib.mkIf config.services.llama-swap.enable {
        format = "yaml";
        mode = "0400";
        restartUnits = [ "llama-swap.service" ];
        sopsFile = ./llama-swap-homelab-secrets.yaml;
      };

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
}
