{
  flake-file.inputs.ds4.url = "github:fedeizzo/ds4";

  flake.modules.nixos.llama-swap = { pkgs-unstable, lib, inputs, pkgs, config, ... }:
    let
       llama-cpp =
        (pkgs-unstable.llama-cpp.override {
          rocmSupport = true;
          rocmGpuTargets = [ "gfx1151" ];
        }).overrideAttrs
          (oldAttrs: rec {
            version = "9925";
            src = pkgs-unstable.fetchFromGitHub {
              owner = "ggml-org";
              repo = "llama.cpp";
              tag = "b${version}";
              hash = "sha256-yX8BrHA0fIgIozBGOXnN72KlfqIcR/mnO5ttUBLvxZE=";

              leaveDotGit = true;

              postFetch = ''
                git -C "$out" rev-parse --short HEAD > $out/COMMIT
                find "$out" -name .git -print0 | xargs -0 rm -rf
              '';
            };
            npmRoot = "tools/ui";
            npmDepsHash = "sha256-6s9skw1wzEfm9QKktTqea3J+oudQAsS6O2VnZEMXAdw=";
            cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [
              "-DLLAMA_HIP_UMA=ON" # unified memory
            ];
            cmakeFlagsArray = (oldAttrs.cmakeFlagsArray or [ ]) ++ [
              "-DCMAKE_HIP_FLAGS=--rocm-path=${pkgs-unstable.rocmPackages.clr} -mllvm --amdgpu-unroll-threshold-local=600"
            ];
          });
      llama-server = lib.getExe' llama-cpp "llama-server";
      ds4-server = lib.getExe' inputs.ds4.packages.${pkgs.system}.default "ds4-server";
      crispasr = pkgs.callPackage ./crispasr.package { useROCm = true; rocmPackages = pkgs-unstable.rocmPackages; };

      commonFlags = ''
        -ngl 999 \
        --no-mmap -fa 1 \
        --no-webui \
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
          healthCheckTimeout = 60;

          models = {
            "qwen36-35b-a3b" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-35B-A3B-MTP-GGUF:UD-Q4_K_XL ${commonFlags} --spec-type draft-mtp --spec-draft-n-max 3 --spec-draft-p-min 0.75 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.00 --presence-penalty 0.0 --repeat-penalty 1.0'';
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

            "whisper-v3-turbo" = {
              env = [
                "FLM_MODEL_PATH=/persist/models/flm"
              ];
              cmd = ''${pkgs.fastflowlm}/bin/flm serve --port ''${PORT} -a 1'';
              aliases = [ "whisper" "transcription" ];
              checkEndpoint = "/v1/models";
            };

            "qwen36-27b" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-27B-MTP-GGUF:UD-Q4_K_XL ${commonFlags} --spec-type draft-mtp --spec-draft-n-max 3 --spec-draft-p-min 0.75 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.00 --presence-penalty 0.0 --repeat-penalty 1.0'';
              aliases = [ "realtime" "q4-xl" "qwen27" ];
              timeouts.responseHeader = 600;
            };

            "ds4" = {
              env = [ "GPU_MAX_HW_QUEUES=1" ];
              cmd = ''${ds4-server} --port ''${PORT} -m /persist/models/DeepSeek-V4-Flash/DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix.gguf --ctx 262144 --kv-disk-dir /tmp/ds4-kv --kv-disk-space-mb 8192'';
              checkEndpoint = "/v1/models";
              aliases = [ "ds4" ];
              timeouts.responseHeader = 600;
              filters.setParamsByID."ds4-nothink".chat_template_kwargs.enable_thinking = false;
            };

            "voxtral" = {
              env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" "HSA_ENABLE_SDMA=0" ];
              cmd = ''${crispasr}/bin/crispasr --server --port ''${PORT} --backend voxtral-tts -m /persist/models/voxtral-4b-tts-f16.gguf --cache-dir /persist/models --no-flash-attn'';
              aliases = [ "tts" "voxtral" ];
            };
          };

          matrix = {
            vars = {
              "q35" = "qwen36-35b-a3b";
              "e" = "bge-m3";
              "ds4" = "ds4";
              "q27" = "qwen36-27b";
              "ws" = "whisper-v3-turbo";
              "vx" = "voxtral";
            };

            sets = {
              standard = "q27 & q35 & e & ws & vx";
              ds4 = "ds4 & q35 & e & ws & vx";
            };
          };

          includeAliasesInList = true;
        };
      };

      systemd.services.llama-swap = {
        environment = {
          LLAMA_CACHE = "/persist/models";
          GPU_MAX_HW_QUEUES = "1";
          # fastflow npu
          FLM_MODEL_PATH = "/persist/models/flm";
          XILINX_XRT = config.environment.sessionVariables.XILINX_XRT or "";
          XRT_PATH = config.environment.sessionVariables.XRT_PATH or "";
          FLM_DISABLE_UPDATE_CHECK = "1";
          LD_LIBRARY_PATH = "${config.environment.sessionVariables.XILINX_XRT or ""}/lib";
        };
        serviceConfig.ReadWritePaths = "/persist/models";
        serviceConfig.LimitMEMLOCK = "infinity"; # fastflowlm with npu support
        serviceConfig.SupplementaryGroups = [ "video" "render" ];
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
