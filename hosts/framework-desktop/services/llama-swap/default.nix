{ pkgs-unstable, lib, inputs, pkgs, ... }:

let
  rocmfp4-llama = pkgs.callPackage ./rocmfp4-llama.nix { };
  llama-cpp =
    (pkgs-unstable.llama-cpp.override {
      rocmSupport = true;
      rocmGpuTargets = [ "gfx1151" ];
    }).overrideAttrs
      (oldAttrs: rec {
        version = "9186";
        src = pkgs-unstable.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          tag = "b${version}";
          hash = "sha256-mkdZl/yReMMbls6neFmyD5gOZYR2wsafipxlRXcDPYM=";

          leaveDotGit = true;

          postFetch = ''
            git -C "$out" rev-parse --short HEAD > $out/COMMIT
            find "$out" -name .git -print0 | xargs -0 rm -rf
          '';
        };
        npmRoot = "tools/ui";
        npmDepsHash = "sha256-WaEePrEZ7O/7deP2KJhe0AwiSKYA8HOqETmMHUkmBe0=";
        cmakeFlags = (oldAttrs.cmakeFlags or [ ]) ++ [
          "-DLLAMA_HIP_UMA=ON" # unified memory
        ];
        # Mirror the Strix Halo toolbox HIP tuning: pin the ROCm path explicitly and
        # raise the local unroll threshold for gfx1151 kernels.
        cmakeFlagsArray = (oldAttrs.cmakeFlagsArray or [ ]) ++ [
          "-DCMAKE_HIP_FLAGS=--rocm-path=${pkgs-unstable.rocmPackages.clr} -mllvm --amdgpu-unroll-threshold-local=600"
        ];
      });
  llama-server = lib.getExe' llama-cpp "llama-server";
  rocmfp4-llama-server = lib.getExe' rocmfp4-llama "llama-server";
  ds4-server = lib.getExe' inputs.ds4.packages.${pkgs.system}.default "ds4-server";

  # Common flags shared across all model configurations.
  commonFlags = ''
    -ngl 999 \
    --no-mmap -fa 1 \
    --no-webui \
    --kv-unified \
    -c 262144 \
    -t 2
  '';
  # -ub 2048 -b 4096 -c 262144 \

in
{
  services.llama-swap = {
    enable = true;
    port = 11435;
    listenAddress = "0.0.0.0";
    settings = {
      healthCheckTimeout = 60;

      models = {
        "qwen36-35b-a3b" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-35B-A3B-MTP-GGUF:UD-Q4_K_XL ${commonFlags} --spec-type draft-mtp --spec-draft-n-max 3 --spec-draft-p-min 0.75 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.00 --presence-penalty 0.0 --repeat-penalty 1.0'';

          aliases = [ "coding" "q3-m" "qwen" ];

          filters = {
            setParamsByID = {
              "qwen-nothink" = {
                chat_template_kwargs = {
                  enable_thinking = false;
                };
              };
            };
          };
        };

        "qwen27" = {
          env = [ "HSA_OVERRIDE_GFX_VERSION=11.5.1" "GGML_HIP_ENABLE_UNIFIED_MEMORY=1" ];
          cmd = ''
            ${rocmfp4-llama-server} \
              -m /persist/models/Qwopus3.6/Qwopus3.6-27B-v2-MTP-BF16-to-ROCmFP4-STRIX_LEAN.gguf \
              --mmproj /persist/models/Qwopus3.6/mmproj-F32.mmproj \
              --port ''${PORT} \
              --jinja \
              -c 262144 \
              -ngl 999 \
              -fa on \
              -dev ROCm0 \
              -b 512 \
              -ub 512 \
              -t 16 \
              -tb 32 \
              -ctk q4_0 \
              -ctv q4_0 \
              --spec-type draft-mtp \
              --spec-draft-device ROCm0 \
              --spec-draft-ngl all \
              --spec-draft-type-k q4_0 \
              --spec-draft-type-v q4_0 \
              --spec-draft-n-max 4 \
              --spec-draft-n-min 0 \
              --spec-draft-p-min 0.0 \
              --spec-draft-p-split 0.10 \
              --parallel 1 \
              --metrics \
              --no-mmap
          '';

          filters = {
            setParamsByID = {
              "qwen27-nothink" = {
                chat_template_kwargs = {
                  enable_thinking = false;
                };
              };
            };
          };
        };

        "qwen3-embedding" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf Qwen/Qwen3-Embedding-8B-GGUF --embedding --pooling last -ub 8192'';
        };

        "bge-m3" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf ggml-org/bge-m3-Q8_0-GGUF --embedding -ub 8192'';
          aliases = [ "embedding" ];
        };

        "qwen36-27b-realtime" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-27B-GGUF:UD-Q4_K_XL -hfd unsloth/Qwen3.5-0.8B-GGUF:UD-Q4_K_XL --temp 1.0 --top-p 0.95 --min-p 0.0 --top-k 20 ${commonFlags} --presence-penalty 1.5 --frequency-penalty 1.0'';
          aliases = [ "realtime" "q4-xl" ];
          timeouts = {
            responseHeader = 600;
          };
        };

        "ds4" = {
          cmd = ''${ds4-server} --port ''${PORT} -m /persist/models/DeepSeek-V4-Flash/DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix.gguf --ctx 262144 --kv-disk-dir /tmp/ds4-kv --kv-disk-space-mb 8192'';
          checkEndpoint = "/v1/models";
          aliases = [ "ds4" ];
          timeouts = {
            responseHeader = 600;
          };
          filters = {
            setParamsByID = {
              "ds4-nothink" = {
                chat_template_kwargs = {
                  enable_thinking = false;
                };
              };
            };
          };
        };
      };

      matrix = {
        vars = {
          "qr" = "qwen36-27b-realtime";
          "qc" = "qwen36-35b-a3b";
          "e" = "bge-m3";
          "ds4" = "ds4";
          "qrr" = "qwen27";
        };

        sets = {
          standard = "qr & qc & e";
          ds4 = "ds4 & qc & e";
          qrr = "qrr & qc & e";
        };
      };

      includeAliasesInList = true;

      # hooks = {
      #   on_startup = {
      #     preload = [ "qwen36-27b-realtime" "qwen36-35b-a3b" ];
      #   };
      # };
    };
  };

  systemd.services.llama-swap = {
    environment.LLAMA_CACHE = "/persist/models";
    serviceConfig.ReadWritePaths = "/persist/models";
  };
}
