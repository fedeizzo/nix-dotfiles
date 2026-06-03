{ pkgs-unstable, lib, ... }:

let
  # llama-cpp = pkgs-unstable.llama-cpp-rocm;
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
          aliases = [ "coding" "q3-m" ];
        };

        "qwen3-embedding" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf Qwen/Qwen3-Embedding-8B-GGUF --embedding --pooling last -ub 8192'';
          # aliases = [ "embedding" ];
        };

        "qwen36-27b-realtime" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-27B-GGUF:UD-Q4_K_XL -hfd unsloth/Qwen3.5-0.8B-GGUF:UD-Q4_K_XL --temp 1.0 --top-p 0.95 --min-p 0.0 --top-k 20 ${commonFlags} --presence-penalty 1.5 --frequency-penalty 1.0'';
          aliases = [ "realtime" "q4-xl" ];
          timeouts = {
            responseHeader = 600;
          };
        };
      };

      matrix = {
        vars = {
          "qr" = "qwen36-27b-realtime";
          "qc" = "qwen36-35b-a3b";
        };

        sets = {
          standard = "qr & qc";
        };
      };

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
