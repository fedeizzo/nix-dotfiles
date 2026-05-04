{ pkgs-unstable, lib, ... }:

let
  llama-cpp = pkgs-unstable.llama-cpp-rocm;
  llama-server = lib.getExe' llama-cpp "llama-server";

  # Common flags shared across all model configurations.
  commonFlags = ''
    -ngl 999 \
    --no-mmap -fa 1 \
    --no-webui \
    --kv-unified \
    -ub 2048 -b 4096 -c 262144 \
    --cache-type-k q8_0 \
    --cache-type-v q8_0 \
    -t 2
  '';

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
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q3_K_M --temp 0.6 --top-p 0.95 --min-p 0.0 --top-k 20 ${commonFlags} --presence-penalty 0.0 --frequency-penalty 1.0'';
          aliases = [ "coding" "q3-m" ];
        };

        "gemma4-26b-a4b" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/gemma-4-26B-A4B-it-GGUF:UD-Q4_K_XL --temp 0.6 --top-p 0.95 --top-k 64 ${commonFlags} --presence-penalty 0.0'';
          aliases = [ "task" "q4-xl" ];
        };

        "gemma4-e4b" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/gemma-4-E4B-it-GGUF:Q4_K_M --temp 0.6 --top-p 0.95 --top-k 64 ${commonFlags} --presence-penalty 0.0'';
          aliases = [ "task-fast" ];
        };

        "qwen3-embedding" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf Qwen/Qwen3-Embedding-8B-GGUF --embedding --pooling last -ub 8192'';
          # aliases = [ "embedding" ];
        };

        "qwen36-27b-async" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-27B-GGUF:UD-Q8_K_XL --temp 1.0 --top-p 0.95 --min-p 0.0 --top-k 20 ${commonFlags} --presence-penalty 1.5 --frequency-penalty 1.0'';
          aliases = [ "async" "q8-xl" ];
          timeouts = {
            responseHeader = 600;
          };
        };

        "qwen36-27b-realtime" = {
          env = [ "LLAMA_CACHE=/persist/models" ];
          cmd = ''${llama-server} --port ''${PORT} -hf unsloth/Qwen3.6-27B-GGUF:UD-Q3_K_XL --temp 1.0 --top-p 0.95 --min-p 0.0 --top-k 20 ${commonFlags} --presence-penalty 1.5 --frequency-penalty 1.0'';
          aliases = [ "realtime" "q3-xl" ];
          timeouts = {
            responseHeader = 600;
          };
        };
      };

      matrix = {
        vars = {
          "qa" = "qwen36-27b-async";
          "qr" = "qwen36-27b-realtime";
          "qc" = "qwen36-35b-a3b";
        };

        sets = {
          standard = "(qa | qr) & qc";
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
