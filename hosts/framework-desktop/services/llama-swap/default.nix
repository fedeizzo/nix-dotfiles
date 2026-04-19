{ pkgs, pkgs-unstable, lib, ... }:

let
  llama-cpp = pkgs-unstable.llama-cpp-rocm;
  llama-server = lib.getExe' llama-cpp "llama-server";
  # +------------------------+---------------------------+------------------------------------------------------------------+
  # | Parameter              | Short Description         | Value Explanation                                                |
  # +------------------------+---------------------------+------------------------------------------------------------------+
  # | --port                 | Port binding              | ''${PORT}: Dynamically sets the listening port via env var.      |
  # | --model                | Main model path           | /persist/.../Qwen3...: Exact file path to Qwen 35B model weights.|
  # | --mmproj               | Vision projector          | /persist/.../mmproj-F16.gguf: Projector for image processing.    |
  # | --seed                 | Randomness seed           | 3407: Fixed seed for deterministic and reproducible generations. |
  # | --temp                 | Temperature               | 1.0: Baseline randomness setting for generation.                 |
  # | --top-p                | Nucleus sampling          | 0.95: Discards the bottom 5% of unlikely tokens.                 |
  # | --min-p                | Minimum probability       | 0.01: Drops tokens with <1% probability of the top token.        |
  # | --top-k                | Top-K sampling            | 40: Restricts the model to consider only the top 40 next tokens. |
  # | -ngl                   | GPU Layer Offload         | 999: Offloads all layers to the Strix Halo GPU cores.            |
  # | --no-mmap              | Disable Memory Map        | Forces loading directly into RAM for high-RAM APU stability.     |
  # | -fa                    | Flash Attention           | 1: Enables Flash Attention, drastically reducing KV cache size.  |
  # | --no-webui             | Disable Web UI            | Turns off the HTML interface; runs strictly as an API backend.   |
  # | --chat-template-kwargs | Template overrides        | '{"enable_thinking":false}': Disables internal reasoning tags.   |
  # | --kv-unified           | Unified Memory KV         | Optimizes memory allocation for shared CPU/GPU RAM (UMA).        |
  # | -ub                    | Micro-batch size          | 512: Optimal physical hardware chunk compute size for Strix Halo.|
  # | -b                     | Logical batch size        | 4096: Max tokens ingested logically before chopping into chunks. |
  # | -c                     | Context window limit      | 262144: Locks in the absolute maximum native context (256K).     |
  # | --cache-type-k         | K-Cache Quantization      | q8_0: Compresses 'Key' cache to 8-bit to save memory bandwidth.  |
  # | --cache-type-v         | V-Cache Quantization      | q8_0: Compresses 'Value' cache to 8-bit to save memory bandwidth.|
  # | --defrag-thold         | Defrag trigger            | 0.1: Auto-defragments KV cache when fragmentation hits 10%.      |
  # | -t                     | CPU Thread count          | 8: Restricts CPU usage to exact physical cores to save bandwidth.|
  # +------------------------+---------------------------+------------------------------------------------------------------+
  cmdQwen35 = (model: mmproj: ''${llama-server}  --port ''${PORT} --model ${model} --mmproj ${mmproj} --temp 0.45 --top-p 0.95 --min-p 0.0 --top-k 20 -ngl 999 --no-mmap -fa 1 --no-webui --kv-unified -ub 512 -b 4096 -c 262144 --cache-type-k q8_0 --cache-type-v q8_0 --defrag-thold 0.1 -t 8'');
  cmdQwen36 = (model: mmproj: ''${llama-server}  --port ''${PORT} --model ${model} --mmproj ${mmproj} --temp 0.6 --top-p 0.95 --min-p 0.0 --top-k 20 -ngl 999 --no-mmap -fa 1 --no-webui --kv-unified -ub 2048 -b 4096 -c 262144 --cache-type-k q8_0 --cache-type-v q8_0 --defrag-thold 0.1 -t 2 --presence-penalty 0.0 --frequency-penalty 1.0'');
in
{
  services.llama-swap = {
    enable = true;
    port = 11435;
    listenAddress = "0.0.0.0";
    settings = {
      healthCheckTimeout = 60;
      models = {
        "qwen36" = {
          cmd = (cmdQwen36 "/persist/models/qwen36-35b-a3b/Qwen3.6-35B-A3B-UD-Q8_K_XL.gguf" "/persist/models/qwen36-35b-a3b/mmproj-F32.gguf");
          # ttl = 300; # 5 minutes
        };
        "qwen35" = {
          cmd = (cmdQwen35 "/persist/models/qwen35-35b-a3b/Qwen3.5-35B-A3B-UD-Q4_K_L.gguf" "/persist/models/qwen35-35b-a3b/mmproj-F16.gguf");
          aliases = [ "the-best" ];
          # ttl = 300; # 5 minutes
        };
        "qwen35-small" = {
          cmd = (cmdQwen35 "/persist/models/qwen35-9b/Qwen3.5-9B-Q4_K_M.gguf" "/persist/models/qwen35-9b/mmproj-F32.gguf");
          # ttl = 300; # 5 minutes
        };
        "qwen35-task" = {
          cmd = (cmdQwen35 "/persist/models/qwen35-4b/Qwen3.5-4B-Q4_K_M.gguf" "/persist/models/qwen35-4b/mmproj-F32.gguf");
          # ttl = 300; # 5 minutes
        };
      };
      groups = {
        "model_and_task" = {
          swap = false;
          exclusive = false;
          members = [
            "qwen35"
            "qwen35-task"
          ];
        };
      };
    };
  };
}
