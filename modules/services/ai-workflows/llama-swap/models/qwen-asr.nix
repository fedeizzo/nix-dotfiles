# Qwen3-ASR served by gufo (asr modality).
{
  flake.modules.nixos.llama-swap-qwen-asr = { inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      gufoQwenASRCmd = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "asr";

        model = "${modelsDir}/qwen3-asr";
        servedModelName = "qwen3-asr";
        context = 1024;

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
      };
    in
    {
      services.gufo-model-sync.models = [ "qwen3-asr" ];
      services.llama-swap.settings.models."qwen3-asr" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenASRCmd}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 120; # Unload after 2 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
