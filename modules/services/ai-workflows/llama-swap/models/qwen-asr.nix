# Qwen3-ASR served by gufo (asr modality).
{
  flake.modules.nixos.llama-swap-qwen-asr = { inputs, pkgs, ... }:
    let
      gufoQwenASRCmd = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "asr";

        model = "/persist/models/by-capability/audio/Qwen3-ASR-1.7B";
        servedModelName = "qwen3-asr";
        context = 1024;

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
      };
    in
    {
      services.llama-swap.settings.models."qwen3-asr" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenASRCmd}";
        ttl = 120; # Unload after 2 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
