# LFM3-Audio-2B speech recognition (`lfm3-audio-asr` provider).
# /v1/audio/transcriptions + OpenAI Realtime WebSockets on /v1/realtime.
# The audio tower mmproj is picked up beside the backbone automatically.
# Weights:
#   REV=02355c1a141e464a064f2ab2c276638dbcb7f38a
#   hf download LiquidAI/LFM3-Audio-2B-GGUF LFM3-Audio-2B-Q8_0.gguf \
#     mmproj-LFM3-Audio-2B-BF16.gguf --revision $REV \
#     --local-dir /persist/models/lfm3-audio
{
  flake.modules.nixos.llama-swap-lfm3-audio-asr = { inputs, pkgs, ... }:
    let
      gufoAudioAsr = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "asr";
        provider = "lfm3-audio-asr";

        model = "/persist/models/lfm3-audio/LFM3-Audio-2B-Q8_0.gguf";
        servedModelName = "lfm3-audio-asr";
        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;

        extraArgs = [
          "--served-model-name"
          "forge/lfm3-audio-asr"
        ];
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.llama-swap.settings.models."lfm3-audio-asr" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoAudioAsr}";
        ttl = 120; # Unload after 2 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
