# LFM3-Audio-2B speech synthesis (`lfm3-audio-tts` provider).
# /v1/audio/speech + /v1/audio/voices (built-in `female`/`male` voices).
# Shares the backbone + mmproj of lfm3-audio-asr (see that file for weights).
{
  flake.modules.nixos.llama-swap-lfm3-audio-tts = { inputs, pkgs, ... }:
    let
      gufoAudioTts = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "tts";
        provider = "lfm3-audio-tts";

        model = "/persist/models/by-capability/audio/LFM3-Audio-2B/LFM3-Audio-2B-Q8_0.gguf";
        servedModelName = "lfm3-audio-tts";
        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
        # NOTE: Do NOT add a second `--served-model-name forge/...` here (as the
        # chat models do). gufo's /v1/audio/speech handler validates the
        # request `model` field against its served-model-name, and llama-swap
        # rewrites every proxied request's model to the config key
        # ("lfm3-audio-tts"). A second flag overrides the first (last wins),
        # so the backend would report "forge/lfm3-audio-tts" and reject every
        # proxied request with 400 invalid_model.
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.llama-swap.settings.models."lfm3-audio-tts" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoAudioTts}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 120; # Unload after 2 minutes idle; fast to reload.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
