# LFM3-Audio-2B speech synthesis (`lfm3-audio-tts` provider).
# /v1/audio/speech + /v1/audio/voices (built-in `female`/`male` voices).
# Shares the backbone + mmproj of lfm3-audio-asr (see that file for weights).
# Two variants: `lfm3-audio-tts` on the NPU and `lfm3-audio-tts-gpu` on the GPU.
{
  flake.modules.nixos.llama-swap-lfm3-audio-tts = { lib, inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      mkGufoAudioTts = { npu, servedModelName }: inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "tts";
        provider = "lfm3-audio-tts";

        model = "${modelsDir}/lfm3-audio-2b/LFM3-Audio-2B-Q8_0.gguf";
        inherit servedModelName;
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
        # For the same reason servedModelName must equal the llama-swap key.
        extraArgs = lib.optional npu "--npu";
      };
      mkModel = args: {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${mkGufoAudioTts args}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.gufo-model-sync.models = [ "lfm3-audio-2b" ];
      services.llama-swap.settings.models = {
        "lfm3-audio-tts" = mkModel { npu = true; servedModelName = "lfm3-audio-tts"; };
        "lfm3-audio-tts-gpu" = mkModel { npu = false; servedModelName = "lfm3-audio-tts-gpu"; };
      };
    };
}
