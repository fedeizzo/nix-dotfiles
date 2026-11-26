# LFM3-Audio-2B speech recognition (`lfm3-audio-asr` provider).
# /v1/audio/transcriptions + OpenAI Realtime WebSockets on /v1/realtime.
# The audio tower mmproj is picked up beside the backbone automatically.
# Weights: gufo model `lfm3-audio-2b`, synced by gufo-model-sync.
# Two variants: `lfm3-audio-asr` on the NPU and `lfm3-audio-asr-gpu` on the GPU.
{
  flake.modules.nixos.llama-swap-lfm3-audio-asr = { lib, inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      mkGufoAudioAsr = { npu, servedModelName }: inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "asr";
        provider = "lfm3-audio-asr";

        model = "${modelsDir}/lfm3-audio-2b/LFM3-Audio-2B-Q8_0.gguf";
        inherit servedModelName;
        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
        # NOTE: Do NOT add a second `--served-model-name forge/...` here (as the
        # chat models do). gufo's /v1/audio/transcriptions handler validates the
        # request `model` field against its served-model-name, and llama-swap
        # rewrites every proxied request's model to the config key
        # ("lfm3-audio-asr"). A second flag overrides the first (last wins),
        # so the backend would report "forge/lfm3-audio-asr" and reject every
        # proxied request with 400 invalid_model.
        # For the same reason servedModelName must equal the llama-swap key.
        extraArgs = lib.optional npu "--npu";
      };
      mkModel = args: {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${mkGufoAudioAsr args}";
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
        "lfm3-audio-asr" = mkModel { npu = true; servedModelName = "lfm3-audio-asr"; };
        "lfm3-audio-asr-gpu" = mkModel { npu = false; servedModelName = "lfm3-audio-asr-gpu"; };
      };
    };
}
