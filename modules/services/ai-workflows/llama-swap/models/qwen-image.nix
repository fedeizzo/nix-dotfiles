# Qwen-Image-2.1-Turbo served by gufo (image modality).
# /v1/images/generations + /v1/images/edits, base64 PNG output.
# Turbo always runs eight steps (~19 s for 1024x1024 on Strix Halo);
# requests must omit `steps` or pass 8.
# Weights: gufo model `qwen-image-2.1-turbo` (~32.5 GB, ~30 GiB resident,
# non-commercial Qwen Research License), synced by gufo-model-sync.
{
  flake.modules.nixos.llama-swap-qwen-image = { inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      gufoQwenImageCmd = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "image";

        model = "${modelsDir}/qwen-image-2.1-turbo";
        # same as the llama-swap key: llama-swap rewrites the request model to it
        servedModelName = "qwen-image-2.1-turbo";

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432; # reference uploads for /v1/images/edits
      };
    in
    {
      services.gufo-model-sync.models = [ "qwen-image-2.1-turbo" ];
      services.llama-swap.settings.models."qwen-image-2.1-turbo" = {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenImageCmd}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 300; # Unload after 5 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
