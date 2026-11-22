# LFM3-VL-2B: text + still-image input through the extension's `lfm3`
# text provider. Weights: gufo model `lfm3-vl-2b`, synced by gufo-model-sync.
{
  flake.modules.nixos.llama-swap-lfm3-vl = { inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      gufoVl = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "llm";
        provider = "lfm3";

        model = "${modelsDir}/lfm3-vl-2b/LFM3-VL-2B-Q8_0.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Model
        servedModelName = "lfm3-vl";
        context = 16384;
        sessions = 2;
        speculative = "off";

        # Scheduling
        prefillChunk = 512;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        # Sampling — LFM3 official preset
        temperature = 0.1;
        maxTokens = 8192;
        topP = 1.0;
        topK = 50;
        minP = 0.0;
        minKeep = 0;
        seed = -1;
        repeatPenalty = 1.1;
        repeatLastN = 64;
        frequencyPenalty = 0.0;
        presencePenalty = 0.0;

        # Reasoning
        think = "on";
        preserveThinking = "auto";

        extraArgs = [
          "--npu"
          "--mmproj"
          "${modelsDir}/lfm3-vl-2b/mmproj-LFM3-VL-2B-BF16.gguf"
          "--served-model-name"
          "forge/lfm3-vl"
        ];
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.gufo-model-sync.models = [ "lfm3-vl-2b" ];
      services.llama-swap.settings.models."lfm3-vl" = {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoVl}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 120; # Unload after 2 minutes idle; fast to reload.
        timeouts.responseHeader = 600;
      };
    };
}
