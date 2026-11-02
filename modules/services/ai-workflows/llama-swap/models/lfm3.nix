# LFM3-1.7B served by gufo with the lfm3 extension.
{
  flake.modules.nixos.llama-swap-lfm3 = { inputs, pkgs, ... }:
    let
      gufoWithLfm3 = inputs.gufo.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
        extensions = [ inputs.lfm3-gufo-extension ];
      };
      gufoLfm3 = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = gufoWithLfm3;
        modality = "llm";

        model = "/persist/models/LFM3-1.7B-Q8_0.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # sessions = 2;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        servedModelName = "lfm3-1.7b";
        context = 250000; # supports up to 262144

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
        think = "on"; # or "off"
        preserveThinking = "auto";

        extraArgs = [
          "--served-model-name"
          "forge/lfm3-1.7"
        ];
      };
    in
    {
      services.llama-swap.settings.models."lfm3-1.7b" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoLfm3}";
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
      };
    };
}
