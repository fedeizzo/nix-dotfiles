# LFM3-VL-2B: text + still-image input through the extension's `lfm3`
# text provider. Weights:
#   hf download LiquidAI/LFM3-VL-2B-GGUF LFM3-VL-2B-Q8_0.gguf \
#     mmproj-LFM3-VL-2B-BF16.gguf --revision b36554cfd35854b06429900131730cd80f092cdb \
#     --local-dir /persist/models/store/LFM3-VL-2B
{
  flake.modules.nixos.llama-swap-lfm3-vl = { inputs, pkgs, ... }:
    let
      gufoVl = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "llm";
        provider = "lfm3";

        model = "/persist/models/by-capability/vision/LFM3-VL-2B/LFM3-VL-2B-Q8_0.gguf";
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
          "--mmproj"
          "/persist/models/by-capability/vision/LFM3-VL-2B/mmproj-LFM3-VL-2B-BF16.gguf"
          "--served-model-name"
          "forge/lfm3-vl"
        ];
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.llama-swap.settings.models."lfm3-vl" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoVl}";
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
      };
    };
}
