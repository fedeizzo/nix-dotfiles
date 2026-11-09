# Qwen3.8-Flash-Next with MTP speculative decoding, served by gufo.
# Vision: image input via the BF16 projector.
#   hf download unsloth/Qwen3.8-Flash-Next-GGUF \
#     --revision d96201194f103c906994f283f504209dc8d2a10c \
#     --include "mmproj-BF16.gguf" \
#     --local-dir /persist/models/by-capability/language/Qwen3.8-Flash-Next
{
  flake.modules.nixos.llama-swap-flash-next = { inputs, pkgs, ... }:
    let
      gufoQwenNext = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "llm";
        model = "/persist/models/by-capability/language/Qwen3.8-Flash-Next/Qwen3.8-Flash-Next-UD-Q4_K_XL-00001-of-00004.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";
        verbose = false;

        # Requests run serially on this model (no batching plan); each session
        # holds its own state (~24 KiB/token of context).
        sessions = 2;

        servedModelName = "qwen-flash";
        context = 260000;
        # Greedy verification: only temperature-0 requests take multi-token steps.
        speculative = "mtp";
        mtpModel = "/persist/models/by-capability/language/Qwen3.8-Flash-Next/mtp-Qwen3.8-Flash-Next-shared-Q8_0.gguf";
        # draftTokens = 3;

        # 2048-token chunks keep the expert GEMMs on the matrix-core route.
        prefillChunk = 2048;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        temperature = 1.0;
        maxTokens = 8192;
        topP = 0.95;
        topK = 20;
        minP = 0.0;
        minKeep = 0;
        seed = -1;
        repeatPenalty = 1.0;
        repeatLastN = 64;
        frequencyPenalty = 0.0;
        presencePenalty = 0.0;

        # Prompt-end snapshots (~50 KiB/token + 111 MiB): a 100k prompt is
        # ~5 GiB, so 32 GiB holds about six long conversations. Staging must
        # fit one whole payload; 8 GiB covers prompts up to ~160k tokens.
        # cacheDisk = "/var/cache/llama-swap/gufo-qwen-flash";
        # cacheDiskBytes = 34359738368;       # 32 GiB
        # cacheDiskStagingBytes = 8589934592; # 8 GiB
        extraArgs = [
          "--served-model-name"
          "forge/qwen-flash"
          # Vision projector; gufo auto-detects it beside the backbone, pass
          # it explicitly so a missing file fails fast at startup.
          "--mmproj"
          "/persist/models/by-capability/language/Qwen3.8-Flash-Next/mmproj-BF16.gguf"
        ];
      };
    in
    {
      services.llama-swap.settings.models."qwen-flash" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenNext}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
