# Qwen3.8-27B (UD-Q4_K_XL) with DFlash-2 speculative decoding, served by gufo.
{
  flake.modules.nixos.llama-swap-qwen27b = { inputs, pkgs, ... }:
    let
      gufoQwen27 = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "llm";

        model = "/persist/models/models--unsloth--Qwen3.8-27B-GGUF/snapshots/fe1e2a23d973adb629709749dc4f6756df66ef10/Qwen3.8-27B-UD-Q4_K_XL.gguf";
        dflashModel = "/persist/models/Qwen3.8-27B-DFlash2-Q8_0.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # sessions = 5;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        servedModelName = "qwen3.8-27b";
        context = 256000;

        # Speculative Decoding (DFlash-2)
        speculative = "dflash2";
        draftTokens = 7;
        # draftPolicy = "auto";
        minDraftTokens = 1;

        # Scheduling
        prefillChunk = 512;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        # Sampling — Qwen3.8 official thinking-mode preset
        temperature = 0.8;
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

        # Reasoning
        think = "on";
        reasoningEffort = "medium";
        preserveThinking = "auto";

        # Cache
        # cacheDisk = "/var/cache/gufo";
        # cacheDiskBytes = 4294967296;
        # cacheDiskStagingBytes = 536870912;
        extraArgs = [
          "--served-model-name"
          "qwen3.8-27b-nothink"
          "--served-model-name"
          "forge/qwen3.8-27b"
        ];
      };
    in
    {
      services.llama-swap.settings.models."qwen3.8-27b" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwen27}";
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ "Qwen3.8-27B" ];
        filters.setParamsByID."qwen3.8-27b-nothink".chat_template_kwargs.enable_thinking = false;
      };
    };
}
