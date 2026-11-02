# DeepSeek-V4-Flash (IQ2XXS imatrix) with DSpark speculative decoding, served by gufo.
{
  flake.modules.nixos.llama-swap-ds4 = { inputs, pkgs, ... }:
    let
      gufoDs4 = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "llm";

        model = "/persist/models/by-capability/language/DeepSeek-V4-Flash/DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix-0731.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # DS4 batches up to 8 sessions. DSpark per-user gain: C2 ~1.8x, C4 ~1.35x,
        # C6+ ~none. Use 1 for solo, 2 for shared.
        sessions = 4;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        servedModelName = "ds4";
        context = 131072;
        # sampled requests fall back to autoregressive decode. See gufo#232.
        speculative = "dspark";
        dsparkModel = "/persist/models/by-capability/language/DeepSeek-V4-Flash/DeepSeek-V4-Flash-DSpark-support-0731.gguf";
        # draftTokens = 3;  # optional ceiling; model-owned adaptive width otherwise

        # Scheduling (all defaults)
        prefillChunk = 512;
        maxPending = 16;
        maxPendingPerClient = 4;
        requestTimeoutMs = 0;
        maxOutputBytes = 1048576;
        maxBufferedOutputBytes = 65536;
        maxBufferedOutputTotal = 262144;

        # Sampling
        temperature = 0.0; # 0.0 to get DSpark on default requests
        maxTokens = 8192;
        topP = 0.95;
        topK = 0;
        minP = 0.0;
        minKeep = 0;
        seed = -1;
        repeatPenalty = 1.0;
        repeatLastN = 64;
        frequencyPenalty = 0.0;
        presencePenalty = 0.0;

        # Cache
        cacheDisk = "/var/cache/llama-swap/gufo-ds4";
        cacheDiskBytes = 4294967296;
        cacheDiskStagingBytes = 536870912;
      };
    in
    {
      services.llama-swap.settings.models."ds4" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoDs4}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
