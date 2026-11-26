# LFM3-1.7B served by gufo with the lfm3 extension.
# Two variants: `lfm3-1.7b` on the NPU and `lfm3-1.7b-gpu` on the GPU.
{
  flake.modules.nixos.llama-swap-lfm3 = { lib, inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      mkGufoLfm3 = { npu, servedModelName, forgeName }: inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "llm";

        model = "${modelsDir}/lfm3-1.7b/LFM3-1.7B-Q8_0.gguf";
        host = "0.0.0.0";
        port = "\${PORT}";

        # Server
        # sessions = 2;
        # maxConnections = 16;
        # maxRequestBytes = 8388608;
        # verbose = false;

        # Model
        inherit servedModelName;
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

        extraArgs = lib.optional npu "--npu" ++ [
          "--served-model-name"
          forgeName
        ];
      };
      mkModel = args: {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${mkGufoLfm3 args}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 120; # Unload after 2 minutes idle; fast to reload.
        timeouts.responseHeader = 600;
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.gufo-model-sync.models = [ "lfm3-1.7b" ];
      services.llama-swap.settings.models = {
        "lfm3-1.7b" = mkModel { npu = true; servedModelName = "lfm3-1.7b"; forgeName = "forge/lfm3-1.7"; };
        "lfm3-1.7b-gpu" = mkModel { npu = false; servedModelName = "lfm3-1.7b-gpu"; forgeName = "forge/lfm3-1.7-gpu"; };
      };
    };
}
