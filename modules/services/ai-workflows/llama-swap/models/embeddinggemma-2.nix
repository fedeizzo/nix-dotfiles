# EmbeddingGemma 2 (`embeddinggemma-2` provider, `gufo serve embedding`).
# /v1/embeddings for text, image and audio inputs (BF16 safetensors checkpoint).
# Weights:
#   hf download google/embeddinggemma-3b --local-dir /persist/models/store/EmbeddingGemma-2
#   (linked into /persist/models/by-capability/embedding/EmbeddingGemma-2)
{
  flake.modules.nixos.llama-swap-embeddinggemma-2 = { inputs, pkgs, ... }:
    let
      gufoEmbedding = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "embedding";
        provider = "embeddinggemma-2";

        model = "/persist/models/by-capability/embedding/EmbeddingGemma-2";
        servedModelName = "embeddinggemma-2";

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
        # NOTE: Do NOT add a second `--served-model-name forge/...` here (as the
        # chat models do). gufo's /v1/embeddings handler validates the request
        # `model` field against its served-model-name, and llama-swap rewrites
        # every proxied request's model to the config key
        # ("embeddinggemma-2"). A second flag overrides the first (last wins)
        # and the backend would reject every proxied request with 400.
      };
    in
    {
      services.llama-swap.settings.models."embeddinggemma-2" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoEmbedding}";
        ttl = 120; # Unload after 2 minutes idle; fast to reload.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
