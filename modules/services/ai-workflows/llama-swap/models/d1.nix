# d1-Omni-2B decisions (`d1` provider): noul/choice/score/box questions over
# text, images and audio without generating text. Served by `gufo serve decision`.
# Weights:
#   hf download LiquidAI/d1-Omni-2B-GGUF d1-Omni-2B-Q8_0.gguf \
#     mmproj-d1-Omni-2B-BF16.gguf --local-dir /persist/models/store/D1-Omni-2B
{
  flake.modules.nixos.llama-swap-d1 = { inputs, pkgs, ... }:
    let
      gufoD1 = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        gufoPackage = pkgs.gufo-lfm3;
        modality = "decision";
        provider = "d1";

        model = "/persist/models/by-capability/omni/D1-Omni-2B/d1-Omni-2B-Q8_0.gguf";
        servedModelName = "d1";
        context = 8192; # default; bounded at 16384
        host = "0.0.0.0";
        port = "\${PORT}";

        extraArgs = [
          "--mmproj"
          "/persist/models/by-capability/omni/D1-Omni-2B/mmproj-d1-Omni-2B-BF16.gguf"
          "--served-model-name"
          "forge/d1"
        ];
      };
    in
    {
      imports = [ inputs.self.modules.nixos.llama-swap-gufo-lfm3 ];
      services.llama-swap.settings.models."d1" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoD1}";
        ttl = 600; # Unload after 10 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
