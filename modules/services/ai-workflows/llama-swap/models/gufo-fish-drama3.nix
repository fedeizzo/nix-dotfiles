# Gufo with the Fish Audio Drama-3 extension built in. Exposes pkgs.gufo-fish-drama3
# for the drama-3 TTS model. Kept separate from gufo-lfm3 so the LFM3 family and
# its preloaded models keep their own cached Gufo build.
# Provider: fish-drama3-tts (speech synthesis + zero-shot voice cloning).
{
  flake.modules.nixos.llama-swap-gufo-fish-drama3 = { inputs, ... }:
    {
      nixpkgs.overlays = [
        (final: prev: {
          gufo-fish-drama3 = inputs.gufo.packages.${prev.stdenv.hostPlatform.system}.default.override {
            extensions = [ inputs.fish-drama3-gufo-extension ];
          };
        })
      ];
    };
}
