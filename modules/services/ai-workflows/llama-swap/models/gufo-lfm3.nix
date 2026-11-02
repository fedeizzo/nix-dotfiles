# Gufo with the lfm3 extension built in. Exposes pkgs.gufo-lfm3 for the
# LFM3-family models (text, VL, audio ASR/TTS, d1 decisions).
# Providers: lfm3 (LFM3-1.7B / LFM3-VL-2B), lfm3-audio-asr, lfm3-audio-tts, d1.
{
  flake.modules.nixos.llama-swap-gufo-lfm3 = { inputs, ... }:
    {
      nixpkgs.overlays = [
        (final: prev: {
          gufo-lfm3 = inputs.gufo.packages.${prev.stdenv.hostPlatform.system}.default.override {
            extensions = [ inputs.lfm3-gufo-extension ];
          };
        })
      ];
    };
}
