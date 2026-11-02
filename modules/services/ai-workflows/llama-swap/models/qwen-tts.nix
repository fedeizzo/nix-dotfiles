# Qwen3-TTS served by gufo (tts modality) with cloned voices.
{
  flake.modules.nixos.llama-swap-qwen-tts = { inputs, pkgs, ... }:
    let
      gufoQwenTTSCmd = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "tts";

        model = "/persist/models/audio/Qwen3-TTS-12Hz-1.7B-Base";
        servedModelName = "qwen3-tts";
        context = 4096;

        voices = {
          narrator_eng = {
            wav = "/persist/models/audio/clear-english-voice.wav";
            text = "It is said with truth that every building is constructed stone by stone, and the same may be said of knowledge. Extract.";
            language = "english";
          };

          narrator_ita = {
            wav = "/persist/models/audio/clear-italian-voice.wav";
            text = "Questo racconto è cresciuto nel corso della narrazione fino a diventare una storia della Grande Guerra dell'Anello, e ha in.";
            language = "italian";
          };

          me = {
            wav = "/persist/models/audio/me.wav";
            text = "ciao il mio nome è Federico sono un ingegnere informatico vivo a Parigi e nel tempo libero mi piace arrampicare";
            language = "italian";
          };
        };

        host = "0.0.0.0";
        port = "\${PORT}";
        maxRequestBytes = 33554432;
      };
    in
    {
      services.llama-swap.settings.models."qwen3-tts" = {
        env = [ "LLAMA_CACHE=/persist/models" "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenTTSCmd}";
        ttl = 120; # Unload after 2 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
