# Qwen3-TTS served by gufo (tts modality) with cloned voices.
# Weights: gufo model `qwen3-tts`, synced by gufo-model-sync.
# Voice samples live in /var/lib/gufo/samples (copied by hand, not synced).
{
  flake.modules.nixos.llama-swap-qwen-tts = { inputs, pkgs, config, ... }:
    let
      modelsDir = config.services.gufo-model-sync.directory;
      samplesDir = "/var/lib/gufo/samples";
      gufoQwenTTSCmd = inputs.gufo.lib.${pkgs.stdenv.hostPlatform.system}.mkGufoServe {
        modality = "tts";

        model = "${modelsDir}/qwen3-tts";
        servedModelName = "qwen3-tts";
        context = 4096;

        voices = {
          narrator_eng = {
            wav = "${samplesDir}/clear-english-voice.wav";
            text = "It is said with truth that every building is constructed stone by stone, and the same may be said of knowledge. Extract.";
            language = "english";
          };

          narrator_ita = {
            wav = "${samplesDir}/clear-italian-voice.wav";
            text = "Questo racconto è cresciuto nel corso della narrazione fino a diventare una storia della Grande Guerra dell'Anello, e ha in.";
            language = "italian";
          };

          me = {
            wav = "${samplesDir}/me.wav";
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
      services.gufo-model-sync.models = [ "qwen3-tts" ];
      systemd.services.llama-swap.serviceConfig.ReadOnlyPaths = [ samplesDir ];
      services.llama-swap.settings.models."qwen3-tts" = {
        env = [ "GPU_MAX_HW_QUEUES=1" ];
        cmd = "${gufoQwenTTSCmd}";
        checkEndpoint = "/ready"; # gufo /health answers 200 during loading; /ready gates on model readiness
        ttl = 120; # Unload after 2 minutes idle.
        timeouts.responseHeader = 600;
        aliases = [ ];
      };
    };
}
