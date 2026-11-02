{
  flake.modules.nixos.homelab = {
    # forge's llama-swap, reachable as `forge/<model>`; unqualified names stay local.
    services.llama-swap.settings.peers.forge = {
      proxy = "http://192.168.1.89:11435";
      models = [ "qwen3.8-27b" "qwen-flash" "lfm3-1.7b" "lfm3-vl" "lfm3-audio-asr" "lfm3-audio-tts" "d1" ];
      timeouts.responseHeader = 600; # cold start of a 27B model
    };
  };
}
