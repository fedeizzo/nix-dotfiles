{
  flake.modules.nixos.homelab = {
    # forge's llama-swap, reachable as `forge/<model>`; unqualified names stay local.
    services.llama-swap.settings.peers.forge = {
      proxy = "http://192.168.1.89:11435";
      models = [ "qwen3.8-27b" "qwen-flash" ];
      timeouts.responseHeader = 600; # cold start of a 27B model
    };
  };
}
