{
  flake.modules.nixos.tailscale = { ... }: {
    services.tailscale = {
      enable = true;
      extraSetFlags = [ "--netfilter-mode=nodivert" ];
    };

    # Required for tailscale exit nodes and subnet routing
    networking.firewall.checkReversePath = "loose";
  };
}
