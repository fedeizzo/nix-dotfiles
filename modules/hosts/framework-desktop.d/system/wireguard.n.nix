{
  flake.modules.nixos.wireguard = { ... }: {
    networking.wireguard.interfaces.wg0.peers = [
      {
        name = "homelab";
        publicKey = "Ug1P6UzLyQ0BFbrfi0G9KLJBNBs+IOisRn3uiLoR5yU=";
        allowedIPs = [ "192.168.7.1/32" "192.168.1.67/32" ];
        endpoint = "vpn.fedeizzo.dev:51821";
        persistentKeepalive = 25;
      }
    ];
  };

  # flake.modules.nixos.homelab = { config, ... }: {
  #   systemConstants.wireguard = {
  #     ip = "192.168.7.1";
  #     privateKeyFile = config.sops.secrets.homelab-wireguard-private-key.path;
  #   };
  # };
}
