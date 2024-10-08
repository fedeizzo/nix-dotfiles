{ config, inputs, ... }:

{
  imports = [
    ../../common/system/networking.nix
  ];

  environment.systemPackages = [ inputs.deploy-rs.defaultPackage.x86_64-linux ];

  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "192.168.7.2/24" ];
      listenPort = 51821;
      privateKeyFile = "${config.sops.secrets.xps-9510-wireguard-private-key.path}";
      peers = [
        {
          # home-lab
          publicKey = "Ug1P6UzLyQ0BFbrfi0G9KLJBNBs+IOisRn3uiLoR5yU=";
          allowedIPs = [ "192.168.7.1" ];
          endpoint = "vpn.fedeizzo.dev:51821";
          dynamicEndpointRefreshSeconds = 5;
          persistentKeepalive = 25;
        }
      ];
    };
  };
}
