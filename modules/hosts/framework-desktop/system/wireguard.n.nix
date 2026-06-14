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

  flake.modules.nixos.framework-desktop = { config, pkgs, ... }: {
    systemConstants.wireguard = {
      ip = "192.168.7.1";
      privateKeyFile = config.sops.secrets.homelab-wireguard-private-key.path;
    };

    networking.wireguard.interfaces = {
      wg0 = {
        postSetup = ''
          WAN_IFACE="eth0"
          DESKTOP_PC="192.168.1.67"

          ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -d $DESKTOP_PC -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o $WAN_IFACE -d $DESKTOP_PC -j MASQUERADE
        '';

        postShutdown = ''
          WAN_IFACE="eth0"
          DESKTOP_PC="192.168.1.50"

          ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -d $DESKTOP_PC -j ACCEPT
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o $WAN_IFACE -d $DESKTOP_PC -j MASQUERADE
        '';
        peers = [
          {
            # Laptop xps 9510
            publicKey = "3i/aGGX9iOSyQ/FLbrKyvaClHzqUq3mGHX4oneerbm0=";
            allowedIPs = [ "192.168.7.2/32" ];
          }
          {
            # Laptop x1 nano
            publicKey = "zzO+Pxb/p+Yzw5eGDfOcg4axjm91vRtohUZ6o2talzY=";
            allowedIPs = [ "192.168.7.3/32" ];
          }
          {
            # Samsung S24
            publicKey = "+Z/mss41U9u0R7ss5GqAmL+PICUd0Wu8yb5/MPDaiW8=";
            allowedIPs = [ "192.168.7.4/32" ];
          }
        ];
      };
    };
  };
}
