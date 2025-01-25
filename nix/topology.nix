{ lib, ... }:

{
  topology = {
    modules = [
      (
        { config, ... }:
        let
          inherit (config.lib.topology)
            mkConnection
            mkInternet
            mkRouter
            ;
        in
        {
          nodes = {
            internet = mkInternet {
              connections = mkConnection "router" "wan1";
            };

            router = mkRouter "Router" {
              interfaceGroups = [
                [ "eth1" "wifi" ]
                [ "wan1" ]
              ];
              interfaces = {
                eth1 = {
                  network = "home";
                  addresses = [ "192.168.1.254/24" ];
                };
                wifi = {
                  network = "home";
                  addresses = [ "192.168.1.254/24" ];
                };
              };
              connections.eth1 = mkConnection "homelab" "eth0";
              connections.wifi = mkConnection "oven" "wlp2s0";
            };
          };

          networks = {
            home = {
              name = "Home";
              cidrv4 = "192.168.1.0/24";
            };
            wg0 = {
              name = "Wireguard VPN";
              cidrv4 = "192.168.7.0/24";
            };
          };
        }
      )
    ];
  };
}
