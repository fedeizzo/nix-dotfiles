{ lib, ... }:

{
  topology = {
    modules = [
      {
        nodes.internet = {
          name = "Internet";
          deviceType = "internet";
          # hardware.image = ../icons/devices/cloud.svg;
          interfaces."*".physicalConnections = [
            { node = "router"; interface = "wan1"; }
          ];
        };

        nodes.router = {
          name = "Router";
          deviceType = "router";
          hardware.info = "FreeBox Pop";
          interfaces = {
            eth1 = {
              network = "home";
              addresses = [ "192.168.1.254/24" ];
              sharesNetworkWith = [ (x: lib.elem x [ "wifi" "eth1" ]) ];
              physicalConnections = [
                { node = "homelab"; interface = "eth0"; }
              ];
            };
            wifi = {
              network = "home";
              addresses = [ "192.168.1.254/24" ];
              sharesNetworkWith = [ (x: lib.elem x [ "wifi" "eth1" ]) ];
              physicalConnections = [
                { node = "oven"; interface = "wlp2s0"; }
              ];
            };
            wan1 = {
              # physicalConnections = [
              #   { node = "internet"; interface = "*"; }
              # ];
            };
          };
        };

        networks = {
          wg0 = {
            name = "Wireguard VPN";
            cidrv4 = "192.168.7.0/24";
          };
          home = {
            name = "Home";
            cidrv4 = "192.168.1.0/24";
          };
        };
      }
    ];
  };
}
