{ config, ... }:

{
  topology.self = {
    name = "Homelab";
    deviceType = "device";
    hardware.info = "Framework Desktop";
    icon = "devices.desktop";

    interfaces.wg0 = {
      addresses = [ "192.168.7.1" ];
      network = "wg0";
      type = "wireguard";
      physicalConnections = [
        (config.lib.topology.mkConnection "oven" "wg0")
      ];

    };
  };
}
