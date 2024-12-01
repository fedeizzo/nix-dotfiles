_:

{
  topology.self = {
    name = "Oven";
    deviceType = "device";
    hardware.info = "X1 Carbon 6th Gen";
    icon = "devices.laptop";

    interfaces.wg0 = {
      addresses = [ "192.168.7.3" ];
      network = "wg0";
      type = "wireguard";
      physicalConnections = [
        { node = "homelab"; interface = "wg0"; }
      ];
    };

    interfaces.wlp2s0 = {
      physicalConnections = [{ node = "router"; interface = "wifi"; }];
    };
  };
}
