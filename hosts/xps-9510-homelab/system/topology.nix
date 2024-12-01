_:

{
  topology.self = {
    name = "Homelab";
    deviceType = "device";
    hardware.info = "XPS 15 9510";
    icon = "devices.desktop";

    interfaces.wg0 = {
      addresses = [ "192.168.7.1" ];
      network = "wg0";
      type = "wireguard";
    };

    interfaces.eth0 = {
      physicalConnections = [{ node = "router"; interface = "eth1"; }];
    };
  };
}