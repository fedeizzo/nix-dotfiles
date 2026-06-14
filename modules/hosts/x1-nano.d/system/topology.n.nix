{ config, ... }:

{
  flake.modules.nixos.x1-nano = { inputs, ... }: {
    imports = [ inputs.nix-topology.nixosModules.default ];
    topology.self = {
      name = "Oven";
      deviceType = "device";
      hardware.info = "X1 Nano 6th Gen";
      icon = "devices.laptop";

      interfaces.wg0 = {
        addresses = [ "192.168.7.3" ];
        renderer.hidePhysicalConnections = false;
        type = "wireguard";
        physicalConnections = [
          (config.lib.topology.mkConnection "homelab" "wg0")
        ];
      };
    };
  };
}
