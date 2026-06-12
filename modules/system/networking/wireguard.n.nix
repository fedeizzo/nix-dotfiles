{ lib, ... }:

{
  flake.modules.nixos.wireguard = { config, ... }: {
    options.systemConstants.wireguard = {
      ip = lib.mkOption {
        type = lib.types.str;
        description = "The wireguard IP address for this host without subnet mask";
      };
      privateKeyFile = lib.mkOption {
        type = lib.types.str;
        description = "Path to the wireguard private key file";
      };
    };

    config = {
      networking.wireguard.enable = true;
      networking.firewall.allowedUDPPorts = [ 51821 ];

      networking.wireguard.interfaces.wg0 = {
        ips = [ "${config.systemConstants.wireguard.ip}/24" ];
        listenPort = 51821;
        privateKeyFile = config.systemConstants.wireguard.privateKeyFile;
        mtu = 1280;
      };
    };
  };
}
