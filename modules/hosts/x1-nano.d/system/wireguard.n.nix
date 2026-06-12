{
  flake.modules.nixos.wireguard = { config, ... }: {
    networking.wireguard.interfaces.wg0.peers = [
      {
        name = "x1-nano";
        publicKey = "zzO+Pxb/p+Yzw5eGDfOcg4axjm91vRtohUZ6o2talzY=";
        allowedIPs = [ "192.168.7.3/32" ];
      }
    ];
  };

  flake.modules.nixos.x1-nano = { config, ... }: {
    systemConstants.wireguard = {
      ip = "192.168.7.3";
      privateKeyFile = config.sops.secrets.x1-wireguard-private-key.path;
    };

    sops.secrets.x1-wireguard-private-key = { };
  };
}
