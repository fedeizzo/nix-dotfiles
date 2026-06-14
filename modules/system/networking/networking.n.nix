{
  flake.modules.nixos.networking = { hostname, ... }: {
    networking = {
      hostName = hostname;
      networkmanager.enable = true;
      useDHCP = false;
      extraHosts = ''
        192.168.7.1 homelab
      '';
    };
  };
}
