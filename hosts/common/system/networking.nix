{ hostname, ... }:

{
  networking = {
    hostName = hostname;
    networkmanager.enable = true;
    useDHCP = false;
    extraHosts = ''
      192.168.7.1 homelab
    '';
    nameservers = [ "192.168.7.1" "1.1.1.1" ];
    firewall = {
      enable = true;
      checkReversePath = "loose";
      allowedUDPPorts = [ 51821 ];
    };
    wireguard.enable = true;
  };
}
