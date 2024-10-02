{ hostname, inputs, ... }:

{
  networking.hostName = hostname;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.extraHosts = ''
    192.168.7.1 homelab
  '';
  networking.firewall = {
    enable = true;
    checkReversePath = "loose";
    allowedUDPPorts = [ 51821 ];
  };
  networking.wireguard.enable = true;
}
