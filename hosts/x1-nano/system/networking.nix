{ hostname, config, ... }:

{
  networking = {
    hostName = hostname;
    networkmanager = {
      enable = true;
      # Disable NetworkManager's internal DNS resolution
      dns = "none";
      wifi.powersave = true;
    };

    # These options are unnecessary when managing DNS ourselves
    dhcpcd.enable = false;
    useDHCP = false;
    extraHosts = ''
      192.168.7.1 homelab
    '';
    nameservers = [ "192.168.7.1" "1.1.1.1" ];
    firewall = {
      enable = true;
      checkReversePath = "loose";
      allowedTCPPorts = [ 2300 ];
    };
    interfaces.wlp0s20f3 = { }; # define the interface for the topology network generation
  };

  services.tailscale = {
    enable = true;
    extraSetFlags = [ "--netfilter-mode=nodivert" ];
  };
}
