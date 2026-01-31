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
      allowedUDPPorts = [ 51821 ];
      allowedTCPPorts = [ 2300 ];
    };
    interfaces.wlp0s20f3 = { }; # define the interface for the topology network generation
    wireguard.enable = true;
    wireguard.interfaces = {
      wg0 = {
        ips = [ "192.168.7.3/24" ];
        listenPort = 51821;
        privateKeyFile = "${config.sops.secrets.x1-wireguard-private-key.path}";
        peers = [
          {
            name = "homelab";
            # home-lab
            publicKey = "Ug1P6UzLyQ0BFbrfi0G9KLJBNBs+IOisRn3uiLoR5yU=";
            allowedIPs = [ "192.168.7.1" "192.168.1.67/32" ];
            endpoint = "vpn.fedeizzo.dev:51821";
            # dynamicEndpointRefreshSeconds = 5;
            persistentKeepalive = 25;
          }
        ];
      };
    };
  };

  # fix the annoying bug for which wireguard has to be restarted after boot
  systemd.services."wireguard-wg0-peer-homelab" = {
    serviceConfig = { Restart = "on-failure"; RestartSec = "10s"; };
    unitConfig.StartLimitIntervalSec = 0;
  };
}
