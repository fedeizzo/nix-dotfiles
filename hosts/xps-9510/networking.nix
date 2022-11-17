{ hostname, pkgs, inputs, config, ... }:

{
  networking.hostName = hostname;
  networking.networkmanager.enable = true;
  networking.useDHCP = false;
  networking.extraHosts = ''
    192.168.7.1 home-lab.wireguard
  '';
  networking.firewall = {
    enable = true;
    checkReversePath = "loose";
    allowedUDPPorts = [ 51821 ];
  };
  services.tailscale.enable = true;
  sops.secrets.xps-9510-wireguard-private-key.sopsFile = ../../secrets.yaml;
  networking.wireguard.enable = true;
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "192.168.7.2/24" ];
      listenPort = 51821;
      privateKeyFile = "${config.sops.secrets.xps-9510-wireguard-private-key.path}";
      peers = [
        {
          # home-lab
          publicKey = "Ug1P6UzLyQ0BFbrfi0G9KLJBNBs+IOisRn3uiLoR5yU=";
          allowedIPs = [ "192.168.7.1" ];
          endpoint = "vpn.fedeizzo.dev:51821";
          dynamicEndpointRefreshSeconds = 5;
          persistentKeepalive = 25;
        }
      ];
    };
  };
  environment.systemPackages = [ inputs.deploy-rs.defaultPackage.x86_64-linux ]
    # VPN for university
    ++ [ pkgs.globalprotect-openconnect ];
  services.globalprotect.enable = true;
  services.globalprotect.csdWrapper = "${pkgs.openconnect}/libexec/openconnect/hipreport.sh";
}
