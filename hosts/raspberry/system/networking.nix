{ pkgs, config, hostname, ... }:

{
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      unmanaged = [ "wg0" ];
    };
    useDHCP = false;
    # defaultGateway = "192.168.1.1";
    nameservers = [ "1.1.1.1" ];
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "192.168.1.65";
        prefixLength = 24;
      }];
    };
    nat = {
      enable = true;
      internalInterfaces = [ "wg0" ];
      externalInterface = "eth0";
    };
    firewall = {
      enable = true;
      interfaces.eth0.allowedTCPPorts = [ 443 ];
      trustedInterfaces = [ "tailscale0" "wg0" ];
      allowedUDPPorts = [ 51820 51821 ];
      allowedTCPPorts = [ 6443 ];
      checkReversePath = "loose";
    };
  };
  # fix: https://github.com/NixOS/nixpkgs/issues/296953
  systemd.services.NetworkManager-wait-online = {
    serviceConfig = {
      ExecStart = [ "" "${pkgs.networkmanager}/bin/nm-online -q" ];
    };
  };

  services.openssh = {
    enable = true;
    allowSFTP = false; # Don't set this if you need sftp
    openFirewall = false;
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
    settings.X11Forwarding = false;
    settings.PermitRootLogin = "yes";
  };
  services.fail2ban.enable = true;

  networking.wireguard.enable = true;
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "192.168.7.1/24" ];
      listenPort = 51821;
      privateKeyFile = "${config.sops.secrets.homelab-wireguard-private-key.path}";
      peers = [
        {
          # Laptop xps 9510
          publicKey = "3i/aGGX9iOSyQ/FLbrKyvaClHzqUq3mGHX4oneerbm0=";
          allowedIPs = [ "192.168.7.2/32" ];
        }
      ];
    };
  };
}
