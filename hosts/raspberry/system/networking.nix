{ pkgs, config, hostname, ... }:

{
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      # unmanaged = [ "wg0" ];
    };
    useDHCP = false;
    # defaultGateway = "192.168.1.1";
    # nameservers = [ "1.1.1.1" ];
    interfaces.eth0 = {
      ipv4.addresses = [{
        address = "192.168.1.90";
        prefixLength = 24;
      }];
    };
    nat = {
      enable = true;
      # internalInterfaces = [ "wg0" ];
      externalInterface = "eth0";
    };
    firewall = {
      enable = true;
      interfaces.eth0.allowedTCPPorts = [ ];
      # trustedInterfaces = [ "wg0" ];
      allowedUDPPorts = [ ];
      allowedTCPPorts = [ ];
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
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      PermitRootLogin = "yes";
    };
  };
  services.fail2ban.enable = true;
}
