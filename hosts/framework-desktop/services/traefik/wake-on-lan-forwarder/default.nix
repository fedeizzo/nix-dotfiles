{ lib, pkgs, config, ... }:

{
  systemd.services.wake-on-lan-forwardauth = {
    description = "Wake-on-LAN ForwardAuth service for Traefik";
    after = [ "network.target" ]; # Start after networking is up
    wantedBy = [ "multi-user.target" ]; # Start during normal multi-user boot

    serviceConfig = {
      Type = "simple"; # The service runs in the foreground
      DynamicUser = true; # Systemd creates a transient non-root user automatically
      ExecStart = (lib.getExe (pkgs.callPackage ./build.nix { }));

      # Harden the service
      PrivateTmp = true; # Isolated /tmp directory
      ProtectSystem = "strict"; # Make /usr, /boot, /etc read-only
      ProtectHome = true; # Restrict access to /home
      NoNewPrivileges = true; # Prevent privilege escalation
      PrivateDevices = true; # Hide raw device nodes
      RestrictAddressFamilies = "AF_INET AF_INET6"; # Only allow IPv4/IPv6 networking

      # Restart settings
      Restart = "always"; # Restart if the service exits
      RestartSec = 5; # Wait 5 seconds before restarting

      EnvironmentFile = config.sops.secrets.wake-on-lan-forwardauth.path;
    };
  };

  sops.secrets.wake-on-lan-forwardauth = {
    format = "dotenv";
    mode = "0400";
    restartUnits = [
      "wake-on-lan-forwardauth.service"
    ];
    sopsFile = ./wake-on-lan-forwarder-homelab-secrets.env;
    key = ""; # to map the whole file as a secret
  };
}
