{ pkgs, hostname, config, ... }:

{
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
      unmanaged = [ "wg0" ];
    };
    useDHCP = false;
    extraHosts = ''
      192.168.7.1 homelab
    '';
    defaultGateway = "192.168.1.254";
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
      interfaces.eth0 = {
        allowedTCPPorts = [
          443 # https
        ];
        allowedUDPPorts = [
          53 # blocky
        ];
      };
      interfaces.wg0.allowedTCPPorts = [
        # streaming
        8989 # sonarr
        7878 # radarr
        9696 # prowlarr
        # 8191 # flaresolverr
        8112 # deluge
        6767 # bazarr
        5055 # jellyseerr
        9854 # dashboard
        config.services.paperless.port
        51000
        28982 # paperless-gpt
        28983 # paperless-ai
      ];
      trustedInterfaces = [ "wg0" ];
      allowedUDPPorts = [
        51821 # wireguard
      ];
      allowedTCPPorts = [
        33333
      ];
      checkReversePath = "loose";
      extraCommands = ''
        # Allow wireguard interface to hit 192.168.7.1:443
        iptables -A INPUT -i wg0 -p tcp -d 192.168.7.1 --dport 443 -j ACCEPT
        # Allow loopback interface to hit 192.168.7.1:443
        iptables -A INPUT -i lo -p tcp -d 192.168.7.1 --dport 443 -j ACCEPT
        # Drop everything else going to 192.168.7.1:443
        iptables -A INPUT -p tcp -d 192.168.7.1 --dport 443 -j DROP
      '';
      extraStopCommands = ''
        iptables -D INPUT -i wg0 -p tcp -d 192.168.7.1 --dport 443 -j ACCEPT || true
        iptables -D INPUT -i lo -p tcp -d 192.168.7.1 --dport 443 -j ACCEPT || true
        iptables -D INPUT -p tcp -d 192.168.7.1 --dport 443 -j DROP || true
      '';
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
  services.fail2ban = {
    enable = true;
    maxretry = 5;
    bantime = "24h"; # Ban IPs for one day on the first ban
    # extraPackages = [ pkgs.ipset ];
    # banaction = "iptables-ipset-proto6-allports";

    bantime-increment = {
      enable = true;
      multipliers = "1 2 4 8 16 32 64";
      maxtime = "168h"; # Do not ban for more than 1 week
      overalljails = true; # Calculate the bantime based on all the violations
    };

    jails = {
      traefik-general-forceful-browsing.settings = {
        enabled = true;
        filter = "traefik-general-forceful-browsing";
        logpath = "/var/volumes/traefik/log/access.json";
      };
    };

    ignoreIP = [
      # "192.168.7.2/32"
      "192.168.7.3/32"
      # "192.168.7.4/32"
    ];
  };
  environment.etc = {
    "fail2ban/filter.d/traefik-general-forceful-browsing.conf".text = ''
      [INCLUDES]

      [Definition]

      # fail regex based on traefik JSON access logs with enabled user agent logging
      failregex = ^{"ClientAddr":"<F-CLIENTADDR>.*</F-CLIENTADDR>","ClientHost":"<HOST>","ClientPort":"<F-CLIENTPORT>.*</F-CLIENTPORT>","ClientUsername":"<F-CLIENTUSERNAME>.*</F-CLIENTUSERNAME>","DownstreamContentSize":<F-DOWNSTREAMCONTENTSIZE>.*</F-DOWNSTREAMCONTENTSIZE>,"DownstreamStatus":<F-DOWNSTREAMSTATUS>.*</F-DOWNSTREAMSTATUS>,"Duration":<F-DURATION>.*</F-DURATION>,"OriginContentSize":<F-ORIGINCONTENTSIZE>.*</F-ORIGINCONTENTSIZE>,"OriginDuration":<F-ORIGINDURATION>.*</F-ORIGINDURATION>,"OriginStatus":(405|404|403|402|401),"Overhead":<F-OVERHEAD>.*</F-OVERHEAD>,"RequestAddr":"<F-REQUESTADDR>.*</F-REQUESTADDR>","RequestContentSize":<F-REQUESTCONTENTSIZE>.*</F-REQUESTCONTENTSIZE>,"RequestCount":<F-REQUESTCOUNT>.*</F-REQUESTCOUNT>,"RequestHost":"<F-CONTAINER>.*</F-CONTAINER>","RequestMethod":"<F-REQUESTMETHOD>.*</F-REQUESTMETHOD>","RequestPath":"<F-REQUESTPATH>.*</F-REQUESTPATH>","RequestPort":"<F-REQUESTPORT>.*</F-REQUESTPORT>","RequestProtocol":"<F-REQUESTPROTOCOL>.*</F-REQUESTPROTOCOL>","RequestScheme":"<F-REQUESTSCHEME>.*</F-REQUESTSCHEME>","RetryAttempts":<F-RETRYATTEMPTS>.*</F-RETRYATTEMPTS>,.*"StartLocal":"<F-STARTLOCAL>.*</F-STARTLOCAL>","StartUTC":"<F-STARTUTC>.*</F-STARTUTC>","TLSCipher":"<F-TLSCIPHER>.*</F-TLSCIPHER>","TLSVersion":"<F-TLSVERSION>.*</F-TLSVERSION>","entryPointName":"<F-ENTRYPOINTNAME>.*</F-ENTRYPOINTNAME>","level":"<F-LEVEL>.*</F-LEVEL>","msg":"<F-MSG>.*</F-MSG>",("request_User-Agent":"<F-USERAGENT>.*</F-USERAGENT>",){0,1}?"time":"<F-TIME>.*</F-TIME>"}$

      # custom date pattern for traefik JSON access logs
      # based on https://github.com/fail2ban/fail2ban/issues/2558#issuecomment-546738270
      datepattern = "StartLocal"\s*:\s*"%%Y-%%m-%%d[T]%%H:%%M:%%S\.%%f\d*(%%z)?",

      # ignore common errors like missing media files or JS/CSS/TXT/ICO stuff
      ignoreregex = ^{"ClientAddr":"<F-CLIENTADDR>.*</F-CLIENTADDR>","ClientHost":"<HOST>","ClientPort":"<F-CLIENTPORT>.*</F-CLIENTPORT>","ClientUsername":"<F-CLIENTUSERNAME>.*</F-CLIENTUSERNAME>","DownstreamContentSize":<F-DOWNSTREAMCONTENTSIZE>.*</F-DOWNSTREAMCONTENTSIZE>,"DownstreamStatus":<F-DOWNSTREAMSTATUS>.*</F-DOWNSTREAMSTATUS>,"Duration":<F-DURATION>.*</F-DURATION>,"OriginContentSize":<F-ORIGINCONTENTSIZE>.*</F-ORIGINCONTENTSIZE>,"OriginDuration":<F-ORIGINDURATION>.*</F-ORIGINDURATION>,"OriginStatus":(405|404|403|402|401),"Overhead":<F-OVERHEAD>.*</F-OVERHEAD>,"RequestAddr":"<F-REQUESTADDR>.*</F-REQUESTADDR>","RequestContentSize":<F-REQUESTCONTENTSIZE>.*</F-REQUESTCONTENTSIZE>,"RequestCount":<F-REQUESTCOUNT>.*</F-REQUESTCOUNT>,"RequestHost":"<F-REQUESTHOST>.*</F-REQUESTHOST>","RequestMethod":"<F-REQUESTMETHOD>.*</F-REQUESTMETHOD>","RequestPath":"<F-REQUESTPATH>.*(\.png|\.txt|\.jpg|\.ico|\.js|\.css|\.ttf|\.woff|\.woff2)(/)*?</F-REQUESTPATH>","RequestPort":"<F-REQUESTPORT>.*</F-REQUESTPORT>","RequestProtocol":"<F-REQUESTPROTOCOL>.*</F-REQUESTPROTOCOL>","RequestScheme":"<F-REQUESTSCHEME>.*</F-REQUESTSCHEME>","RetryAttempts":<F-RETRYATTEMPTS>.*</F-RETRYATTEMPTS>,.*"StartLocal":"<F-STARTLOCAL>.*</F-STARTLOCAL>","StartUTC":"<F-STARTUTC>.*</F-STARTUTC>","TLSCipher":"<F-TLSCIPHER>.*</F-TLSCIPHER>","TLSVersion":"<F-TLSVERSION>.*</F-TLSVERSION>","entryPointName":"<F-ENTRYPOINTNAME>.*</F-ENTRYPOINTNAME>","level":"<F-LEVEL>.*</F-LEVEL>","msg":"<F-MSG>.*</F-MSG>",("request_User-Agent":"<F-USERAGENT>.*</F-USERAGENT>",){0,1}?"time":"<F-TIME>.*</F-TIME>"}$
    '';
  };

  networking.wireguard.enable = true;
  networking.wireguard.interfaces = {
    wg0 = {
      ips = [ "192.168.7.1/24" ];
      listenPort = 51821;
      privateKeyFile = config.sops.secrets.homelab-wireguard-private-key.path;

      postSetup = ''
        WAN_IFACE="eth0"
        DESKTOP_PC="192.168.1.67"

        ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -d $DESKTOP_PC -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -o $WAN_IFACE -d $DESKTOP_PC -j MASQUERADE
      '';

      postShutdown = ''
        WAN_IFACE="eth0"
        DESKTOP_PC="192.168.1.50"

        ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -d $DESKTOP_PC -j ACCEPT
        ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -o $WAN_IFACE -d $DESKTOP_PC -j MASQUERADE
      '';
      peers = [
        {
          # Laptop xps 9510
          publicKey = "3i/aGGX9iOSyQ/FLbrKyvaClHzqUq3mGHX4oneerbm0=";
          allowedIPs = [ "192.168.7.2/32" ];
        }
        {
          # Laptop x1 nano
          publicKey = "zzO+Pxb/p+Yzw5eGDfOcg4axjm91vRtohUZ6o2talzY=";
          allowedIPs = [ "192.168.7.3/32" ];
        }
        {
          # Samsung S24
          publicKey = "+Z/mss41U9u0R7ss5GqAmL+PICUd0Wu8yb5/MPDaiW8=";
          allowedIPs = [ "192.168.7.4/32" ];
        }
      ];
    };
  };
}
