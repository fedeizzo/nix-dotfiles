{ config, pkgs, lib, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    tmpOnTmpfs = true;
    cleanTmpDir = true;
    # ttyAMA0 is the serial console broken out to the GPIO
    kernelParams = [
      "8250.nr_uarts=1"
      "console=ttyAMA0,115200"
      "console=tty1"
      # A lot GUI programs need this, nearly all wayland applications
      "cma=128M"
    ];
    loader = {
      raspberryPi.firmwareConfig = "dtparam=sd_poll_once=on";
      grub.enable = false;
      systemd-boot.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  # Required for the Wireless firmware
  hardware.enableRedistributableFirmware = true;

  # NETWORKING
  networking = {
    hostName = "rasp-nixos";
    networkmanager = {
      enable = true;
    };
    interfaces.eth0.ipv4.addresses = [{
      address = "192.168.1.31";
      prefixLength = 24;
    }];
    nat = {
      enable = true;
      externalInterfaces = "wlp1s0u1u3";
      internalInterfaces = [ "wg0" ];
    };
    firewall = {
      enable = true;
      trustedInterfaces = [ "wg0" ];
      allowedUDPPorts = [ 51820 ];
    };
    wireguard = {
      enable = true;
      interfaces = {
        wg0 = {
          ips = [ "192.168.1.31" ];

          listenPort = 51820;

          postSetup = ''
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
          '';

          postShutdown = ''
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
          '';

          # Path to the private key file.
          #
          # Note: The private key can also be included inline via the privateKey option,
          # but this makes the private key world-readable; thus, using privateKeyFile is
          # recommended.
          # privateKeyFile = "path to private key file";
          privateKey = "TODO";

          peers = [
            # {
            #   # John Doe
            #   publicKey = "{john doe's public key}";
            #   # visible other machine in the private network
            #   allowedIPs = [ "10.100.0.3/32" ];
            # }
          ];
        };
      };
    };

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      allowSFTP = false; # Don't set this if you need sftp
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
      openFirewall = false;
      forxardX11 = false;
      permitRootLogin = "no";
    };
    services.fail2ban.enable = true;

    # KEYMAP AND TIME
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      keyMap = "us";
    };
    time.timeZone = "Europe/Rome";
    time.hardwareClockInLocalTime = true;

    # PACKAGES
    environment.systemPackages = with pkgs; [
      raspberrypifw
      bc
      curl
      killall
      wget
      git
      vim
      docker-compose
      dnsmasq
      hostapd
      firefox
    ];
    virtualisation = {
      docker = {
        enable = true;
        enableOnBoot = true;
        enableNvidia = false;
      };
    };
    programs.bash = {
      enableCompletion = true;
      enableLsColors = true;
    };

    # i3
    # services.xserver = {
    #   enable = true;

    #   desktopManager = {
    #     xterm.enable = false;
    #   };

    #   displayManager = {
    #     startx.enable = false;
    #     defaultSession = "none+i3";
    #   };

    #   windowManager.i3 = {
    #     enable = true;
    #     extraPackages = with pkgs; [
    #       dmenu #application launcher most people use
    #       i3status # gives you the default i3 status bar
    #       i3lock #default i3 screen locker
    #     ];
    #   };
    # };

    # SECURITY
    security.sudo.enable = false;
    security.doas = {
      enable = true;
      extraRules = [
        { groups = [ "wheel" ]; keepEnv = true; persist = true; }
      ];
    };
    # Show log with journactl -f
    security.auditd.enable = true;
    security.audit.enable = true;
    security.audit.rules = [
      "-a exit,always -F arch=b64 -S execve"
    ];

    # USER
    users.users.rasp = {
      name = "rasp";
      isNormalUser = true;
      createHome = true;
      extraGroups = [
        "wheel"
        "docker"
        "autologin"
        "users"
        "networkmanager"
      ];
      shell = pkgs.bash;
    };

    # NIX STUFF
    nixpkgs.config = {
      allowUnfree = true;
    };
    nix = {
      autoOptimiseStore = true;
      package = pkgs.nixFlakes;
      gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };
      # Free up to 1GiB whenever there is less than 100MiB left.
      extraOptions = ''
          min-free = ${toString (100 * 1024 * 1024)}
        max-free = ${toString (1024 * 1024 * 1024)}
        experimental-features = nix-command flakes
      '';
    };
    system.stateVersion = "22.05";
  }

