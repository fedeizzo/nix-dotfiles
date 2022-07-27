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
    hostName = "rasp-nixos"; # Define your hostname.
    networkmanager = {
      enable = true;
    };
    # defaultGateway = "192.168.119.255";
    # nameservers = [ "8.8.8.8" ];
    # interfaces.eth0.ipv4.addresses = [{
    #   address = "192.168.118.0";
    #   prefixLength = 22;
    # }];
  };
  # services.hostapd = {
  #   enable = true;
  #   ssid = "Rete1220J";
  #   interface = "wlp1s0u1u3";
  #   # wpaPassphrase = "";
  # };
  # services.dnsmasq = {
  #   enable = true;
  #   servers = [
  #     "8.8.8.8"
  #     "8.8.4.4"
  #   ];
  #   extraConfig = ''
  #     interface=wlp1s0u1u3
  #     listen-address = 192.168.5.1
  #     bind-interfaces
  #     domain-needed
  #     bogus-priv
  #     dhcp-range=192.168.5.50,192.168.5.150,12h
  #   '';
  # };
  # boot.kernel.sysctl = {
  #   net.ipv4.ip_forward = 1;
  # };


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
  services.xserver = {
    enable = true;

    desktopManager = {
      xterm.enable = false;
    };

    displayManager = {
      startx.enable = false;
      defaultSession = "none+i3";
    };

    windowManager.i3 = {
      enable = true;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3lock #default i3 screen locker
      ];
    };
  };

  # SECURITY
  security.doas = {
    enable = true;
    extraRules = [
      { groups = [ "wheel" ]; keepEnv = true; persist = true; }
    ];
  };

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

