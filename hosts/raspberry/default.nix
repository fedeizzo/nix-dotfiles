{ config
, inputs
, username
, hostname
, syncthing
, pkgs
, ...
}:

{
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    inputs.sops-nix.nixosModules.sops
    ./hardware-configuration.nix
    ./containers
    ../common/syncthing.nix
  ];
  fiCluster.services = {
    cert-manager.enable = false;
    cert-manager.applicationOrder = 1;
    traefik.enable = false;
    traefik.applicationOrder = 2;
    authelia.enable = false;
    authelia.applicationOrder = 3;
    cloudflare-ddns.enable = false;
    cloudflare-ddns.applicationOrder = 4;
    homer.enable = false;
    homer.applicationOrder = 5;
    fedeizzodev.enable = false;
    fedeizzodev.applicationOrder = 6;
    pihole.enable = false;
    pihole.applicationOrder = 7;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_rpi4;
    tmp.useTmpfs = true;
    tmp.cleanOnBoot = true;
    # ttyAMA0 is the serial console broken out to the GPIO
    kernelParams = [
      "8250.nr_uarts=1"
      "console=ttyAMA0,115200"
      "console=tty1"
      # A lot GUI programs need this, nearly all wayland applications
      "cma=128M"
      "cgroup_memory=1"
      "cgroup_enable=memory"
    ];
    loader = {
      raspberryPi = {
        enable = true;
        uboot.enable = false;
        version = 4;
        firmwareConfig = ''
          dtparam=sd_poll_once=on
          dtoverlay=gpio-fan,gpiopin=14,temp=60000
        '';
      };
      grub.enable = false;
      systemd-boot.enable = false;
      generic-extlinux-compatible.enable = false;
    };
    kernel.sysctl."net.ipv4.ip_forward" = 1;
    kernel.sysctl."net.ipv6.conf.all.forwarding" = 1;
  };

  # Required for the Wireless firmware
  hardware.enableRedistributableFirmware = true;

  # NETWORKING
  networking = {
    hostName = "${hostname}";
    networkmanager = {
      enable = true;
    };
    useDHCP = false;
    defaultGateway = "192.168.1.1";
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

  sops.secrets.homelab-wireguard-private-key.sopsFile = ../../secrets.yaml;
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
  # tailscale up
  # ip link show tailscale0
  # jorntalctl -fu tailscale
  services.tailscale = {
    enable = true;
    package = pkgs.tailscalewithnginx;
    port = 51820;
  };
  services.restic.backups = {
    backblaze = {
      user = "root";
      initialize = true;
      environmentFile = "/root/.restic_backup_env";
      repositoryFile = config.sops.secrets.restic-repository.path;
      passwordFile = config.sops.secrets.restic-password.path;
      paths = [
        "/var/volumes"
        "/var/container_envs"
        "/var/lib/sops"
        "/borgbackups"
      ];
      pruneOpts = [
        "--keep-daily 1"
        "--keep-weekly 2"
        "--keep-monthly 2"
        "--keep-yearly 2"
      ];
    };
  };
  services.k3s = {
    enable = true;
    role = "server";
    extraFlags = "--disable traefik";
  };
  systemd.services.k3s = {
    wants = [ "containerd.service" ];
    after = [ "containerd.service" ];
  };
  # add gpio group
  users.groups.gpio = { };

  # udev rule for gpio
  services.udev.extraRules = ''
    SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
    SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
    SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
  '';
  systemd.services = {
    fan-control = {
      enable = true;
      script = ''
        while true; do
          ontemp=55
          temp=$(${pkgs.libraspberrypi}/bin/vcgencmd measure_temp | egrep -o '[0-9]*\.[0-9]*')
          temp0=$${temp%.*}

          if [[ $temp > $ontemp ]]; then
              ${pkgs.libgpiod}/bin/gpioset gpiochip0 14=1
          else
              ${pkgs.libgpiod}/bin/gpioset gpiochip0 14=0

          fi
          sleep 10
        done
      '';
      unitConfig = {
        Type = "simple";
      };
      wantedBy = [ "multi-user.target" ];
    };
    # tailscale-nginx-auth = {
    #   description = "Tailscale NGINX Authentication service";
    #   after = [ "nginx.service" ];
    #   wants = [ "nginx.service" ];
    #   serviceConfig = {
    #     ExecStart = "${pkgs.tailscalewithnginx}/bin/nginx-auth";
    #     DynamicUser = "yes";
    #   };
    #   wantedBy = [ "default.target" ];
    # };
  };
  # systemd.sockets = {
  #   tailscale-nginx-auth = {
  #     description = "Tailscale NGINX Authentication socket";
  #     partOf = [ "tailscale-nginx-auth.service" ];
  #     wantedBy = [ "sockets.target" ];
  #     listenStreams = [ "/var/run/tailscale/tailscale.nginx-auth.sock" ];
  #   };
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
    tailscale
    raspberrypifw
    bc
    curl
    killall
    wget
    git
    vim
    # docker-compose
    dnsmasq
    hostapd
    firefox
    raspberrypi-eeprom
    libraspberrypi
    libgpiod
    borgbackup
    kubectl
    kubernetes-helm
    htop
  ];
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
      enableNvidia = false;
    };
    podman.enable = false;
  };
  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };

  environment = {
    shellAliases = {
      "k3sapply" = "find /etc/homelab-kubernetes -type l -name '*apply*' | sort | xargs -I sub k3s kubectl apply -f sub";
      "k3sdelete" = "find /etc/homelab-kubernetes -type l -name '*delete*' | sort -r | xargs -I sub k3s kubectl delete -f sub";
      "update_docker_images" = ''docker images --format "{{.Repository}}:{{.Tag}}" | xargs -L1 docker pull'';
      "restart_docker_containers" = "systemctl restart docker-*.service";
    };
    shellInit = ''
      export KUBECONFIG=/etc/rancher/k3s/k3s.yaml
    '';
  };

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
  sops = {
    defaultSopsFile = ../../secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];
  };
  sops.secrets.syncthing-private-key = {
    owner = syncthing.user;
    sopsFile = ../../secrets/raspberry-secrets.yaml;
    format = "yaml";
    path = "${syncthing.dataDir}/.config/syncthing/key.pem";
  };
  sops.secrets.syncthing-public-key = {
    owner = syncthing.user;
    mode = "0644";
    sopsFile = ../../secrets/raspberry-secrets.yaml;
    format = "yaml";
    path = "${syncthing.dataDir}/.config/syncthing/cert.pem";
  };
  sops.secrets.restic-repository = {
    sopsFile = ../../secrets/raspberry-secrets.yaml;
    format = "yaml";
  };
  sops.secrets.restic-password = {
    sopsFile = ../../secrets/raspberry-secrets.yaml;
    format = "yaml";
  };

  # USER
  sops.secrets.laptop-ssh-public-key = {
    sopsFile = ../../secrets.yaml;
  };
  users.users = {
    root = {
      openssh.authorizedKeys.keyFiles = [
        config.sops.secrets.laptop-ssh-public-key.path
      ];
    };
    sync = {
      name = "${syncthing.user}";
      isNormalUser = true;
      createHome = true;
      extraGroups = [ "users" ];
    };
  };

  # NIX STUFF
  documentation.nixos.enable = false;
  nixpkgs.config = {
    allowUnfree = true;
  };
  nix = {
    settings.trusted-users = [ "root" ];
    settings.auto-optimise-store = true;
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
  system.stateVersion = "23.05";
}
