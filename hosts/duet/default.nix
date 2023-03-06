{ config
, username
, syncthing
, hostname
, inputs
, lib
, pkgs
, nixpkgs-unstable
, ...
}:

let
  firwmareAdditions = pkgs.runCommand "lenovo-krane-firmware"
    {
      src = pkgs.firmwareLinuxNonfree;
    } ''
    for firmware in \
      ath10k/QCA6174/hw3.0 \
      mediatek/mt8183/scp.img \
      qca/nvm_00440302.bin \
      qca/nvm_00440302_eu.bin \
      qca/nvm_00440302_i2s_eu.bin \
      qca/rampatch_00440302.bin \
    ; do
      mkdir -p "$(dirname $out/lib/firmware/$firmware)"
      cp -vrf "$src/lib/firmware/$firmware" $out/lib/firmware/$firmware
    done
  '';
in
{
  imports = [
    inputs.sops-nix.nixosModules.sops
    inputs.home-manager.nixosModules.home-manager
    (import "${inputs.mobile-nixos}/lib/configuration.nix" { device = "lenovo-krane"; })
    # ./phosh.nix
    ./kde-mobile.nix
    ./hardware-configuration.nix
    ../common/syncthing.nix
  ];

  #######################################
  # NETWORKING
  #######################################
  networking = {
    hostName = hostname;
    networkmanager.enable = true;
  };
  services.tailscale = {
    enable = true;
    port = 51820;
  };
  #######################################
  # HARDWARE
  #######################################
  hardware.firmware = [ config.mobile.device.firmware firwmareAdditions ];
  hardware.enableRedistributableFirmware = true;
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    disabledPlugins = [ "sap" ];
  };
  hardware.opengl = {
    enable = true;
    extraPackages = [
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
    ];
  };

  powerManagement.enable = true;
  zramSwap.enable = true;
  security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   socketActivation = true;
  #   audio.enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  #   media-session.enable = false;
  #   wireplumber.enable = true;
  # };
  hardware.pulseaudio.enable = lib.mkDefault true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    HandlePowerKeyLongPress=poweroff
  '';

  #######################################
  # SECURITY
  #######################################
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    allowSFTP = false; # Don't set this if you need sftp
    kbdInteractiveAuthentication = false;
    openFirewall = false;
    forwardX11 = false;
    permitRootLogin = "yes";
  };

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [
      { groups = [ "wheel" ]; keepEnv = true; persist = true; }
    ];
  };

  sops = {
    defaultSopsFile = ../../secrets.yaml;
    defaultSopsFormat = "yaml";
    age.keyFile = "/var/lib/sops/keys.txt";
    age.generateKey = false;
    age.sshKeyPaths = [ ];
  };
  sops.secrets.laptop-ssh-public-key = {
    sopsFile = ../../secrets.yaml;
    owner = config.users.users.${username}.name;
    group = config.users.users.${username}.group;
  };
  sops.secrets.nixtab-path.sopsFile = ../../secrets.yaml;
  sops.secrets.nixtab-path.neededForUsers = true;
  sops.secrets.syncthing-private-key = {
    owner = username;
    sopsFile = ../../secrets/duet-secrets.yaml;
    format = "yaml";
    path = "${syncthing.dataDir}/.config/syncthing/key.pem";
  };
  sops.secrets.syncthing-public-key = {
    owner = username;
    mode = "0644";
    sopsFile = ../../secrets/duet-secrets.yaml;
    format = "yaml";
    path = "${syncthing.dataDir}/.config/syncthing/cert.pem";
  };
  #######################################
  # USERS
  #######################################
  users.mutableUsers = false;
  users.users = {
    root = {
      hashedPassword = "!";
      openssh.authorizedKeys.keyFiles = [
        config.sops.secrets.laptop-ssh-public-key.path
      ];
    };
    "${username}" = {
      openssh.authorizedKeys.keyFiles = [
        config.sops.secrets.laptop-ssh-public-key.path
      ];
      isNormalUser = true;
      passwordFile = config.sops.secrets.nixtab-path.path;
      extraGroups = [
        "input"
        "dialout"
        "feedbackd"
        "networkmanager"
        "video"
        "wheel"
        "keys"
      ];
    };
  };

  #######################################
  # PACKAGES
  #######################################
  environment.systemPackages = with pkgs; [
    neofetch
    vim
    killall
    git
    acpi
    bluez
    bluez-alsa
    bluez-tools
    htop
    libsForQt5.plasma-settings
  ];
  # programs.sway = {
  #   enable = true;
  #   extraSessionCommands = ''
  #     export SDL_VIDEODRIVER=wayland
  #     export _JAVA_AWT_WM_NONREPARENTING=1
  #     export QT_QPA_PLATFORM=wayland-egl
  #     export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
  #     export XDG_CURRENT_DESKTOP=sway
  #     export XDG_SESSION_DESKTOP=sway
  #     export XDG_SESSION_TYPE="wayland"
  #     export GTK_USE_PORTAL=0
  #     export MOZ_ENABLE_WAYLAND=1
  #     systemctl --user import-environment
  #   '';
  #   extraPackages = [ ];
  # };
  # environment.sessionVariables = {
  #   "GTK_USE_PORTAL" = "0";
  # };
  # xdg.portal = {
  #   enable = true;
  #   wlr.enable = true;
  # };
  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.pinentryFlavor = "gnome3";
  programs.dconf.enable = true;

  #######################################
  # FONT and TIME
  #######################################
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Rome";
  time.hardwareClockInLocalTime = false;

  fonts = {
    fonts = [
      pkgs.fira-code
      pkgs.font-awesome
      nixpkgs-unstable.joypixels
      pkgs.jetbrains-mono
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "JetBrains Mono" ];
      };
    };
  };

  #######################################
  # NIXOS
  #######################################
  nixpkgs = {
    config = {
      allowUnfree = true;
      joypixels.acceptLicense = true;
      chromium.commandLineArgs = "--enable-features=UseOzonePlatform --ozone-platform=wayland";
    };
  };
  nix = {
    settings.trusted-users = [
      "root"
    ];
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
  system.stateVersion = "22.11";
}

