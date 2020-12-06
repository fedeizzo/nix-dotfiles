{ config, pkgs, ... }:

{
  # TODO see postBootCommand to do snapshots or instead create a systemd service
  # TODO see container in order to run copy of nixOs for steam and other pkg
  # TODO see console in order to change ctrl with caps on laptop keyboard
  imports = [ ./hardware-configuration.nix ];

  #################################
  # BOOT
  #################################
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  #################################
  # KEYMAP and TIME and FONT
  #################################
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    keyMap = "us";
  };
  time.timeZone = "Europe/Rome";
  # TODO find a way to run fc-cache -rv to reload font cache (root and user)
  fonts = {
    fonts = [
      pkgs.fira-code
      pkgs.font-awesome
      pkgs.joypixels
      pkgs.symbola
      (pkgs.callPackage ./pkgs/gfonts.nix { })
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Nimbus Mono PS" ];
        sansSerif = [ "Nimbus Sans" ];
        serif = [ "Nimbus Roman" ];
      };
    };
  };

  #################################
  # NIX/NIXOS
  #################################
  nixpkgs.config = {
    allowUnfree = true;
  };
  system.stateVersion = "20.09";
  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  #################################
  # PROGRAMS and ENV
  #################################
  environment.systemPackages = with pkgs; [
    # BSPWM SETUP
    # bspwm
    # xdo
    # sxhkd
    # XORG
    bc
    curl
    dunst
    haskellPackages.xmobar
    killall
    libnotify
    neofetch
    picom
    unclutter
    wget
    xclip
    xorg.xbacklight
    xsel
    # OTHER
    acpi
    arandr
    docker
    docker-compose
    podman
    podman-compose
    git
    highlight
    lm_sensors
    vim
  ];
  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = true;
    };
    podman = {
      enable = true;
    };
  };
  environment.shells = [ pkgs.bash pkgs.zsh ];
  environment.pathsToLink = [ "/share/zsh" ];
  environment.variables = {
    "EDITOR" = "nvim";
    "READER" = "zathura";
    "VISUAL" = "nvim";
    "CODEEDITOR" = "nvim";
    "TERMINAL" = "alacritty";
    "BROWSER" = "qutebrowser";
    "COLORTERM" = "truecolor";
  };
  environment.shellAliases = {
    # cp optimized for btrfs
    "cp" = "cp --reflink=auto -i";
    # some useful aliases
    "grep" = "grep --color=auto";
    "ip" = "ip -c ";
    ":q" = "exit";
    "mv" = "mv -i";
    "open" = "xdg-open";
    # editor aliases
    "v" = "nvim";
    "SS" = "systemctl";
  };
  programs.bash = {
    enableCompletion = true;
    enableLsColors = true;
  };
  programs.autojump.enable = true;
  programs.light.enable = true;
  programs.ssh.askPassword = "";
  programs.ccache.enable = true;
  programs.gnupg.agent.enable = true;
  # TODO add this two line when problem will be fixed 
  # virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = [ "fedeizzo" ];

  #################################
  # HARDWARE
  #################################
  # TODO maybe useful for Italian accent
  # services.xserver.xkbOptions = "eurosign:e";
  # TODO this 32bit support is useful only with steam
  # hardware.opengl.driSupport32Bit = true;
  # hardware.pulseaudio.support32Bit = true;
  powerManagement.enable = true;
  hardware.opengl = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.vaapiIntel
      pkgs.vaapiVdpau
      pkgs.libvdpau-va-gl
    ];
  };
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
  };
  hardware.cpu.intel.updateMicrocode = true;
  hardware.pulseaudio = {
    enable = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    extraConfig = "load-module module-bluetooth-discover a2dp_config=\"ldac_eqmid=sq\"\n";
    package = pkgs.pulseaudioFull;
  };
  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  #################################
  # NETWORKING
  #################################
  networking.hostName = "fedeizzo-nixos";
  networking.networkmanager.enable = true;
  networking.useDHCP = false;

  #################################
  # SECURITY
  #################################
  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };

  #################################
  # SECURITY
  #################################
  services.xserver = {
    enable = true;
    autorun = true;
    desktopManager.default = null;
    displayManager.lightdm = {
      enable = true;
    };
    layout = "us";
    xkbVariant = "altgr-intl";
    libinput.enable = true;
    extraConfig = ''
      Section "InputClass"
        Identifier "touchpad"
        Driver "libinput"
        MatchIsTouchpad "on"
        Option "NaturalScrolling" "true"
      EndSection
    '';
    videoDrivers = [ "intel" ];

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
  services.tlp = {
    enable = true;
    settings = {
      TLP_ENABLE = 1;
      TLP_DEFAULT_MODE = "AC";
      WIFI_PWR_ON_AC = "off";
      WIFI_PWR_ON_BAT = "on";
      CPU_HWP_ON_AC = "performance";
      CPU_HWP_ON_BAT = "balance-performance";
      DEVICES_TO_ENABLE_ON_STARTUP = "bluetooth wifi";
    };
  };
  services.thermald = {
    enable = true;
  };
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };
  services.postgresql.enable = true;
}
